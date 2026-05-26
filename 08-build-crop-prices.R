# 08-build-crop-prices.R
#
# Builds a (country × GAEZ crop) USD-per-tonne price table for use with the
# zone × crop production layers, so we can express actual / gap / potential
# in USD instead of tonnes.
#
# Two price sources, in priority order:
#   1. Producer Price (USD/tonne) from FAOSTAT Prices, median 2014-2020.
#   2. Implied price = ag_value_data (Gross Production Value, constant 2014-2016
#      thousand USD, ×1000) / ag_prod_data (Production, tonnes), median 2014-2020.
#   3. Africa-wide median of implied price for the crop.
#
# Crops that combine multiple FAO items (e.g. BAN = Bananas + Plantains, OCE =
# Rye + Oats + Buckwheat + ...) are aggregated by summing value and production
# across constituent items before computing implied price; producer prices are
# averaged across the constituents.
#
# Output:
#   generated/crop_prices_usd_per_tonne.csv
#     iso_a3, crop, price_usd_per_tonne, source

library(tidyverse)
library(arrow)
library(countrycode)
library(rnaturalearth)

# ── Paths ────────────────────────────────────────────────────────────────
fao_prod_path  <- "/home/taimur/dev/shared-data/fao/ag_prod_data.parquet"
fao_value_path <- "/home/taimur/dev/shared-data/fao/ag_value_data.parquet"
fao_price_path <- "/home/taimur/dev/shared-data/fao/bulk/Prices_E_All_Data_(Normalized).csv"

PERIOD <- 2014:2020   # GAEZ baseline is 2020, prices in constant 2014-2016 USD

# ── Crosswalk and African countries ─────────────────────────────────────
xw <- read_csv("data/gaez_to_faostat_crosswalk.csv", show_col_types = FALSE)
message("Crosswalk: ", n_distinct(xw$crop), " GAEZ crops, ",
        nrow(xw), " (crop,FAO-item) rows")

africa_sf <- ne_countries(continent = "Africa", scale = "medium",
                          returnclass = "sf") |>
  sf::st_drop_geometry() |>
  filter(!is.na(iso_a3), iso_a3 != "-99") |>
  select(iso_a3, name)
africa_iso <- africa_sf$iso_a3

# ── Helper: FAOSTAT Area name/M49 → ISO3 ────────────────────────────────
to_iso3 <- function(name, m49) {
  m49n <- suppressWarnings(as.numeric(m49))
  iso  <- countrycode(m49n, origin = "un", destination = "iso3c", warn = FALSE)
  iso[is.na(iso)] <- countrycode(name[is.na(iso)], origin = "country.name",
                                 destination = "iso3c", warn = FALSE)
  iso
}

# ── 1. Producer Price (USD/tonne) ───────────────────────────────────────
message("Loading FAOSTAT producer prices...")
pp <- read_csv(fao_price_path, show_col_types = FALSE) |>
  filter(Element == "Producer Price (USD/tonne)",
         Year %in% PERIOD,
         Months == "Annual value",
         !is.na(Value)) |>
  transmute(area = Area, m49 = `Area Code (M49)`,
            fao_item_code = `Item Code`, year = Year, price_usd_per_t = Value)

pp <- pp |>
  mutate(iso_a3 = to_iso3(area, m49)) |>
  filter(iso_a3 %in% africa_iso) |>
  inner_join(xw, by = "fao_item_code")

producer_price <- pp |>
  group_by(iso_a3, crop) |>
  summarise(price = median(price_usd_per_t, na.rm = TRUE), .groups = "drop") |>
  filter(is.finite(price))

message("Producer prices: ", nrow(producer_price),
        " (country, crop) pairs covered")

# ── 2. Implied price = USD / tonne from ag_value / ag_prod ──────────────
message("Loading FAOSTAT production volumes and values...")
prod <- read_parquet(fao_prod_path) |>
  filter(Element == "Production", Unit == "t",
         Year %in% PERIOD, !is.na(Value)) |>
  transmute(area = Area, m49 = `Area Code (M49)`,
            fao_item_code = `Item Code`, year = Year, prod_t = Value)

val <- read_parquet(fao_value_path) |>
  filter(Element == "Gross Production Value (constant 2014-2016 thousand US$)",
         Year %in% PERIOD, !is.na(Value)) |>
  transmute(area = Area, m49 = `Area Code (M49)`,
            fao_item_code = `Item Code`, year = Year,
            value_usd = Value * 1000)  # 1000 USD → USD

# Restrict to crops + countries we care about
prod <- prod |> mutate(iso_a3 = to_iso3(area, m49)) |>
  filter(iso_a3 %in% africa_iso) |> inner_join(xw, by = "fao_item_code")
val  <- val  |> mutate(iso_a3 = to_iso3(area, m49)) |>
  filter(iso_a3 %in% africa_iso) |> inner_join(xw, by = "fao_item_code")

# Aggregate value & production by (country, GAEZ crop, year). For compound
# GAEZ crops (e.g. POT = Potatoes + Sweet potatoes), this sums constituents.
prod_agg <- prod |>
  group_by(iso_a3, crop, year) |>
  summarise(prod_t = sum(prod_t, na.rm = TRUE), .groups = "drop")
val_agg <- val |>
  group_by(iso_a3, crop, year) |>
  summarise(value_usd = sum(value_usd, na.rm = TRUE), .groups = "drop")

implied <- inner_join(prod_agg, val_agg, by = c("iso_a3", "crop", "year")) |>
  filter(prod_t > 0, value_usd > 0) |>
  mutate(implied_price = value_usd / prod_t) |>
  group_by(iso_a3, crop) |>
  summarise(price = median(implied_price, na.rm = TRUE), .groups = "drop") |>
  filter(is.finite(price))

message("Implied prices: ", nrow(implied),
        " (country, crop) pairs covered")

# ── 3. Africa-wide median implied price (final fallback) ────────────────
africa_med <- implied |>
  group_by(crop) |>
  summarise(price = median(price, na.rm = TRUE), .groups = "drop")

message("Africa-wide implied price covers ",
        nrow(africa_med), " of ", n_distinct(xw$crop), " crops")

# ── 4. Stack with priority: producer → implied → africa median ───────────
prices <- producer_price |> mutate(source = "producer_price") |>
  bind_rows(
    implied |> anti_join(producer_price, by = c("iso_a3", "crop")) |>
      mutate(source = "implied_value_per_prod")
  )

# Fill remaining gaps for countries with zone presence but no FAOSTAT data
all_pairs <- expand_grid(iso_a3 = africa_iso, crop = unique(xw$crop))
prices <- all_pairs |>
  left_join(prices, by = c("iso_a3", "crop")) |>
  left_join(africa_med |> rename(price_africa = price), by = "crop") |>
  mutate(
    price_usd_per_tonne = coalesce(price, price_africa),
    source = case_when(
      !is.na(price)            ~ source,
      !is.na(price_africa)     ~ "africa_median_implied",
      TRUE                     ~ NA_character_
    )
  ) |>
  select(iso_a3, crop, price_usd_per_tonne, source)

message("\nCoverage by source:")
print(prices |> count(source))

write_csv(prices, "generated/crop_prices_usd_per_tonne.csv")
message("\nWrote generated/crop_prices_usd_per_tonne.csv (",
        nrow(prices), " rows)")

# Quick sanity check
cat("\nMaize price (USD/t) by source:\n")
prices |> filter(crop == "MZE") |>
  arrange(desc(price_usd_per_tonne)) |> head(10) |> print()
cat("\nMedian price by crop:\n")
prices |> group_by(crop) |>
  summarise(median_price_usd_per_t = median(price_usd_per_tonne, na.rm = TRUE)) |>
  arrange(desc(median_price_usd_per_t)) |> print(n = 40)
