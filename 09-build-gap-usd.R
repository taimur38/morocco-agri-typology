# 09-build-gap-usd.R
#
# Express the production-gap features in USD instead of tonnes.
# Joins the (iso_a3, crop) → USD/tonne table built by 08 onto the zone × crop
# tonnage outputs from 06. Zone-level achievement is then dollar-weighted,
# treating wheat, cassava, sugarcane etc. by their actual market value.
#
# Outputs:
#   generated/country_aez_gap_long_usd.parquet
#   generated/country_aez_gap_wide_usd.parquet
#   generated/country_aez_gap_by_group_usd.parquet

library(tidyverse)
library(arrow)

long  <- read_parquet("generated/country_aez_gap_long.parquet")
zones <- read_parquet("generated/country_aez_zones.parquet")
prices <- read_csv("generated/crop_prices_usd_per_tonne.csv",
                   show_col_types = FALSE)

# Bring iso_a3 onto the zone × crop table, then attach prices
long_iso <- long |>
  left_join(zones |> select(zone_id, iso_a3), by = "zone_id") |>
  left_join(prices, by = c("iso_a3", "crop"))

n_missing <- sum(is.na(long_iso$price_usd_per_tonne))
message("Missing prices on zone-crop rows: ", n_missing,
        " of ", nrow(long_iso))

# ── 1. Long table in USD ────────────────────────────────────────────────
# NB: despite the column name `actual_1000t`, the GAEZ RES06-PRD raw pixel
# values are already in tonnes (global maize sum = 1.17 Bt matches FAO),
# so we multiply price × tonnes directly.
long_usd <- long_iso |>
  mutate(
    actual_usd    = actual_1000t    * price_usd_per_tonne,
    gap_usd       = gap_1000t       * price_usd_per_tonne,
    potential_usd = potential_1000t * price_usd_per_tonne,
    achievement_crop_usd = ifelse(potential_usd > 0,
                                  actual_usd / potential_usd, NA_real_)
  )

write_parquet(long_usd, "generated/country_aez_gap_long_usd.parquet")
message("Wrote zone × crop USD long: ", nrow(long_usd), " rows")

# ── 2. Zone rollup in USD ────────────────────────────────────────────────
wide_usd <- long_usd |>
  group_by(zone_id) |>
  summarise(
    sum_actual_usd    = sum(actual_usd,    na.rm = TRUE),
    sum_gap_usd       = sum(gap_usd,       na.rm = TRUE),
    sum_potential_usd = sum_actual_usd + sum_gap_usd,
    achievement_ratio_usd = ifelse(sum_potential_usd > 0,
                                   sum_actual_usd / sum_potential_usd,
                                   NA_real_),
    .groups = "drop"
  ) |>
  left_join(zones, by = "zone_id")

write_parquet(wide_usd, "generated/country_aez_gap_wide_usd.parquet")
message("Wrote zone USD wide: ", nrow(wide_usd), " rows")

# ── 3. Zone × group rollup in USD ────────────────────────────────────────
by_group_usd <- long_usd |>
  group_by(zone_id, group) |>
  summarise(
    sum_actual    = sum(actual_usd, na.rm = TRUE),
    sum_gap       = sum(gap_usd,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(achievement = ifelse((sum_actual + sum_gap) > 0,
                              sum_actual / (sum_actual + sum_gap),
                              NA_real_))

write_parquet(by_group_usd, "generated/country_aez_gap_by_group_usd.parquet")
message("Wrote zone × group USD: ", nrow(by_group_usd), " rows")

# ── Sanity check ─────────────────────────────────────────────────────────
cat("\nUSD-weighted achievement ratio summary:\n")
print(summary(wide_usd$achievement_ratio_usd))

cat("\nTop 10 zones by potential USD:\n")
wide_usd |> arrange(desc(sum_potential_usd)) |>
  select(iso_a3, regime, sum_actual_usd, sum_gap_usd, achievement_ratio_usd) |>
  mutate(across(c(sum_actual_usd, sum_gap_usd),
                ~ scales::label_number(scale_cut = scales::cut_short_scale())(.))) |>
  head(10) |> print()

cat("\nBottom 10 by achievement_ratio_usd (filtered to potential > 100M USD):\n")
wide_usd |> filter(sum_potential_usd > 1e8) |>
  arrange(achievement_ratio_usd) |>
  select(iso_a3, regime, sum_potential_usd, achievement_ratio_usd) |>
  mutate(sum_potential_usd = scales::label_number(
    scale_cut = scales::cut_short_scale())(sum_potential_usd)) |>
  head(10) |> print()
