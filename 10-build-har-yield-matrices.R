# 10-build-har-yield-matrices.R
#
# Build two country × AEZ × crop matrices using GAEZ RES06-HAR (harvested
# area) and RES06-PRD (production):
#   1. Cropland-use matrix: share of zone's cropland in each crop
#   2. Yield matrix: actual yield (t/ha) per crop in each zone
#
# Restricted to the 26 individual GAEZ crops (drops aggregate codes like CER,
# RTS, OIL that double-count constituents). HAR coverage is broader than QGA —
# it includes COC, COF, CON, RUB, TEA, TOM (the tropical exports that the gap
# pipeline misses).
#
# Outputs:
#   generated/country_aez_har_long.parquet         — zone × crop, area + prod
#   generated/country_aez_area_share_wide.parquet  — zone × crop area share
#   generated/country_aez_yield_wide.parquet       — zone × crop yield (t/ha)
#   generated/country_aez_main_crops.csv           — flagged "main" crop set

library(tidyverse)
library(terra)
library(arrow)

africa_bbox <- ext(-20, 55, -37, 40)
har_dir <- "~/dev/shared-data/fao/gaez/v5/global/RES06-HAR"
prd_dir <- "~/dev/shared-data/fao/gaez/v5/global/RES06-PRD"

zone_r <- rast("generated/country_aez_zone_raster.tif")
names(zone_r) <- "zone_id"

zones     <- read_parquet("generated/country_aez_zones.parquet")
crop_dict <- read_csv("data/crop_dictionary.csv", show_col_types = FALSE)

crops <- c("BAN","BRL","COC","COF","CON","COT","CSV","GRD","MLT","MZE",
           "OCE","OLP","POT","RCW","RSD","RUB","SES","SFL","SOY","SRG",
           "SUB","SUC","TEA","TOB","TOM","WHE")

zstat <- function(r, zone_r) {
  out <- zonal(r, zone_r, fun = "sum", na.rm = TRUE) |> as_tibble()
  names(out) <- c("zone_id", "value")
  out
}

# ── Zonal area + production per crop ─────────────────────────────────────
message("Computing zonal area + production for ", length(crops), " crops...")
long <- map_dfr(crops, function(code) {
  fh <- file.path(har_dir, paste0("GAEZ-V5.RES06-HAR.", code, ".WST.tif"))
  fp <- file.path(prd_dir, paste0("GAEZ-V5.RES06-PRD.", code, ".WST.tif"))
  rh <- rast(fh) |> crop(africa_bbox)
  rp <- rast(fp) |> crop(africa_bbox)
  ha <- zstat(rh, zone_r) |> rename(area_ha = value)
  pr <- zstat(rp, zone_r) |> rename(prod_t  = value)
  ha |> left_join(pr, by = "zone_id") |> mutate(crop = code)
})

long <- long |>
  mutate(area_ha = ifelse(is.nan(area_ha), 0, area_ha),
         prod_t  = ifelse(is.nan(prod_t),  0, prod_t),
         yield_t_ha = ifelse(area_ha > 0, prod_t / area_ha, NA_real_)) |>
  left_join(crop_dict |> select(crop, caption, group), by = "crop")

write_parquet(long, "generated/country_aez_har_long.parquet")
message("Wrote zone × crop long: ", nrow(long), " rows")

# ── Sanity check: maize Africa total harvested area ──────────────────────
mze_total <- long |> filter(crop == "MZE") |> pull(area_ha) |> sum(na.rm = TRUE)
message("Africa maize harvested area (HAR sum): ",
        round(mze_total / 1e6, 2), " M ha  (FAO 2014-2020 ≈ 38 M ha)")

# ── 1. Cropland-use matrix (area share) ──────────────────────────────────
total_area_zone <- long |>
  group_by(zone_id) |>
  summarise(zone_total_area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop")

area_share <- long |>
  left_join(total_area_zone, by = "zone_id") |>
  mutate(area_share = ifelse(zone_total_area_ha > 0,
                             area_ha / zone_total_area_ha, 0))

# ── Identify "main crops" ────────────────────────────────────────────────
# A crop is "main" if it's in the top 15 by Africa-wide area, OR ever the
# dominant (top-1) crop in some zone, OR a notable export item (COC, COF,
# TEA, RUB, TOB, TOM, COT, SES) — these can be small in area but high in value.
africa_area <- area_share |>
  group_by(crop) |>
  summarise(total_area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total_area_ha))

zone_dominant <- area_share |>
  filter(area_ha > 0) |>
  group_by(zone_id) |>
  slice_max(area_share, n = 1, with_ties = FALSE) |>
  pull(crop) |> unique()

high_value_exports <- c("COC","COF","TEA","RUB","TOB","TOM","COT","SES","OLP","CON")

top_n_africa <- africa_area |> head(15) |> pull(crop)

main_crops <- union(union(top_n_africa, zone_dominant), high_value_exports)

main_tbl <- crop_dict |>
  filter(crop %in% main_crops) |>
  left_join(africa_area, by = "crop") |>
  mutate(in_top15      = crop %in% top_n_africa,
         is_zone_top1  = crop %in% zone_dominant,
         is_export_hv  = crop %in% high_value_exports) |>
  arrange(desc(total_area_ha))

write_csv(main_tbl, "generated/country_aez_main_crops.csv")
message("Main crops kept (", nrow(main_tbl), "):")
print(main_tbl, n = 30)

# ── 2. Pivot to wide matrices ────────────────────────────────────────────
area_share_wide <- area_share |>
  filter(crop %in% main_crops) |>
  select(zone_id, crop, area_share) |>
  pivot_wider(names_from = crop, values_from = area_share, values_fill = 0) |>
  left_join(zones, by = "zone_id") |>
  left_join(total_area_zone, by = "zone_id") |>
  relocate(zone_id, iso_a3, regime, regime_id, zone_total_area_ha)

yield_wide <- long |>
  filter(crop %in% main_crops) |>
  select(zone_id, crop, yield_t_ha) |>
  pivot_wider(names_from = crop, values_from = yield_t_ha, values_fill = NA_real_) |>
  left_join(zones, by = "zone_id") |>
  relocate(zone_id, iso_a3, regime, regime_id)

write_parquet(area_share_wide, "generated/country_aez_area_share_wide.parquet")
write_parquet(yield_wide,      "generated/country_aez_yield_wide.parquet")
message("Wrote area-share wide: ", nrow(area_share_wide), " zones × ",
        sum(names(area_share_wide) %in% main_crops), " crops")
message("Wrote yield wide:      ", nrow(yield_wide), " zones × ",
        sum(names(yield_wide) %in% main_crops), " crops")

# ── Quick look ───────────────────────────────────────────────────────────
cat("\nTop 5 zones by total cropped area, area shares of staples:\n")
area_share_wide |> arrange(desc(zone_total_area_ha)) |>
  select(iso_a3, regime, zone_total_area_ha,
         any_of(c("MZE","CSV","SRG","WHE","RCW","COC","COF","TOB"))) |>
  mutate(across(any_of(c("MZE","CSV","SRG","WHE","RCW","COC","COF","TOB")),
                ~ scales::percent(., 0.1)),
         zone_total_area_ha = scales::label_number(scale_cut = scales::cut_short_scale())(zone_total_area_ha)) |>
  head() |> print()

cat("\nSame zones, yield (t/ha):\n")
yield_wide |>
  inner_join(area_share_wide |> select(zone_id, zone_total_area_ha), by = "zone_id") |>
  arrange(desc(zone_total_area_ha)) |>
  select(iso_a3, regime,
         any_of(c("MZE","CSV","SRG","WHE","RCW","COC","COF","TOB"))) |>
  mutate(across(any_of(c("MZE","CSV","SRG","WHE","RCW","COC","COF","TOB")),
                ~ round(., 2))) |>
  head() |> print()

message("\nDone. Outputs:")
message("  generated/country_aez_har_long.parquet")
message("  generated/country_aez_area_share_wide.parquet")
message("  generated/country_aez_yield_wide.parquet")
message("  generated/country_aez_main_crops.csv")
