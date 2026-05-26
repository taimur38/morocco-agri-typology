# 06-build-gap-features.R
#
# "Distance from potential" per country × AEZ zone.
# Uses GAEZ v5 RES07-QGA (production gap) + RES06-PRD (actual production).
# Restricted to the 20 individual crops for which RES07-QGA exists.
#
# Outputs:
#   generated/country_aez_gap_long.parquet — zone × crop, actual + gap (1000 t)
#   generated/country_aez_gap_wide.parquet — zone-level rollup:
#     sum_actual, sum_gap, sum_potential, achievement_ratio,
#     achievement_by_group (nested list-col optional; skipped for simplicity)

library(tidyverse)
library(terra)
library(arrow)

africa_bbox <- ext(-20, 55, -37, 40)

qga_dir <- "~/dev/shared-data/fao/gaez/v5/global/RES07-QGA"
prd_dir <- "~/dev/shared-data/fao/gaez/v5/global/RES06-PRD"

zone_r <- rast("generated/country_aez_zone_raster.tif")
names(zone_r) <- "zone_id"

crop_dict <- read_csv("data/crop_dictionary.csv", show_col_types = FALSE)
zones <- read_parquet("generated/country_aez_zones.parquet")

zstat <- function(r, zone_r, fun = "sum") {
  out <- zonal(r, zone_r, fun = fun, na.rm = TRUE) |> as_tibble()
  names(out) <- c("zone_id", "value")
  out
}

# ── Which crops have QGA? ────────────────────────────────────────────────
qga_files <- list.files(qga_dir, pattern = "\\.WST\\.tif$", full.names = TRUE)
qga_codes <- str_match(basename(qga_files), "RES07-QGA\\.([A-Z]+)\\.WST")[, 2]
message("QGA available for ", length(qga_codes), " crops: ",
        paste(qga_codes, collapse = ", "))

# ── Zonal sums for actual production (subset to QGA crops) ──────────────
message("Computing zonal actual production (1000 t)...")
actual_long <- map_dfr(qga_codes, function(code) {
  f <- file.path(prd_dir, paste0("GAEZ-V5.RES06-PRD.", code, ".WST.tif"))
  r <- rast(f) |> crop(africa_bbox)
  zstat(r, zone_r, "sum") |>
    rename(actual_1000t = value) |>
    mutate(crop = code)
})

# ── Zonal sums for production gap ────────────────────────────────────────
message("Computing zonal production gap (1000 t)...")
gap_long <- map2_dfr(qga_files, qga_codes, function(f, code) {
  r <- rast(f) |> crop(africa_bbox)
  zstat(r, zone_r, "sum") |>
    rename(gap_1000t = value) |>
    mutate(crop = code)
})

# ── Long table: zone × crop, actual + gap ────────────────────────────────
long <- actual_long |>
  left_join(gap_long, by = c("zone_id", "crop")) |>
  mutate(
    potential_1000t   = actual_1000t + gap_1000t,
    achievement_crop  = ifelse(potential_1000t > 0,
                               actual_1000t / potential_1000t, NA_real_)
  ) |>
  left_join(crop_dict, by = "crop")

write_parquet(long, "generated/country_aez_gap_long.parquet")
message("Wrote zone × crop gap long: ", nrow(long), " rows")

# ── Wide rollup per zone ─────────────────────────────────────────────────
# Production-weighted achievement = Σ actual / Σ (actual + gap) across QGA crops.
wide <- long |>
  group_by(zone_id) |>
  summarise(
    sum_actual_1000t    = sum(actual_1000t, na.rm = TRUE),
    sum_gap_1000t       = sum(gap_1000t, na.rm = TRUE),
    sum_potential_1000t = sum_actual_1000t + sum_gap_1000t,
    achievement_ratio   = ifelse(sum_potential_1000t > 0,
                                 sum_actual_1000t / sum_potential_1000t,
                                 NA_real_),
    .groups = "drop"
  ) |>
  left_join(zones, by = "zone_id")

write_parquet(wide, "generated/country_aez_gap_wide.parquet")
message("Wrote zone gap wide: ", nrow(wide), " rows")

# ── Per-group achievement (cereals, oilseeds, ...) ───────────────────────
by_group <- long |>
  group_by(zone_id, group) |>
  summarise(
    sum_actual    = sum(actual_1000t, na.rm = TRUE),
    sum_gap       = sum(gap_1000t,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(achievement = ifelse((sum_actual + sum_gap) > 0,
                              sum_actual / (sum_actual + sum_gap),
                              NA_real_))

write_parquet(by_group, "generated/country_aez_gap_by_group.parquet")
message("Wrote zone × group achievement: ", nrow(by_group), " rows")

message("\nDone. Outputs:")
message("  generated/country_aez_gap_long.parquet")
message("  generated/country_aez_gap_wide.parquet")
message("  generated/country_aez_gap_by_group.parquet")
