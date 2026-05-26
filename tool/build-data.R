# tool/build-data.R
#
# Builds the data files for the interactive zone-perf web tool.
#
# Outputs:
#   tool/data/zones.geojson        — simplified zone polygons + viz properties
#   tool/data/zone_details.json    — per-zone crop tables, frontier pointers,
#                                    and cluster descriptions

library(tidyverse)
library(arrow)
library(sf)
sf::sf_use_s2(FALSE)
library(terra)
library(jsonlite)
library(rnaturalearth)

# Run from the tool/ directory.

# ── Load ───────────────────────────────────────────────────────────────
zone_r   <- rast("../generated/country_aez_zone_raster.tif")
names(zone_r) <- "zone_id"

perf       <- read_csv("../generated/country_aez_zone_perf.csv",
                       show_col_types = FALSE)
clusters   <- read_csv("../generated/country_aez_zone_clusters_labeled.csv",
                       show_col_types = FALSE)
har_long   <- read_parquet("../generated/country_aez_har_long.parquet")
gap_usd    <- read_parquet("../generated/country_aez_gap_long_usd.parquet")
crops_main <- read_csv("../generated/country_aez_main_crops.csv",
                       show_col_types = FALSE) |> pull(crop)

africa_sf <- ne_countries(continent = "Africa", scale = "medium",
                          returnclass = "sf") |>
  filter(!is.na(iso_a3), iso_a3 != "-99") |>
  select(iso_a3, country_name = name_long)

country_lookup <- africa_sf |> st_drop_geometry()

# ── Per-zone × crop data with frontier pointers ────────────────────────
zc <- har_long |>
  filter(crop %in% crops_main) |>
  inner_join(clusters |> select(zone_id, iso_a3, regime, regime_id, cluster,
                                label, zone_total_area_ha),
             by = "zone_id") |>
  filter(area_ha > 0, !is.na(yield_t_ha), is.finite(yield_t_ha)) |>
  mutate(area_share = ifelse(zone_total_area_ha > 0,
                             area_ha / zone_total_area_ha, 0)) |>
  group_by(label, crop) |>
  mutate(n_obs = n()) |>
  ungroup() |>
  filter(n_obs >= 5)

cluster_crop <- zc |>
  group_by(label, crop, caption, group) |>
  summarise(
    frontier_yield_p95 = unname(quantile(yield_t_ha, 0.95, na.rm = TRUE)),
    max_yield          = max(yield_t_ha, na.rm = TRUE),
    frontier_zone_id   = zone_id[which.max(yield_t_ha)],
    frontier_iso       = iso_a3[which.max(yield_t_ha)],
    frontier_regime    = regime[which.max(yield_t_ha)],
    frontier_regime_id = regime_id[which.max(yield_t_ha)],
    .groups = "drop"
  ) |>
  left_join(country_lookup |>
              rename(frontier_iso = iso_a3,
                     frontier_country = country_name),
            by = "frontier_iso")

zc_full <- zc |>
  inner_join(cluster_crop |>
               select(label, crop, frontier_yield_p95, max_yield,
                      frontier_zone_id, frontier_iso, frontier_country,
                      frontier_regime, frontier_regime_id),
             by = c("label", "crop")) |>
  mutate(frontier_ratio = yield_t_ha / frontier_yield_p95)

# ── Per zone × crop GAEZ potential, ordered by potential USD value ────
# potential_yield_t_ha is implied from potential_1000t ÷ actual harvested
# area, since GAEZ's gap (RES07-QGA) is computed for currently-cropped land.
har_areas <- har_long |>
  group_by(zone_id, crop) |>
  summarise(area_ha_actual = sum(area_ha, na.rm = TRUE), .groups = "drop")

potential_long <- gap_usd |>
  filter(!is.na(potential_1000t), potential_1000t > 0,
         !is.na(price_usd_per_tonne)) |>
  left_join(har_areas, by = c("zone_id", "crop")) |>
  mutate(
    current_yield_t_ha   = ifelse(area_ha_actual > 0,
                                  actual_1000t    / area_ha_actual,
                                  NA_real_),
    potential_yield_t_ha = ifelse(area_ha_actual > 0,
                                  potential_1000t / area_ha_actual,
                                  NA_real_),
    achievement_crop     = pmin(actual_1000t / potential_1000t, 1)
  )

potential_by_zone_list <- potential_long |>
  arrange(zone_id, desc(potential_usd)) |>
  group_by(zone_id) |>
  slice_head(n = 10) |>
  ungroup() |>
  transmute(
    zone_id,
    crop_code              = crop,
    crop                   = caption,
    group,
    area_ha_actual         = round(area_ha_actual),
    current_yield_t_ha     = round(current_yield_t_ha,   2),
    potential_yield_t_ha   = round(potential_yield_t_ha, 2),
    price_usd_per_tonne    = round(price_usd_per_tonne,  0),
    potential_usd          = round(potential_usd),
    actual_usd             = round(actual_usd),
    gap_usd                = round(gap_usd),
    achievement_crop       = round(achievement_crop, 3)
  )

# ── Per-zone top crops list (top 8 by area) ────────────────────────────
zone_crops_list <- zc_full |>
  arrange(zone_id, desc(area_ha)) |>
  group_by(zone_id) |>
  slice_head(n = 8) |>
  ungroup() |>
  transmute(
    zone_id,
    crop_code            = crop,
    crop                 = caption,
    group,
    area_ha              = round(area_ha),
    area_share           = round(area_share, 4),
    yield_t_ha           = round(yield_t_ha, 3),
    frontier_yield_p95   = round(frontier_yield_p95, 3),
    max_yield            = round(max_yield, 3),
    frontier_ratio       = round(frontier_ratio, 3),
    frontier_zone_id,
    frontier_iso,
    frontier_country,
    frontier_regime,
    frontier_regime_id,
    is_self_frontier     = zone_id == frontier_zone_id
  )

# ── Brief cluster blurbs (hard-coded) ──────────────────────────────────
cluster_blurbs <- tribble(
  ~label,                         ~description,
  "Humid Mixed-Staple",           "Humid tropical zones with diversified staple cultivation: cassava, plantain, maize, rice, plus tree crops (cocoa, oil palm, banana). Found across the West and Central African humid belt.",
  "Maize Belt",                   "Sub-humid mixed cropping where maize is the dominant cereal. Often paired with groundnut, cassava, and tobacco in southern and eastern Africa.",
  "Sugarcane Plantation",         "Irrigated or high-rainfall cane-dominated systems, typically with strong agro-industrial integration.",
  "Wetland Rice",                 "Lowland rice systems on alluvial / hydromorphic soils — inland deltas, floodplains, and irrigated perimeters.",
  "Sahel Millet",                 "Pearl millet and sorghum dominate under short, erratic rainy seasons; cowpea and groundnut as companion crops.",
  "Sudano-Sahel Mixed",           "Transitional belt between Sahel millet and humid mixed-staple systems: sorghum, millet, maize, cotton, groundnut.",
  "Dryland Sorghum",              "Sorghum-dominated rainfed systems in semi-arid environments, often with limited diversification.",
  "Mediterranean Wheat-Barley",   "Cool-winter rainfed cereals (wheat, barley) plus olives, citrus, and pulses around the Mediterranean rim and southern Africa winter-rainfall zones."
)

# ── Zone metadata table ────────────────────────────────────────────────
zone_meta <- clusters |>
  select(zone_id, iso_a3, regime, regime_id, cluster, label,
         zone_total_area_ha) |>
  left_join(country_lookup, by = "iso_a3") |>
  left_join(perf |> select(zone_id, composite_perf, n_crops),
            by = "zone_id") |>
  left_join(cluster_blurbs, by = "label") |>
  mutate(
    zone_label = paste0(iso_a3, "·", regime_id),
    composite_perf = round(composite_perf, 4),
    zone_total_area_ha = round(zone_total_area_ha)
  )

# ── Vectorize zone raster and simplify ─────────────────────────────────
message("Vectorizing zone raster... (this can take a minute)")
zones_v <- as.polygons(zone_r, dissolve = TRUE) |> st_as_sf()
names(zones_v)[1] <- "zone_id"

message("Simplifying polygons...")
zones_simp <- zones_v |>
  st_simplify(dTolerance = 0.05, preserveTopology = TRUE) |>
  filter(!st_is_empty(geometry))

# Attach properties
zones_props <- zones_simp |>
  left_join(zone_meta |>
              select(zone_id, iso_a3, country_name, regime, regime_id,
                     label, composite_perf, zone_total_area_ha, zone_label),
            by = "zone_id") |>
  filter(!is.na(composite_perf))

# ── Write GeoJSON ──────────────────────────────────────────────────────
dir.create("data", showWarnings = FALSE, recursive = TRUE)
out_geo <- "data/zones.geojson"
if (file.exists(out_geo)) file.remove(out_geo)
st_write(zones_props, out_geo, driver = "GeoJSON",
         layer_options = c("RFC7946=YES", "WRITE_BBOX=YES",
                           "COORDINATE_PRECISION=4"))
message("Wrote ", out_geo,
        "  (", round(file.info(out_geo)$size / 1024), " KB)")

# Country boundary backdrop (one feature per country, very simplified)
africa_simp <- africa_sf |>
  st_simplify(dTolerance = 0.08, preserveTopology = TRUE)
out_ctry <- "data/africa.geojson"
if (file.exists(out_ctry)) file.remove(out_ctry)
st_write(africa_simp, out_ctry, driver = "GeoJSON",
         layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=3"))
message("Wrote ", out_ctry,
        "  (", round(file.info(out_ctry)$size / 1024), " KB)")

# ── Build per-zone details JSON ────────────────────────────────────────
group_to_named_list <- function(df) {
  splits <- df |> group_by(zone_id) |> group_split()
  setNames(
    lapply(splits, function(d) purrr::pmap(select(d, -zone_id), list)),
    vapply(splits, function(d) as.character(d$zone_id[1]), character(1))
  )
}
crops_by_zone     <- group_to_named_list(zone_crops_list)
potential_by_zone <- group_to_named_list(potential_by_zone_list)

zone_meta_keep <- zone_meta |>
  filter(zone_id %in% zones_props$zone_id) |>
  arrange(zone_id)

zone_details_list <- setNames(
  lapply(seq_len(nrow(zone_meta_keep)), function(i) {
    row <- as.list(zone_meta_keep[i, ])
    key <- as.character(row$zone_id)
    row$crops            <- crops_by_zone[[key]]     %||% list()
    row$potential_crops  <- potential_by_zone[[key]] %||% list()
    row
  }),
  as.character(zone_meta_keep$zone_id)
)

write_json(zone_details_list,
           "data/zone_details.json",
           auto_unbox = TRUE, na = "null", null = "null")
message("Wrote data/zone_details.json  (",
        round(file.info("data/zone_details.json")$size / 1024), " KB)")

# Across-zone median composite_perf for the map midpoint anchor
midpoint <- median(perf$composite_perf, na.rm = TRUE) |> round(4)
write_json(list(
  midpoint = midpoint,
  n_zones  = nrow(zones_props),
  perf_min = min(zone_meta$composite_perf, na.rm = TRUE) |> round(4),
  perf_max = max(zone_meta$composite_perf, na.rm = TRUE) |> round(4)
), "data/meta.json", auto_unbox = TRUE)

message("Done.")
