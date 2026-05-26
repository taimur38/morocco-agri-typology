# 04-build-country-aez-features.R
#
# Aggregates GAEZ v5 pixel layers to country × AEZ-regime zones for Africa.
# The AEZ dimension uses the 13 thermal/special groups defined in script 02.
#
# Outputs:
#   generated/country_aez_features_wide.parquet  — one row per zone, scalar features
#     (zone_area_ha, cropland_area_ha, cropland_share, cropland_irr_share,
#      soil_quality_{him,lim,mean}, val_total_intl_usd, val_rainfed, val_irrigated,
#      irr_share_val, val_per_ha_land, val_per_ha_cropland)
#   generated/country_aez_crop_production.parquet — zone × crop, production (1000 t)
#     joined with crop_dictionary (caption, crop_type = individual|aggregate, group)
#   generated/country_aez_zones.parquet          — zone_id ↔ country/regime lookup

library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(arrow)

africa_bbox <- ext(-20, 55, -37, 40)

# ── Paths ────────────────────────────────────────────────────────────────
aez_tif    <- "~/dev/shared-data/fao/gaez/v5/global/AEZ57/GAEZ-V5.AEZ57.tif"
lcc_dir    <- "~/dev/shared-data/fao/gaez/v5/global/LR-LCC"
sqx_dir    <- "~/dev/shared-data/fao/gaez/v5/global/SQX"
val_dir    <- "~/dev/shared-data/fao/gaez/v5/global/RES06-VAL"
prd_dir    <- "~/dev/shared-data/fao/gaez/v5/global/RES06-PRD"

# ── 1. Country boundaries → 10km raster ──────────────────────────────────
africa_sf <- ne_countries(continent = "Africa", scale = "medium",
                          returnclass = "sf") |>
  select(iso_a3, name, geometry) |>
  filter(!is.na(iso_a3), iso_a3 != "-99") |>
  mutate(country_id = row_number())

message("African countries loaded: ", nrow(africa_sf))

# 10km master grid, aligned to GAEZ RES06 grid
template_10km <- rast(val_ <- file.path(val_dir, "GAEZ-V5.RES06-VAL.ALL.WST.tif")) |>
  crop(africa_bbox)
values(template_10km) <- NA

country_r <- rasterize(vect(africa_sf), template_10km, field = "country_id")

# ── 2. AEZ regime raster ─────────────────────────────────────────────────
regime_levels <- c("Tropics lowland", "Tropics highland",
                   "Sub-tropics warm", "Sub-tropics mod. cool",
                   "Sub-tropics cool", "Temperate moderate",
                   "Temperate cool", "Cold (no permafrost)",
                   "Severe terrain/soil", "Irrigated / hydromorphic",
                   "Desert/Arid", "Boreal/Arctic", "Built-up / water")

regime_map <- tibble(
  value = 0:57,
  regime_id = dplyr::case_when(
    value == 0        ~ NA_integer_,
    value %in% 1:6    ~ 1L,
    value %in% 7:12   ~ 2L,
    value %in% 13:18  ~ 3L,
    value %in% 19:24  ~ 4L,
    value %in% 25:30  ~ 5L,
    value %in% 31:36  ~ 6L,
    value %in% 37:42  ~ 7L,
    value %in% 43:48  ~ 8L,
    value %in% 49:50  ~ 9L,
    value %in% 51:52  ~ 10L,
    value == 53       ~ 11L,
    value %in% 54:55  ~ 12L,
    value %in% 56:57  ~ 13L
  )
)

message("Aggregating AEZ57 1km → 10km (modal)...")
aez1km  <- rast(aez_tif) |> crop(africa_bbox)
aez10km <- aggregate(aez1km, fact = 10, fun = "modal", na.rm = TRUE)

# Reclass raw AEZ values → regime_id (2-col rcl = value-by-value substitution)
rcl <- regime_map |> filter(!is.na(regime_id)) |>
  transmute(from = value, becomes = regime_id) |>
  as.matrix()
regime_r <- classify(aez10km, rcl = rcl, others = NA)
regime_r <- resample(regime_r, template_10km, method = "near")
stopifnot(max(values(regime_r), na.rm = TRUE) <= 13)

# ── 3. Zone raster: country_id * 100 + regime_id ─────────────────────────
zone_r <- country_r * 100L + regime_r
names(zone_r) <- "zone_id"

# Persist the zone raster so downstream scripts can map features without rebuilding.
writeRaster(zone_r, "generated/country_aez_zone_raster.tif",
            overwrite = TRUE, datatype = "INT4S")

# ── Zone lookup table ────────────────────────────────────────────────────
zone_ids_present <- as.data.frame(zone_r, xy = FALSE, na.rm = TRUE) |>
  as_tibble() |>
  distinct(zone_id) |>
  arrange(zone_id)

zones <- zone_ids_present |>
  mutate(country_id = zone_id %/% 100L,
         regime_id  = zone_id %%  100L) |>
  left_join(st_drop_geometry(africa_sf), by = "country_id") |>
  mutate(regime = regime_levels[regime_id])

write_parquet(zones, "generated/country_aez_zones.parquet")
message("Zones: ", nrow(zones), " non-empty country × regime cells")

# ── 4. Zonal helper ──────────────────────────────────────────────────────
# terra::zonal returns a data.frame whose first column is zone_id and second
# is the stat. Wrap to tibble with consistent names.
zstat <- function(r, zone_r, fun = "mean") {
  out <- zonal(r, zone_r, fun = fun, na.rm = TRUE) |> as_tibble()
  names(out) <- c("zone_id", "value")
  out
}

# ── 5. Area per zone (hectares, via terra::cellSize) ────────────────────
# cellSize gives the true surface area of each pixel in the chosen unit,
# correcting for latitude — important for Africa spanning ~37°S to ~37°N.
area_ha_r <- cellSize(template_10km, unit = "ha")
area_df <- zstat(area_ha_r, zone_r, fun = "sum") |>
  rename(zone_area_ha = value) |>
  left_join(
    as.data.frame(zone_r, xy = FALSE, na.rm = TRUE) |>
      as_tibble() |>
      count(zone_id, name = "n_pixels_10km"),
    by = "zone_id"
  )

# ── 6. LR-LCC shares (cropland = LC02, irrigated cropland = LC12) ────────
message("Computing LR-LCC shares (cropland, irrigated cropland)...")
lcc_crop <- rast(file.path(lcc_dir, "GAEZ-V5.LR-LCC.LC02.tif")) |>
  crop(africa_bbox) |> aggregate(fact = 10, fun = "mean", na.rm = TRUE) |>
  resample(template_10km, method = "bilinear")
lcc_irr  <- rast(file.path(lcc_dir, "GAEZ-V5.LR-LCC.LC12.tif")) |>
  crop(africa_bbox) |> aggregate(fact = 10, fun = "mean", na.rm = TRUE) |>
  resample(template_10km, method = "bilinear")

z_crop <- zstat(lcc_crop, zone_r, "mean") |> rename(cropland_share = value)
z_irr  <- zstat(lcc_irr,  zone_r, "mean") |> rename(cropland_irr_share = value)

# ── 7. Soil quality (SQX SQ0 — overall index, HIM + LIM variants) ───────
# HIM = High Input Management assumption, LIM = Low Input Management.
# They produce different soil-quality indices; save both + a simple mean.
message("Computing SQX mean soil quality (HIM + LIM)...")
sqx_him <- rast(file.path(sqx_dir, "GAEZ-V5.SQX.SQ0.HIM.tif")) |>
  crop(africa_bbox) |> aggregate(fact = 10, fun = "mean", na.rm = TRUE) |>
  resample(template_10km, method = "bilinear")
sqx_lim <- rast(file.path(sqx_dir, "GAEZ-V5.SQX.SQ0.LIM.tif")) |>
  crop(africa_bbox) |> aggregate(fact = 10, fun = "mean", na.rm = TRUE) |>
  resample(template_10km, method = "bilinear")
z_sqx_him <- zstat(sqx_him, zone_r, "mean") |> rename(soil_quality_him = value)
z_sqx_lim <- zstat(sqx_lim, zone_r, "mean") |> rename(soil_quality_lim = value)

# ── 8. Production value (total, rainfed, irrigated) ─────────────────────
message("Computing production value sums...")
val_tot <- rast(file.path(val_dir, "GAEZ-V5.RES06-VAL.ALL.WST.tif")) |>
  crop(africa_bbox)
val_wsr <- rast(file.path(val_dir, "GAEZ-V5.RES06-VAL.ALL.WSR.tif")) |>
  crop(africa_bbox)
val_wsi <- rast(file.path(val_dir, "GAEZ-V5.RES06-VAL.ALL.WSI.tif")) |>
  crop(africa_bbox)

z_val_tot <- zstat(val_tot, zone_r, "sum") |> rename(val_total_intl_usd = value)
z_val_wsr <- zstat(val_wsr, zone_r, "sum") |> rename(val_rainfed    = value)
z_val_wsi <- zstat(val_wsi, zone_r, "sum") |> rename(val_irrigated  = value)

# ── 9. Assemble wide feature table ──────────────────────────────────────
wide <- zones |>
  left_join(area_df,    by = "zone_id") |>
  left_join(z_crop,     by = "zone_id") |>
  left_join(z_irr,      by = "zone_id") |>
  left_join(z_sqx_him,  by = "zone_id") |>
  left_join(z_sqx_lim,  by = "zone_id") |>
  left_join(z_val_tot,  by = "zone_id") |>
  left_join(z_val_wsr,  by = "zone_id") |>
  left_join(z_val_wsi,  by = "zone_id") |>
  mutate(
    soil_quality_mean = (soil_quality_him + soil_quality_lim) / 2,
    # irrigation's share of production value — bounded in [0, 1] because
    # WST (total) is a weighted composite, not WSI + WSR.
    irr_share_val = ifelse((val_irrigated + val_rainfed) > 0,
                           val_irrigated / (val_irrigated + val_rainfed),
                           NA_real_),
    # Value densities. cropland_share is in percent, so /100 to get fraction.
    cropland_area_ha   = zone_area_ha * cropland_share / 100,
    val_per_ha_land    = val_total_intl_usd / zone_area_ha,
    val_per_ha_cropland = ifelse(cropland_area_ha > 0,
                                 val_total_intl_usd / cropland_area_ha,
                                 NA_real_)
  )

write_parquet(wide, "generated/country_aez_features_wide.parquet")
message("Wrote wide features: ", nrow(wide), " rows × ", ncol(wide), " cols")

# ── 10. Long-form crop production (all 36 RES06-PRD layers, WST) ─────────
# Includes both individual crops (e.g. WHE, MZE) and FAO aggregates
# (e.g. CER, OIL, RTS). Crop dictionary tags each row accordingly.
message("Computing crop-specific production (WST)...")

crop_dict <- read_csv("data/crop_dictionary.csv", show_col_types = FALSE)

prd_files <- list.files(prd_dir, pattern = "\\.WST\\.tif$", full.names = TRUE)
prd_codes <- str_match(basename(prd_files), "RES06-PRD\\.([A-Z]+)\\.WST")[, 2]

crop_long <- map2_dfr(prd_files, prd_codes, function(f, code) {
  r <- rast(f) |> crop(africa_bbox)
  zstat(r, zone_r, "sum") |>
    rename(production_1000t = value) |>
    mutate(crop = code)
}) |>
  left_join(crop_dict, by = "crop")

write_parquet(crop_long, "generated/country_aez_crop_production.parquet")
message("Wrote crop production: ", nrow(crop_long), " rows (",
        length(prd_codes), " crops × ", length(unique(crop_long$zone_id)), " zones)")

message("\nDone. Outputs in generated/:")
message("  country_aez_zones.parquet")
message("  country_aez_features_wide.parquet")
message("  country_aez_crop_production.parquet")
