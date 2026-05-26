# 05-zone-maps.R
#
# Visualize the 246 country × AEZ-regime zones built by script 04.
# Each map paints the zone raster by a feature from country_aez_features_wide.
#
# Outputs (imgs/):
#   zones_regime.png              — sanity check: thermal regime per pixel
#   zones_cropland_share.png      — cropland cover share
#   zones_val_total.png           — total ag production value (log)
#   zones_val_per_ha_land.png     — value density, per ha of total land
#   zones_val_per_ha_cropland.png — value density, per ha of cropland
#   zones_irr_share.png           — irrigation's share of production value
#   zones_soil_quality.png        — soil quality mean (HIM + LIM)
#   zones_scatter_val_vs_cropland.png — national × AEZ scatter

library(tidyverse)
library(sf)
library(terra)
library(arrow)
library(rnaturalearth)
library(scales)

source("~/dev/gl-design/skills/gl-ggplot/assets/theme_gl.R")
gl_setup()

africa_bbox <- ext(-20, 55, -37, 40)

# ── Inputs ───────────────────────────────────────────────────────────────
zone_r <- rast("generated/country_aez_zone_raster.tif")
names(zone_r) <- "zone_id"

wide  <- read_parquet("generated/country_aez_features_wide.parquet")
zones <- read_parquet("generated/country_aez_zones.parquet")

africa_sf <- ne_countries(continent = "Africa", scale = "medium",
                          returnclass = "sf") |>
  filter(!is.na(iso_a3), iso_a3 != "-99")

regime_levels <- c("Tropics lowland", "Tropics highland",
                   "Sub-tropics warm", "Sub-tropics mod. cool",
                   "Sub-tropics cool", "Temperate moderate",
                   "Temperate cool", "Cold (no permafrost)",
                   "Severe terrain/soil", "Irrigated / hydromorphic",
                   "Desert/Arid", "Boreal/Arctic", "Built-up / water")

regime_pal <- c(
  "Tropics lowland"          = "#b00000",
  "Tropics highland"         = "#800080",
  "Sub-tropics warm"         = "#e67300",
  "Sub-tropics mod. cool"    = "#ded701",
  "Sub-tropics cool"         = "#58b000",
  "Temperate moderate"       = "#2e8b57",
  "Temperate cool"           = "#408080",
  "Cold (no permafrost)"     = "#0080ff",
  "Severe terrain/soil"      = "#8a8a8a",
  "Irrigated / hydromorphic" = "#0076ae",
  "Desert/Arid"              = "#f0d090",
  "Boreal/Arctic"            = "#8c8cff",
  "Built-up / water"         = "#e60000"
)

# ── Helper: paint zone raster by a per-zone scalar ───────────────────────
# For numeric features we build a 2-col substitution matrix, apply with
# terra::subst, and turn into a tibble for ggplot.
paint_zones <- function(zone_r, df, key = "zone_id", value_col) {
  vals <- df[[value_col]]
  rcl  <- cbind(df[[key]], vals)
  rcl  <- rcl[!is.na(rcl[, 2]), , drop = FALSE]
  r    <- classify(zone_r, rcl = rcl, others = NA)
  as.data.frame(r, xy = TRUE, na.rm = TRUE) |>
    as_tibble() |>
    rename(value = 3)
}

base_map <- function(df, fill_name, ...) {
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = africa_sf, fill = NA, color = "black", linewidth = 0.25) +
    coord_sf(xlim = c(-20, 55), ylim = c(-37, 38), expand = FALSE) +
    labs(x = NULL, y = NULL, fill = fill_name) +
    theme(axis.title = element_blank(),
          legend.position = "right")
}

# ── 1. Regime sanity check (categorical) ─────────────────────────────────
regime_df <- paint_zones(zone_r, wide, value_col = "regime_id") |>
  mutate(regime = factor(regime_levels[as.integer(value)],
                         levels = regime_levels))

p_regime <- ggplot() +
  geom_raster(data = regime_df, aes(x = x, y = y, fill = regime)) +
  geom_sf(data = africa_sf, fill = NA, color = "black", linewidth = 0.25) +
  scale_fill_manual(values = regime_pal, na.value = NA) +
  coord_sf(xlim = c(-20, 55), ylim = c(-37, 38), expand = FALSE) +
  guides(fill = guide_legend(ncol = 2, title = NULL)) +
  labs(title    = "Country × AEZ regime — 246 zones",
       subtitle = "GAEZ v5 AEZ57 grouped to 13 regimes, rasterized with country borders",
       x = NULL, y = NULL) +
  theme(axis.title = element_blank(),
        legend.position = "bottom")

ggsave("imgs/zones_regime.png", p_regime,
       width = 9, height = 11, dpi = 150)

# ── 2. Cropland share ────────────────────────────────────────────────────
crop_df <- paint_zones(zone_r, wide, value_col = "cropland_share")

p_crop <- base_map(crop_df, "% cropland") +
  scale_fill_viridis_c(option = "mako", direction = -1,
                       labels = scales::percent_format(scale = 1, accuracy = 1),
                       na.value = "grey90") +
  labs(title    = "Cropland land-cover share by country × AEZ zone",
       subtitle = "GAEZ v5 LR-LCC class 02 (cropland), mean over 10-km pixels")

ggsave("imgs/zones_cropland_share.png", p_crop,
       width = 9, height = 9, dpi = 150)

# ── 3. Total production value (log) ──────────────────────────────────────
val_df <- paint_zones(zone_r, wide, value_col = "val_total_intl_usd") |>
  mutate(value = ifelse(value > 0, value, NA_real_))

p_val <- base_map(val_df, "Value (int'l USD)") +
  scale_fill_viridis_c(
    option = "mako", direction = -1, trans = "log10",
    labels = scales::label_number(scale_cut = cut_short_scale()),
    na.value = "grey90"
  ) +
  labs(title    = "Total agricultural production value (2020)",
       subtitle = "GAEZ v5 RES06-VAL, summed to country × AEZ zone, log scale")

ggsave("imgs/zones_val_total.png", p_val,
       width = 9, height = 9, dpi = 150)

# ── 3b. Value density per hectare of total land ──────────────────────────
vph_land_df <- paint_zones(zone_r, wide, value_col = "val_per_ha_land") |>
  mutate(value = ifelse(value > 0, value, NA_real_))

p_vph_land <- base_map(vph_land_df, "USD / ha") +
  scale_fill_viridis_c(
    option = "mako", direction = -1, trans = "log10",
    labels = scales::label_number(scale_cut = cut_short_scale()),
    na.value = "grey90"
  ) +
  labs(title    = "Agricultural value density — per hectare of zone land",
       subtitle = "RES06-VAL summed per zone, divided by total zone area (log scale)")

ggsave("imgs/zones_val_per_ha_land.png", p_vph_land,
       width = 9, height = 9, dpi = 150)

# ── 3c. Value density per hectare of cropland ────────────────────────────
vph_crop_df <- paint_zones(zone_r, wide, value_col = "val_per_ha_cropland") |>
  mutate(value = ifelse(value > 0, value, NA_real_))

p_vph_crop <- base_map(vph_crop_df, "USD / ha cropland") +
  scale_fill_viridis_c(
    option = "mako", direction = -1, trans = "log10",
    labels = scales::label_number(scale_cut = cut_short_scale()),
    na.value = "grey90"
  ) +
  labs(title    = "Agricultural productivity — per hectare of cropland",
       subtitle = "RES06-VAL / (zone area × cropland share), log scale")

ggsave("imgs/zones_val_per_ha_cropland.png", p_vph_crop,
       width = 9, height = 9, dpi = 150)

# ── 4. Irrigation share of production value ──────────────────────────────
irr_df <- paint_zones(zone_r, wide, value_col = "irr_share_val")

p_irr <- base_map(irr_df, "Irrigated share") +
  scale_fill_viridis_c(option = "mako", direction = -1,
                       labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), na.value = "grey90") +
  labs(title    = "Irrigation share of agricultural production value",
       subtitle = "Val(irrigated) / (Val(irrigated) + Val(rainfed)) by zone")

ggsave("imgs/zones_irr_share.png", p_irr,
       width = 9, height = 9, dpi = 150)

# ── 5. Soil quality mean (HIM + LIM average) ─────────────────────────────
soil_df <- paint_zones(zone_r, wide, value_col = "soil_quality_mean")

p_soil <- base_map(soil_df, "Soil quality (SQX)") +
  scale_fill_viridis_c(option = "mako", direction = -1, na.value = "grey90") +
  labs(title    = "Soil quality by country × AEZ zone",
       subtitle = "GAEZ v5 SQX SQ0, mean of HIM and LIM variants")

ggsave("imgs/zones_soil_quality.png", p_soil,
       width = 9, height = 9, dpi = 150)

# ── 6. Scatter: production value vs cropland share, by regime ────────────
scatter_df <- wide |>
  filter(!is.na(val_total_intl_usd), val_total_intl_usd > 0,
         !is.na(cropland_share)) |>
  mutate(regime = factor(regime, levels = regime_levels),
         # label the top 12 zones by value for orientation
         is_top = rank(-val_total_intl_usd) <= 12,
         lbl    = ifelse(is_top, paste0(iso_a3, "·", regime_id), NA_character_))

p_scatter <- scatter_df |>
  ggplot(aes(x = cropland_share, y = val_total_intl_usd, color = regime)) +
  geom_point(alpha = 0.85, size = 2) +
  ggrepel::geom_text_repel(aes(label = lbl), size = 3,
                           show.legend = FALSE, max.overlaps = 20) +
  scale_color_manual(values = regime_pal, na.value = "grey60") +
  scale_x_continuous(labels = scales::percent_format(scale = 1, accuracy = 1)) +
  scale_y_log10(labels = scales::label_number(scale_cut = cut_short_scale())) +
  labs(title    = "Where is African agricultural value concentrated?",
       subtitle = "246 country × AEZ zones · size 1 point = 1 zone · top 12 by value labelled",
       x = "Cropland share of zone area",
       y = "Total production value (int'l USD, log scale)",
       color = "AEZ regime") +
  guides(color = guide_legend(ncol = 1))

ggsave("imgs/zones_scatter_val_vs_cropland.png", p_scatter,
       width = 11, height = 7, dpi = 150)

message("Done. Saved 8 figures to imgs/.")
