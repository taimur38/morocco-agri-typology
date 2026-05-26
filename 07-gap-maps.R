# 07-gap-maps.R
#
# Visualize "distance from potential" per country × AEZ zone, USD-weighted,
# using the dollar-denominated gap features from 09-build-gap-usd.R
# (which itself depends on prices from 08-build-crop-prices.R).
#
# Outputs (imgs/):
#   gap_achievement_ratio.png       — achievement = actual_usd / potential_usd
#   gap_absolute.png                — absolute production gap in USD (log)
#   gap_by_group_heatmap.png        — USD achievement by crop group × regime
#   gap_scatter_size_vs_ratio.png   — zone scatter: potential USD vs ratio
#   gap_scatter_facet_by_regime.png — same, faceted by AEZ regime

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
zone_r   <- rast("generated/country_aez_zone_raster.tif")
names(zone_r) <- "zone_id"

gap_wide <- read_parquet("generated/country_aez_gap_wide_usd.parquet")
by_group <- read_parquet("generated/country_aez_gap_by_group_usd.parquet")

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

paint_zones <- function(zone_r, df, key = "zone_id", value_col) {
  vals <- df[[value_col]]
  rcl  <- cbind(df[[key]], vals)
  rcl  <- rcl[!is.na(rcl[, 2]), , drop = FALSE]
  r    <- classify(zone_r, rcl = rcl, others = NA)
  as.data.frame(r, xy = TRUE, na.rm = TRUE) |>
    as_tibble() |>
    rename(value = 3)
}

base_map <- function(df, fill_name) {
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = africa_sf, fill = NA, color = "black", linewidth = 0.25) +
    coord_sf(xlim = c(-20, 55), ylim = c(-37, 38), expand = FALSE) +
    labs(x = NULL, y = NULL, fill = fill_name) +
    theme(axis.title = element_blank(),
          legend.position = "right")
}

# ── 1. Achievement ratio (actual_usd / potential_usd) ────────────────────
ach_df <- paint_zones(zone_r, gap_wide, value_col = "achievement_ratio_usd")

p_ach <- base_map(ach_df, "Achievement") +
  scale_fill_viridis_c(
    option = "mako", direction = -1,
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), na.value = "grey90"
  ) +
  labs(title    = "Yield achievement (USD-weighted): actual / potential",
       subtitle = paste0("20 QGA crops priced at country-level USD/tonne (FAOSTAT).",
                         " Higher = closer to potential dollar value."))

ggsave("imgs/gap_achievement_ratio.png", p_ach,
       width = 9, height = 9, dpi = 150)

# ── 2. Absolute production gap in USD (log) ──────────────────────────────
abs_df <- paint_zones(zone_r, gap_wide, value_col = "sum_gap_usd") |>
  mutate(value = ifelse(value > 0, value, NA_real_))

p_abs <- base_map(abs_df, "Gap (USD)") +
  scale_fill_viridis_c(
    option = "mako", direction = -1, trans = "log10",
    labels = scales::label_number(scale_cut = cut_short_scale(), prefix = "$"),
    na.value = "grey90"
  ) +
  labs(title    = "Absolute production gap per zone (USD)",
       subtitle = "Σ over 20 QGA crops of (potential − actual) priced at country USD/tonne, log scale")

ggsave("imgs/gap_absolute.png", p_abs,
       width = 9, height = 9, dpi = 150)

# ── 3. Heatmap: achievement by crop group × regime ──────────────────────
zones <- read_parquet("generated/country_aez_zones.parquet")

hm <- by_group |>
  left_join(zones |> select(zone_id, regime), by = "zone_id") |>
  filter(!is.na(group), !is.na(regime), !is.na(achievement),
         (sum_actual + sum_gap) > 0) |>
  group_by(regime, group) |>
  summarise(
    actual = sum(sum_actual, na.rm = TRUE),
    gap    = sum(sum_gap,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(achievement = actual / (actual + gap),
         regime = factor(regime, levels = regime_levels))

p_hm <- hm |>
  ggplot(aes(x = regime, y = fct_rev(factor(group)))) +
  geom_tile(aes(fill = achievement), color = "white", linewidth = 0.4) +
  geom_text(aes(label = scales::percent(achievement, accuracy = 1),
                color = ifelse(achievement > 0.5, "white", "black")),
            size = 3) +
  scale_color_identity() +
  scale_fill_viridis_c(
    option = "mako", direction = -1,
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), na.value = "grey90"
  ) +
  scale_x_discrete(position = "top") +
  labs(title    = "Yield achievement (USD-weighted) by AEZ regime × crop group",
       subtitle = "Africa-wide, weighted by potential dollar value across 20 QGA crops",
       x = NULL, y = NULL, fill = "Achievement") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0),
        panel.grid = element_blank())

ggsave("imgs/gap_by_group_heatmap.png", p_hm,
       width = 12, height = 5, dpi = 150)

# ── 4. Scatter: zone potential vs achievement, with quadrant lines ───────
# Drop Built-up/water (urban-adjacent zones, not really agricultural).
# Quadrant lines at the Africa-wide medians of potential USD and achievement.
sc_base <- gap_wide |>
  filter(sum_potential_usd > 0, !is.na(achievement_ratio_usd),
         regime != "Built-up / water")

med_pot <- median(sc_base$sum_potential_usd,    na.rm = TRUE)
med_ach <- median(sc_base$achievement_ratio_usd, na.rm = TRUE)

sc <- sc_base |>
  mutate(regime = factor(regime, levels = setdiff(regime_levels, "Built-up / water")),
         is_top = rank(-sum_potential_usd) <= 12,
         lbl    = ifelse(is_top, paste0(iso_a3, "·", regime_id), NA_character_))

p_sc <- sc |>
  ggplot(aes(x = sum_potential_usd, y = achievement_ratio_usd, color = regime)) +
  geom_vline(xintercept = med_pot, color = "grey50", linewidth = 0.4) +
  geom_hline(yintercept = med_ach, color = "grey50", linewidth = 0.4) +
  geom_point(alpha = 0.85, size = 2) +
  ggrepel::geom_text_repel(aes(label = lbl), size = 3,
                           show.legend = FALSE, max.overlaps = 20) +
  scale_color_manual(values = regime_pal, na.value = "grey60") +
  scale_x_log10(labels = scales::label_number(scale_cut = cut_short_scale(),
                                              prefix = "$")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(title    = "Yield achievement (USD-weighted) vs potential value",
       subtitle = paste0("Quadrants split at Africa median (potential = ",
                         scales::label_number(scale_cut = cut_short_scale(),
                                              prefix = "$")(med_pot),
                         ", achievement = ",
                         scales::percent(med_ach, accuracy = 1),
                         ") · Built-up/water excluded"),
       x = "Potential production value (USD, log)",
       y = "Achievement ratio",
       color = "AEZ regime") +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = "bottom")

ggsave("imgs/gap_scatter_size_vs_ratio.png", p_sc,
       width = 11, height = 7, dpi = 150)

# ── 5. Same scatter, faceted by AEZ regime ───────────────────────────────
drop_regimes <- c("Built-up / water", "Cold (no permafrost)")
facet_levels <- setdiff(regime_levels, drop_regimes)

sc_facet <- gap_wide |>
  filter(sum_potential_usd > 0, !is.na(achievement_ratio_usd),
         !regime %in% drop_regimes) |>
  mutate(regime = factor(regime, levels = facet_levels),
         lbl    = iso_a3)

# Per-facet medians for the quadrant cross
facet_meds <- sc_facet |>
  group_by(regime) |>
  summarise(med_pot = median(sum_potential_usd, na.rm = TRUE),
            med_ach = median(achievement_ratio_usd, na.rm = TRUE),
            .groups = "drop")

p_sc_facet <- sc_facet |>
  ggplot(aes(x = sum_potential_usd, y = achievement_ratio_usd)) +
  geom_vline(data = facet_meds, aes(xintercept = med_pot),
             color = "grey50", linewidth = 0.4) +
  geom_hline(data = facet_meds, aes(yintercept = med_ach),
             color = "grey50", linewidth = 0.4) +
  geom_point(alpha = 0.85, size = 2) +
  ggrepel::geom_text_repel(aes(label = lbl), size = 2.8,
                           max.overlaps = Inf, min.segment.length = 0,
                           segment.color = "grey60", segment.size = 0.2) +
  scale_x_log10(labels = scales::label_number(scale_cut = cut_short_scale(),
                                              prefix = "$")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ regime, ncol = 3, scales = "free") +
  labs(title    = "Yield achievement (USD-weighted) vs potential value, by AEZ regime",
       subtitle = "Each panel: country × AEZ zones in that regime · cross at within-regime median",
       x = "Potential production value (USD, log)",
       y = "Achievement ratio") +
  theme(strip.text = element_text(size = 9))

ggsave("imgs/gap_scatter_facet_by_regime.png", p_sc_facet,
       width = 14, height = 14, dpi = 150)

message("Done. Saved 5 figures to imgs/.")
