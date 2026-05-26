# 02-africa-base-map.R
#
# Orienting figure: Africa with country borders and the GAEZ v5 AEZ57
# agro-ecological zones overlaid. Produces two maps:
#   imgs/africa_aez57_full.png     — 57-class version (official GAEZ colors)
#   imgs/africa_aez57_grouped.png  — collapsed to thermal regime (8 groups + special)
#   imgs/africa_aez57_barchart.png — pixel share by AEZ class, Africa-wide

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(rnaturalearth)

source("~/dev/gl-design/skills/gl-ggplot/assets/theme_gl.R")
gl_setup()

# ── Paths ────────────────────────────────────────────────────────────────
aez_tif    <- "~/dev/shared-data/fao/gaez/v5/global/AEZ57/GAEZ-V5.AEZ57.tif"
legend_csv <- "~/dev/shared-data/fao/gaez/v5/global/AEZ57/aez57_legend.csv"

# ── Geometries: Africa at country level ──────────────────────────────────
africa_sf <- ne_countries(continent = "Africa", scale = "medium",
                          returnclass = "sf") |>
  select(iso_a3, name, geometry)

africa_bbox <- ext(-20, 55, -37, 40)

# ── Raster: AEZ57, crop to Africa, aggregate for plotting ────────────────
aez <- rast(aez_tif) |> crop(africa_bbox)
# ~1 km → ~5 km using modal (categorical) for plotting speed
aez_plot <- aggregate(aez, fact = 5, fun = "modal", na.rm = TRUE)

# ── Legend + thermal-regime groupings ────────────────────────────────────
legend <- read_csv(legend_csv, show_col_types = FALSE)

# Collapse labels into a smaller thermal / special grouping
legend <- legend |>
  mutate(
    regime = case_when(
      value == 0                 ~ NA_character_,
      value %in% 1:6             ~ "Tropics lowland",
      value %in% 7:12            ~ "Tropics highland",
      value %in% 13:18           ~ "Sub-tropics warm",
      value %in% 19:24           ~ "Sub-tropics mod. cool",
      value %in% 25:30           ~ "Sub-tropics cool",
      value %in% 31:36           ~ "Temperate moderate",
      value %in% 37:42           ~ "Temperate cool",
      value %in% 43:48           ~ "Cold (no permafrost)",
      value %in% c(49, 50)       ~ "Severe terrain/soil",
      value %in% c(51, 52)       ~ "Irrigated / hydromorphic",
      value == 53                ~ "Desert/Arid",
      value %in% c(54, 55)       ~ "Boreal/Arctic",
      value %in% c(56, 57)       ~ "Built-up / water",
      TRUE                       ~ "Other"
    )
  )

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

# ── Raster → data frame for ggplot ───────────────────────────────────────
aez_df <- as.data.frame(aez_plot, xy = TRUE, na.rm = TRUE) |>
  rename(value = 3) |>
  filter(value > 0) |>
  left_join(legend, by = "value") |>
  mutate(
    label  = factor(label,  levels = legend$label),
    regime = factor(regime, levels = regime_levels)
  )

# ── Map 1: full 57-class version with official colors ────────────────────
full_pal <- setNames(legend$hex, legend$label)
used_labels <- levels(droplevels(aez_df$label))

p_full <- ggplot() +
  geom_raster(data = aez_df, aes(x = x, y = y, fill = label)) +
  geom_sf(data = africa_sf, fill = NA, color = "black", linewidth = 0.25) +
  scale_fill_manual(values = full_pal, breaks = used_labels, na.value = NA) +
  coord_sf(xlim = c(-20, 55), ylim = c(-37, 38), expand = FALSE) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE, title = NULL)) +
  labs(title    = "GAEZ v5 AEZ57 — Africa",
       subtitle = "Official 57-class agro-ecological zones (climate × soil × terrain × land cover)") +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 6),
        legend.key.size = unit(0.3, "cm"),
        axis.title      = element_blank())

ggsave("imgs/africa_aez57_full.png", p_full,
       width = 11, height = 14, dpi = 150)

# ── Map 2: collapsed to thermal regime (readable legend) ─────────────────
p_grouped <- ggplot() +
  geom_raster(data = aez_df, aes(x = x, y = y, fill = regime)) +
  geom_sf(data = africa_sf, fill = NA, color = "black", linewidth = 0.25) +
  scale_fill_manual(values = regime_pal, na.value = NA) +
  coord_sf(xlim = c(-20, 55), ylim = c(-37, 38), expand = FALSE) +
  guides(fill = guide_legend(ncol = 2, title = NULL)) +
  labs(title    = "Africa agro-ecological zones — thermal regime",
       subtitle = "GAEZ v5 AEZ57 collapsed to thermal / special-zone groups") +
  theme(legend.position = "bottom",
        axis.title      = element_blank())

ggsave("imgs/africa_aez57_grouped.png", p_grouped,
       width = 9, height = 11, dpi = 150)

# ── Bar chart: Africa-wide pixel share per AEZ class ─────────────────────
aez_counts <- aez_df |>
  count(value, label, regime, name = "pixels") |>
  mutate(share = pixels / sum(pixels))

p_bar <- aez_counts |>
  mutate(label = fct_reorder(label, share)) |>
  ggplot(aes(x = share, y = label, fill = regime)) +
  geom_col() +
  scale_fill_manual(values = regime_pal, na.value = "grey70") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "AEZ class shares across Africa",
       subtitle = "Share of Africa land pixels in each AEZ57 class",
       x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "right",
        plot.margin = margin(10, 10, 10, 10))

ggsave("imgs/africa_aez57_barchart.png", p_bar,
       width = 14, height = 11, dpi = 150)

# Also save the counts table for quick reference
write_csv(aez_counts, "generated/africa_aez_pixel_counts.csv")

message("Done. Saved 3 figures to imgs/ and pixel counts to generated/.")
