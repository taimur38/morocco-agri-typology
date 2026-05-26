# 15-perf-map.R
#
# Map of Africa where each country × AEZ zone is painted by its composite
# frontier ratio (within-cluster, p95-based).  Higher = closer to the
# within-cluster yield frontier on the crops the zone actually grows.
#
# Output:
#   imgs/zone_perf_map.png

library(tidyverse)
library(sf)
library(terra)
library(arrow)
library(rnaturalearth)

source("~/dev/gl-design/skills/gl-ggplot/assets/theme_gl.R")
gl_setup()

zone_r <- rast("generated/country_aez_zone_raster.tif")
names(zone_r) <- "zone_id"

perf <- read_csv("generated/country_aez_zone_perf.csv", show_col_types = FALSE)

africa_sf <- ne_countries(continent = "Africa", scale = "medium",
                          returnclass = "sf") |>
  filter(!is.na(iso_a3), iso_a3 != "-99")

# ── Paint zones by composite_perf ────────────────────────────────────────
rcl <- cbind(perf$zone_id, perf$composite_perf)
rcl <- rcl[!is.na(rcl[, 2]), , drop = FALSE]

painted <- classify(zone_r, rcl = rcl, others = NA) |>
  as.data.frame(xy = TRUE, na.rm = TRUE) |>
  as_tibble() |>
  rename(perf = 3)

# Diverging palette anchored at the across-zone median, so half of zones
# read "above" and half "below" the typical performer.
midpoint <- median(perf$composite_perf, na.rm = TRUE)

# ── Plot ─────────────────────────────────────────────────────────────────
p <- ggplot() +
  geom_raster(data = painted, aes(x = x, y = y, fill = perf)) +
  geom_sf(data = africa_sf, fill = NA, color = "black", linewidth = 0.25) +
  coord_sf(xlim = c(-20, 55), ylim = c(-37, 38), expand = FALSE) +
  scale_fill_gradient2(
    low      = "#b2182b",     # red — far from frontier
    mid      = "#f7f7f7",
    high     = "#1b7837",     # green — at/near frontier
    midpoint = midpoint,
    limits   = c(0, 1),
    oob      = scales::squish,
    breaks   = c(0, 0.5, 1),
    labels   = scales::percent_format(accuracy = 1),
    na.value = "grey90"
  ) +
  labs(title    = "Within-cluster distance from yield frontier, country × AEZ zones",
       subtitle = sprintf("Composite area-weighted yield ÷ within-cluster p95 peer · 1 = at p95 frontier · midpoint = across-zone median (%.0f%%)",
                          100 * midpoint),
       x = NULL, y = NULL, fill = "Frontier ratio") +
  theme(axis.title = element_blank(),
        legend.position = "right")

ggsave("imgs/zone_perf_map.png", p, width = 11, height = 9, dpi = 150)

message("Done. Output: imgs/zone_perf_map.png")
