# 13-cluster-map.R
#
# Paint Africa with the cropland-use cluster assigned to each country × AEZ
# zone in 11/12.
#
# Output:
#   imgs/zone_cluster_map.png

library(tidyverse)
library(sf)
library(terra)
library(arrow)
library(rnaturalearth)

source("~/dev/gl-design/skills/gl-ggplot/assets/theme_gl.R")
gl_setup()

zone_r <- rast("generated/country_aez_zone_raster.tif")
names(zone_r) <- "zone_id"

clusters <- read_csv("generated/country_aez_zone_clusters_labeled.csv",
                     show_col_types = FALSE)

africa_sf <- ne_countries(continent = "Africa", scale = "medium",
                          returnclass = "sf") |>
  filter(!is.na(iso_a3), iso_a3 != "-99")

# ── Paint zones ──────────────────────────────────────────────────────────
cl_lookup <- clusters |> distinct(cluster, label) |>
  mutate(cluster_int = as.integer(as.character(cluster))) |>
  arrange(cluster_int)

paint_df <- clusters |> mutate(cluster_int = as.integer(as.character(cluster)))
rcl <- cbind(paint_df$zone_id, paint_df$cluster_int)
rcl <- rcl[!is.na(rcl[, 2]), , drop = FALSE]

painted <- classify(zone_r, rcl = rcl, others = NA) |>
  as.data.frame(xy = TRUE, na.rm = TRUE) |>
  as_tibble() |>
  rename(cluster_int = 3) |>
  left_join(cl_lookup |> select(cluster_int, label), by = "cluster_int") |>
  mutate(label = factor(label, levels = cl_lookup$label))

# ── Cluster palette ──────────────────────────────────────────────────────
# Hand-pick so that visually similar regimes (humid mixed/maize/sugarcane) get
# nearby greens, drylands get warm/sandy hues, Mediterranean gets a cool blue.
cluster_pal <- c(
  "Humid Mixed-Staple"          = "#1b7837",   # deep green
  "Maize Belt"                  = "#5aae61",   # green
  "Sugarcane Plantation"        = "#a6dba0",   # pale green
  "Wetland Rice"                = "#762a83",   # purple (paddy hydrology)
  "Sahel Millet"                = "#fdae61",   # warm sand
  "Sudano-Sahel Mixed"          = "#d6604d",   # red-brown
  "Dryland Sorghum"             = "#b2182b",   # deep red
  "Mediterranean Wheat-Barley"  = "#4393c3"    # cool blue
)

p <- ggplot() +
  geom_raster(data = painted, aes(x = x, y = y, fill = label)) +
  geom_sf(data = africa_sf, fill = NA, color = "black", linewidth = 0.25) +
  coord_sf(xlim = c(-20, 55), ylim = c(-37, 38), expand = FALSE) +
  scale_fill_manual(values = cluster_pal, na.value = "grey90") +
  labs(title    = "Cropland-use clusters across African country × AEZ zones",
       subtitle = "8 cropland-use regimes from k-means on 23-dim crop area-share vectors",
       x = NULL, y = NULL, fill = NULL) +
  theme(axis.title = element_blank(),
        legend.position = "right")

ggsave("imgs/zone_cluster_map.png", p, width = 11, height = 9, dpi = 150)

message("Done. Output: imgs/zone_cluster_map.png")
