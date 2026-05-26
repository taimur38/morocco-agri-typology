# 11-cluster-zones.R
#
# Cluster country × AEZ zones by their cropland-use vector.
#   - K-means in the original 23-dim area-share space
#   - UMAP 2-D embedding for visualisation
#   - Heatmap of zones × crops
#
# Outputs (first pass — labels added in 12):
#   imgs/zone_cropland_use_heatmap.png
#   imgs/zone_umap_clusters.png
#   generated/country_aez_zone_clusters.parquet
#   generated/country_aez_cluster_signatures.csv
#
# Run twice: first pass writes cluster signatures so a human/LLM can label the
# clusters; second pass (12-cluster-viz.R) reads those labels and rebuilds the
# coloured-and-labelled UMAP map.

library(tidyverse)
library(arrow)
library(uwot)

source("~/dev/gl-design/skills/gl-ggplot/assets/theme_gl.R")
gl_setup()

# ── Load ──────────────────────────────────────────────────────────────────
asw            <- read_parquet("generated/country_aez_area_share_wide.parquet")
main_crops_tbl <- read_csv("generated/country_aez_main_crops.csv", show_col_types = FALSE)
crops          <- main_crops_tbl |> arrange(desc(total_area_ha)) |> pull(crop)

# Keep only zones with non-trivial cropland (a few desert/Built-up zones have
# zero or near-zero cropped area and would yield degenerate vectors).
asw <- asw |> filter(zone_total_area_ha > 1000)
message("Zones with >1000 ha cropland: ", nrow(asw))

M <- asw |> select(all_of(crops)) |> as.matrix()
rownames(M) <- paste0(asw$iso_a3, "·", asw$regime_id)

# Renormalise to be safe (rows should already sum to 1 across the 23 main crops
# minus a tiny residual that lives in the dropped tail crops).
M <- M / pmax(rowSums(M), 1e-9)

# ── 1. Heatmap (zones × crops) ────────────────────────────────────────────
hc_zones   <- hclust(dist(M), method = "ward.D2")
zone_order <- rownames(M)[hc_zones$order]

hm_df <- as.data.frame(M) |>
  rownames_to_column("zone") |>
  mutate(zone = factor(zone, levels = zone_order)) |>
  pivot_longer(-zone, names_to = "crop", values_to = "share") |>
  mutate(crop = factor(crop, levels = crops))

p_hm <- hm_df |>
  ggplot(aes(x = crop, y = zone, fill = share)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen",
                      labels = scales::percent_format(accuracy = 1)) +
  labs(title    = "Cropland-use composition per country × AEZ zone",
       subtitle = "Rows = zones (Ward hierarchical order on area-share vectors) · cell = zone's crop area share",
       x = NULL, y = NULL, fill = "Area share") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("imgs/zone_cropland_use_heatmap.png", p_hm,
       width = 9, height = 14, dpi = 150)

# ── 2. K-means on cropland-use ────────────────────────────────────────────
set.seed(42)
K <- 8
km <- kmeans(M, centers = K, nstart = 50, iter.max = 200)
asw$cluster <- factor(km$cluster)

# ── 3. UMAP for visualisation ─────────────────────────────────────────────
set.seed(42)
um <- uwot::umap(M, n_neighbors = 15, min_dist = 0.15, metric = "cosine")
asw$umap1 <- um[, 1]; asw$umap2 <- um[, 2]

write_parquet(asw, "generated/country_aez_zone_clusters.parquet")

# ── 4. Cluster signatures ────────────────────────────────────────────────
cluster_signatures <- asw |>
  pivot_longer(all_of(crops), names_to = "crop", values_to = "share") |>
  group_by(cluster, crop) |>
  summarise(mean_share = mean(share), .groups = "drop") |>
  group_by(cluster) |>
  arrange(desc(mean_share)) |>
  slice_head(n = 5) |>
  summarise(top_crops = paste0(crop, "(", round(mean_share*100), "%)", collapse = " "),
            .groups = "drop")

cluster_members <- asw |>
  group_by(cluster) |>
  summarise(n_zones    = n(),
            top_iso    = paste(head(unique(iso_a3), 8), collapse = ","),
            top_regime = names(sort(table(regime), decreasing = TRUE))[1],
            .groups = "drop")

cluster_summary <- cluster_signatures |>
  left_join(cluster_members, by = "cluster") |>
  arrange(cluster)

cat("\n=== Cluster signatures (top crops by mean area share) ===\n")
print(cluster_summary, n = 30, width = Inf)

write_csv(cluster_summary, "generated/country_aez_cluster_signatures.csv")

# ── 5. First-pass UMAP plot (uncoloured-labelled, just clusters) ─────────
# Just for sanity — coloured by cluster id only, no human label yet.
p_umap_raw <- asw |>
  ggplot(aes(x = umap1, y = umap2, color = cluster)) +
  geom_point(alpha = 0.85, size = 2) +
  labs(title    = paste0("UMAP of country×AEZ zones, k-means k=", K, " (raw cluster ids)"),
       subtitle = "Coloured by cluster id only — labels assigned in 12-cluster-viz.R after inspection",
       x = "UMAP-1", y = "UMAP-2", color = "Cluster") +
  theme(legend.position = "right")

ggsave("imgs/zone_umap_clusters_raw.png", p_umap_raw,
       width = 9, height = 7, dpi = 150)

# ── 6. Cluster × regime cross-tab (sanity) ───────────────────────────────
cat("\n=== Cluster × AEZ regime cross-tab ===\n")
asw |> count(cluster, regime) |>
  pivot_wider(names_from = regime, values_from = n, values_fill = 0) |>
  print(width = Inf)

cat("\n=== Member zones for each cluster (sample) ===\n")
asw |> arrange(cluster) |>
  group_by(cluster) |>
  slice_head(n = 10) |>
  select(cluster, iso_a3, regime, zone_total_area_ha) |>
  mutate(zone_total_area_ha = scales::label_number(scale_cut = scales::cut_short_scale())(zone_total_area_ha)) |>
  print(n = 80)

message("\nDone. Outputs:")
message("  imgs/zone_cropland_use_heatmap.png")
message("  imgs/zone_umap_clusters_raw.png")
message("  generated/country_aez_zone_clusters.parquet")
message("  generated/country_aez_cluster_signatures.csv")
