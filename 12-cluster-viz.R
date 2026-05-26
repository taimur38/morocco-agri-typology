# 12-cluster-viz.R
#
# Second-pass visualisation: read the cluster assignments and UMAP coords from
# 11-cluster-zones.R, attach human/LLM-generated cluster labels, and produce:
#   - imgs/zone_umap_clusters_labeled.png  — scatter coloured by cluster, with
#                                            label text at each cluster centroid
#   - imgs/zone_cluster_signature_heatmap.png — cluster centroids × crops
#
# Cluster labels were assigned by inspecting the top-5 mean area shares and
# member countries from `country_aez_cluster_signatures.csv`. They are
# qualitative descriptors of the cropland-use signature, not strict definitions.

library(tidyverse)
library(arrow)
library(ggrepel)

source("~/dev/gl-design/skills/gl-ggplot/assets/theme_gl.R")
gl_setup()

asw   <- read_parquet("generated/country_aez_zone_clusters.parquet")
crops <- read_csv("generated/country_aez_main_crops.csv", show_col_types = FALSE) |>
  arrange(desc(total_area_ha)) |> pull(crop)

# ── Cluster labels (LLM-assigned from cropland-use signatures) ───────────
cluster_labels <- tibble::tribble(
  ~cluster, ~label,
  "1",     "Humid Mixed-Staple",
  "2",     "Maize Belt",
  "3",     "Wetland Rice",
  "4",     "Sugarcane Plantation",
  "5",     "Sahel Millet",
  "6",     "Mediterranean Wheat-Barley",
  "7",     "Sudano-Sahel Mixed",
  "8",     "Dryland Sorghum"
) |> mutate(cluster = factor(cluster))

asw <- asw |> left_join(cluster_labels, by = "cluster")

# ── Centroid (mean UMAP position) per cluster, for label placement ───────
centroids <- asw |>
  group_by(cluster, label) |>
  summarise(umap1 = mean(umap1), umap2 = mean(umap2),
            n_zones = n(), .groups = "drop")

# ── Labeled UMAP ──────────────────────────────────────────────────────────
p_umap <- asw |>
  ggplot(aes(x = umap1, y = umap2, color = label)) +
  geom_point(alpha = 0.85, size = 2) +
  geom_label_repel(data = centroids,
                   aes(label = paste0(label, "\n(", n_zones, " zones)")),
                   size = 3.5, fontface = "bold", color = "black",
                   fill = alpha("white", 0.85), label.size = 0.2,
                   box.padding = 0.6, point.padding = 0.4,
                   min.segment.length = 0, segment.color = "grey40",
                   max.overlaps = Inf, show.legend = FALSE) +
  labs(title    = "Country × AEZ zones in cropland-use space",
       subtitle = "UMAP of 23-dim area-share vectors · k-means clusters labelled at centroids",
       x = "UMAP-1", y = "UMAP-2", color = NULL) +
  theme(legend.position = "none")

ggsave("imgs/zone_umap_clusters_labeled.png", p_umap,
       width = 11, height = 8, dpi = 150)

# ── Cluster signature heatmap ─────────────────────────────────────────────
sig <- asw |>
  pivot_longer(all_of(crops), names_to = "crop", values_to = "share") |>
  group_by(label, crop) |>
  summarise(mean_share = mean(share), .groups = "drop") |>
  mutate(crop  = factor(crop, levels = crops),
         label = factor(label, levels = cluster_labels$label))

p_sig <- sig |>
  ggplot(aes(x = crop, y = fct_rev(label), fill = mean_share)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(mean_share >= 0.05,
                               scales::percent(mean_share, accuracy = 1), ""),
                color = ifelse(mean_share > 0.3, "white", "black")),
            size = 3) +
  scale_color_identity() +
  scale_fill_gradient(low = "white", high = "darkgreen",
                      labels = scales::percent_format(accuracy = 1),
                      limits = c(0, NA)) +
  labs(title    = "Cluster signatures: mean cropland share by crop",
       subtitle = "Each row = one k-means cluster · cell = mean share of zones' cropland in that crop",
       x = NULL, y = NULL, fill = "Mean share") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

ggsave("imgs/zone_cluster_signature_heatmap.png", p_sig,
       width = 11, height = 5, dpi = 150)

# ── Save labelled cluster table ───────────────────────────────────────────
write_csv(asw |>
            select(zone_id, iso_a3, regime, regime_id, zone_total_area_ha,
                   cluster, label, umap1, umap2),
          "generated/country_aez_zone_clusters_labeled.csv")

message("Done. Outputs:")
message("  imgs/zone_umap_clusters_labeled.png")
message("  imgs/zone_cluster_signature_heatmap.png")
message("  generated/country_aez_zone_clusters_labeled.csv")
