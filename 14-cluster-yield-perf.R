# 14-cluster-yield-perf.R
#
# Within-cluster yield-frontier analysis (distance from potential).
#
#   frontier_ratio(z, c) = yield(z, c) / 95th-pctile yield of crop c among
#                          zones in the same cluster as z
#   composite_perf(z)    = Σ_c (area_share(z, c) × frontier_ratio(z, c))
#                          / Σ_c area_share(z, c) over crops where yield ok
#
# The 95th percentile (rather than the strict max) defines the within-cluster
# frontier so that a single outlier zone can't dominate the reference. Values
# above 1× are possible (top ~5% of zones for that crop). Crops need ≥ 5 zone
# observations within the cluster for the percentile to be meaningful.
#
# Visualised per-cluster as a quadrant scatter:
#   x = log10(zone total cropped area)        →  scale of the zone
#   y = composite_perf                        →  area-weighted frontier ratio
# Quadrant lines at the within-cluster median area and within-cluster median
# composite_perf (the y reference is now the cluster median, since 1 means
# the zone *is* the frontier and is rare by construction).
#
# Outputs:
#   imgs/zone_yield_perf_quadrants.png
#   generated/country_aez_zone_perf.csv

library(tidyverse)
library(arrow)
library(ggrepel)

source("~/dev/gl-design/skills/gl-ggplot/assets/theme_gl.R")
gl_setup()

# ── Load ──────────────────────────────────────────────────────────────────
har_long  <- read_parquet("generated/country_aez_har_long.parquet")
clusters  <- read_csv("generated/country_aez_zone_clusters_labeled.csv",
                      show_col_types = FALSE) |>
  mutate(cluster = factor(cluster))
crops_main <- read_csv("generated/country_aez_main_crops.csv",
                       show_col_types = FALSE) |> pull(crop)

zc <- har_long |>
  filter(crop %in% crops_main) |>
  inner_join(clusters |> select(zone_id, iso_a3, regime, regime_id, cluster,
                                label, zone_total_area_ha),
             by = "zone_id") |>
  mutate(area_share = ifelse(zone_total_area_ha > 0, area_ha / zone_total_area_ha, 0))

# ── Within-cluster 95th-pctile yield per crop, then frontier ratio ───────
zc <- zc |>
  filter(area_ha > 0, !is.na(yield_t_ha), is.finite(yield_t_ha)) |>
  group_by(label, crop) |>
  mutate(frontier_yield_cluster = quantile(yield_t_ha, 0.95, na.rm = TRUE),
         n_obs = n()) |>
  ungroup() |>
  filter(n_obs >= 5) |>          # need a meaningful peer set
  mutate(frontier_ratio = yield_t_ha / frontier_yield_cluster)

# ── Composite per zone ───────────────────────────────────────────────────
composite <- zc |>
  group_by(zone_id, iso_a3, regime, regime_id, label, zone_total_area_ha) |>
  summarise(
    composite_perf = sum(area_share * frontier_ratio) / sum(area_share),
    crops_used     = paste(sort(crop), collapse = "|"),
    n_crops        = n_distinct(crop),
    .groups = "drop"
  ) |>
  filter(!is.na(composite_perf))

write_csv(composite, "generated/country_aez_zone_perf.csv")

cat("Zones with composite perf computed: ", nrow(composite), " of ",
    nrow(clusters), "\n", sep = "")
cat("\nSummary by cluster:\n")
print(composite |>
        group_by(label) |>
        summarise(n = n(),
                  med_perf = round(median(composite_perf), 2),
                  q10 = round(quantile(composite_perf, 0.1), 2),
                  q90 = round(quantile(composite_perf, 0.9), 2),
                  .groups = "drop"))

# ── Per-cluster medians for the quadrant cross ───────────────────────────
cluster_meds <- composite |>
  group_by(label) |>
  summarise(med_area = median(zone_total_area_ha, na.rm = TRUE),
            med_perf = median(composite_perf,     na.rm = TRUE),
            .groups = "drop")

# ── Top + bottom labels per cluster ──────────────────────────────────────
labelled <- composite |>
  group_by(label) |>
  mutate(rank_top = rank(-composite_perf),
         rank_bot = rank( composite_perf),
         lbl = ifelse(rank_top <= 3 | rank_bot <= 3,
                      paste0(iso_a3, "·", regime_id), NA_character_)) |>
  ungroup()

# ── Plot ─────────────────────────────────────────────────────────────────
p <- labelled |>
  ggplot(aes(x = zone_total_area_ha, y = composite_perf)) +
  geom_vline(data = cluster_meds, aes(xintercept = med_area),
             color = "grey50", linewidth = 0.4) +
  geom_hline(data = cluster_meds, aes(yintercept = med_perf),
             color = "grey50", linewidth = 0.4) +
  geom_point(alpha = 0.85, size = 2) +
  geom_text_repel(aes(label = lbl), size = 2.8,
                  max.overlaps = Inf, min.segment.length = 0,
                  segment.color = "grey60", segment.size = 0.2) +
  scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale(),
                                              suffix = " ha")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ label, ncol = 3, scales = "free_x") +
  labs(title    = "Within-cluster yield frontier: distance from peer p95",
       subtitle = "x = zone total cropped area · y = area-weighted yield ÷ within-cluster 95th-pctile yield (per crop) · top/bottom 3 labelled · cross at within-cluster medians",
       x = "Zone total cropped area (log)",
       y = "Composite frontier ratio (1 = at cluster p95)") +
  theme(strip.text = element_text(size = 9))

ggsave("imgs/zone_yield_perf_quadrants.png", p,
       width = 14, height = 14, dpi = 150)

message("Done. Outputs:")
message("  imgs/zone_yield_perf_quadrants.png")
message("  generated/country_aez_zone_perf.csv")
