# 16-perf-vs-achievement.R
#
# Combine the two complementary lenses on each country × AEZ zone:
#   x = composite_perf      (within-cluster frontier ratio, 1 = at peer p95)
#   y = achievement_ratio   (USD-weighted actual / potential from GAEZ QGA)
#
# Faceted by AEZ regime, labelled by ISO3.
# Quadrant cross at within-regime medians on both axes.
#
# Output:
#   imgs/zone_perf_vs_achievement.png

library(tidyverse)
library(arrow)
library(ggrepel)

source("~/dev/gl-design/skills/gl-ggplot/assets/theme_gl.R")
gl_setup()

# ── Load both metrics ────────────────────────────────────────────────────
perf <- read_csv("generated/country_aez_zone_perf.csv", show_col_types = FALSE)
gap  <- read_parquet("generated/country_aez_gap_wide_usd.parquet")

# Regime ordering (drops Built-up / water and Cold which are excluded elsewhere)
regime_levels <- c("Tropics lowland", "Tropics highland",
                   "Sub-tropics warm", "Sub-tropics mod. cool",
                   "Sub-tropics cool", "Temperate moderate",
                   "Temperate cool", "Severe terrain/soil",
                   "Irrigated / hydromorphic", "Desert/Arid")
drop_regimes <- c("Built-up / water", "Cold (no permafrost)")

cluster_pal <- c(
  "Humid Mixed-Staple"          = "#1b7837",
  "Maize Belt"                  = "#5aae61",
  "Sugarcane Plantation"        = "#a6dba0",
  "Wetland Rice"                = "#762a83",
  "Sahel Millet"                = "#fdae61",
  "Sudano-Sahel Mixed"          = "#d6604d",
  "Dryland Sorghum"             = "#b2182b",
  "Mediterranean Wheat-Barley"  = "#4393c3"
)

zc <- perf |>
  inner_join(gap |> select(zone_id, achievement_ratio_usd, sum_potential_usd),
             by = "zone_id") |>
  filter(!regime %in% drop_regimes,
         !is.na(achievement_ratio_usd),
         !is.na(composite_perf),
         sum_potential_usd > 0) |>
  mutate(regime = factor(regime, levels = regime_levels),
         label  = factor(label,  levels = names(cluster_pal)))

# ── Within-regime medians for the quadrant cross (both axes) ─────────────
regime_meds <- zc |>
  group_by(regime) |>
  summarise(med_perf = median(composite_perf,        na.rm = TRUE),
            med_ach  = median(achievement_ratio_usd, na.rm = TRUE),
            .groups = "drop")

# ── Plot ─────────────────────────────────────────────────────────────────
p <- zc |>
  ggplot(aes(x = composite_perf, y = achievement_ratio_usd, color = label)) +
  geom_vline(data = regime_meds, aes(xintercept = med_perf),
             color = "grey50", linewidth = 0.4) +
  geom_hline(data = regime_meds, aes(yintercept = med_ach),
             color = "grey50", linewidth = 0.4) +
  geom_point(alpha = 0.85, size = 2) +
  geom_text_repel(aes(label = iso_a3), size = 2.8,
                  max.overlaps = Inf, min.segment.length = 0,
                  segment.color = "grey60", segment.size = 0.2,
                  show.legend = FALSE) +
  scale_color_manual(values = cluster_pal, na.value = "grey60") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ regime, ncol = 3, scales = "free_y") +
  labs(title    = "Yield: distance from peer frontier vs distance from potential",
       subtitle = paste0("x = composite within-cluster frontier ratio (1 = at cluster p95) · ",
                         "y = USD-weighted achievement (actual ÷ potential GAEZ) · ",
                         "cross at within-regime medians"),
       x = "Composite frontier ratio (within cluster)",
       y = "Achievement ratio (actual / potential, USD-weighted)",
       color = "Cropland-use cluster") +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 3))) +
  theme(strip.text = element_text(size = 9),
        legend.position = "bottom")

ggsave("imgs/zone_perf_vs_achievement.png", p,
       width = 14, height = 14, dpi = 150)

cat("Zones plotted: ", nrow(zc), "\n")
cat("\nWithin-regime medians:\n")
print(regime_meds)

cat("\nCorrelation of composite_perf with achievement_ratio:\n")
cat("  Pearson : ", round(cor(zc$composite_perf,
                              zc$achievement_ratio_usd), 3), "\n")
cat("  Spearman: ", round(cor(zc$composite_perf,
                              zc$achievement_ratio_usd,
                              method = "spearman"), 3), "\n")

message("\nDone. Output: imgs/zone_perf_vs_achievement.png")
