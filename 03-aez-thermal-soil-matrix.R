# 03-aez-thermal-soil-matrix.R
#
# Decomposes Africa's land area into a thermal-regime × soil-status matrix.
# Reads the pixel counts already produced by 02-africa-base-map.R.

library(tidyverse)

source("~/dev/gl-design/skills/gl-ggplot/assets/theme_gl.R")
gl_setup()

# ── Inputs ───────────────────────────────────────────────────────────────
aez_counts <- read_csv("generated/africa_aez_pixel_counts.csv",
                       show_col_types = FALSE)

# Soil-status dimension derived from AEZ57 class numbering:
#   odd  in 1..47  → "No soil/terrain limits"
#   even in 2..48  → "With soil/terrain limits"
#   49, 50         → "Severe limits"
#   51..57         → "Climate / land extreme" (desert, irrigated, boreal, built-up, water)
aez_counts <- aez_counts |>
  mutate(
    soil_status = case_when(
      value %in% seq(1, 47, by = 2) ~ "No soil/terrain limits",
      value %in% seq(2, 48, by = 2) ~ "With soil/terrain limits",
      value %in% c(49, 50)          ~ "Severe limits",
      value %in% 51:57              ~ "Climate / land extreme",
      TRUE                          ~ NA_character_
    )
  )

regime_levels <- c("Tropics lowland", "Tropics highland",
                   "Sub-tropics warm", "Sub-tropics mod. cool",
                   "Sub-tropics cool", "Temperate moderate",
                   "Temperate cool", "Cold (no permafrost)",
                   "Severe terrain/soil", "Irrigated / hydromorphic",
                   "Desert/Arid", "Boreal/Arctic", "Built-up / water")

soil_levels <- c("No soil/terrain limits",
                 "With soil/terrain limits",
                 "Severe limits",
                 "Climate / land extreme")

# ── Build the matrix ─────────────────────────────────────────────────────
mat <- aez_counts |>
  group_by(regime, soil_status) |>
  summarise(share = sum(share), .groups = "drop") |>
  mutate(
    regime      = factor(regime, levels = regime_levels),
    soil_status = factor(soil_status, levels = soil_levels)
  )

# Pad with empty cells for every combination so the grid renders fully
grid <- expand_grid(
  regime      = factor(regime_levels, levels = regime_levels),
  soil_status = factor(soil_levels,   levels = soil_levels)
) |>
  left_join(mat, by = c("regime", "soil_status"))

# ── Marginals (column totals and row totals) ─────────────────────────────
col_totals <- mat |> group_by(regime) |> summarise(share = sum(share))
row_totals <- mat |> group_by(soil_status) |> summarise(share = sum(share))

# ── Heatmap ──────────────────────────────────────────────────────────────
fmt_pct <- function(x) ifelse(is.na(x) | x == 0, "",
                              scales::percent(x, accuracy = 0.1))

p <- grid |>
  ggplot(aes(x = regime, y = fct_rev(soil_status))) +
  geom_tile(aes(fill = share), color = "white", linewidth = 0.6) +
  geom_text(aes(label = fmt_pct(share),
                color = ifelse(!is.na(share) & share > 0.12, "white", "black")),
            size = 3) +
  scale_color_identity() +
  scale_fill_viridis_c(
    option   = "mako", direction = -1, na.value = "grey95",
    labels   = scales::percent_format(accuracy = 1),
    breaks   = c(0.01, 0.05, 0.10, 0.20, 0.40),
    trans    = "sqrt"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Africa land area by AEZ thermal regime × soil/terrain status",
    subtitle = "GAEZ v5 AEZ57, % of continental land area (~1 km pixels, aggregated to 5 km)",
    x = NULL, y = NULL, fill = "% of Africa"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 0),
    legend.position = "right",
    panel.grid = element_blank()
  )

ggsave("imgs/africa_aez_thermal_soil_matrix.png", p,
       width = 13, height = 5.5, dpi = 150)

# ── Companion bars: column and row marginals ────────────────────────────
p_col <- col_totals |>
  mutate(regime = factor(regime, levels = regime_levels)) |>
  ggplot(aes(x = regime, y = share)) +
  geom_col(fill = "grey40") +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
            vjust = -0.3, size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "By thermal regime", x = NULL, y = "% of Africa") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_row <- row_totals |>
  mutate(soil_status = factor(soil_status, levels = soil_levels)) |>
  ggplot(aes(x = fct_rev(soil_status), y = share)) +
  geom_col(fill = "grey40") +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
            vjust = -0.3, size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  labs(title = "By soil/terrain status", x = NULL, y = "% of Africa")

ggsave("imgs/africa_aez_marginals_regime.png", p_col,
       width = 10, height = 4.5, dpi = 150)
ggsave("imgs/africa_aez_marginals_soilstatus.png", p_row,
       width = 7, height = 4, dpi = 150)

# Save the underlying table
write_csv(mat, "generated/africa_aez_thermal_soil_matrix.csv")

message("Done. Saved matrix + marginals to imgs/, data to generated/.")
