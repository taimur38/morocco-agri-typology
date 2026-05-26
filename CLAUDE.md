# agri-complexity — Claude instructions

## Data layout

- **Code lives in this repo (GitHub).**
- **Data lives on the UM6P Data Playground** under `u13/agri-complexity/`
  (Taimur's namespace).
- Use the bundled `um6p-storage` skill (in `.claude/skills/`) to pull data —
  see `README.md`.
- Raw FAO/GAEZ pixel data (only needed to re-run scripts 01, 02, 04, 06, 08,
  10) is expected at `~/dev/shared-data/fao/...`, the lab convention.

## ggplot style

Every R script that produces charts loads the Growth Lab design system:

```r
source("~/dev/gl-design/skills/gl-ggplot/assets/theme_gl.R")
gl_setup()
```

After `gl_setup()`:

- **Do not override the theme per chart.** Let the theme do the work. No
  per-chart font sizes, font styling, or geom colors/sizes adjustments.
- **Highlight** with `highlight` (`#C64646`), never `"red"`. Paint twice —
  once for all data, once filtered for the highlighted set:

  ```r
  data |>
      ggplot(aes(x = x, y = y, label = text)) +
      geom_point() +
      geom_point(data = \(d) filter(d, highlighted), color = highlight, size = 3)
  ```

- **Color scales** apply automatically. For named sector palettes, use
  `scale_color_gl("hs_sectors")` or `scale_fill_gl("hs_sectors")`.
- **Save** with `save_fig("full", "filename.png")`. Named sizes: `full`
  (6.5×4"), `full_tall` (6.5×6"), `full_square` (6.5×6.5"), `major`
  (4.278×4"), `half` (3.167×3"), `half_tall` (3.167×5"). Standalone saves
  outside the framework use height=6 width=9 (or height=9 for stacked).
- **Log scale**: use `scale_x_log10()` when GDP per capita is on the x-axis.

Full docs: `~/dev/gl-design/skills/gl-ggplot/SKILL.md`
