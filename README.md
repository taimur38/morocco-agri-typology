# agri-complexity

Country × AEZ typology for African agriculture. Uses FAO GAEZ v5 pixel data
to build country-by-agro-ecological-zone features (yield gaps, value/ha,
crop mix), clusters zones into recurring "zone types," and benchmarks
realized yield against the GAEZ potential.

Companion project notes:
[`project_africa_ag_typology.md`](https://github.com/taimur38/agri-complexity/wiki)
(Obsidian vault, internal).

## Repo layout

```
01–10  build scripts   reads raw FAO/GAEZ → writes generated/
11–13  cluster zones   k-means + UMAP on zone features
14–16  zone perf       realized vs. potential yield
tool/                  small d3 web viewer (reads tool/data/*.geojson|json)
imgs/                  saved chart PNGs (tracked — used as PR previews)
data/                  small reference CSVs (crop dictionary, FAO crosswalk)
```

## Code in git, data on the cluster

Anything in `generated/` or `tool/data/` is excluded from git. It lives on
the **UM6P Data Playground** under `u13/agri-complexity/` (Taimur's
namespace). Pull it with the bundled `um6p-storage` Claude skill:

```bash
# one-time, gets a bearer token
python .claude/skills/um6p-storage/dp_storage.py login --email you@um6p.ma

# pull all the generated outputs
mkdir -p generated tool/data
for f in $(python .claude/skills/um6p-storage/dp_storage.py ls u13/agri-complexity/generated \
           | awk '$1 ~ /^[0-9]+$/ {print $2}'); do
  python .claude/skills/um6p-storage/dp_storage.py get "$f" generated/
done
for f in $(python .claude/skills/um6p-storage/dp_storage.py ls u13/agri-complexity/tool-data \
           | awk '$1 ~ /^[0-9]+$/ {print $2}'); do
  python .claude/skills/um6p-storage/dp_storage.py get "$f" tool/data/
done
```

Or, inside Claude Code, just ask: *"pull the agri-complexity data from the
playground."* The skill knows the namespace.

After pushing changes that update outputs:

```bash
python .claude/skills/um6p-storage/dp_storage.py put generated/foo.parquet agri-complexity/generated
```

(only Taimur — `u13` — can write into `u13/`; other team members publish
under their own namespace and we sync.)

## Bootstrapping from scratch

A new collaborator needs only:

1. `git clone git@github.com:taimur38/agri-complexity.git`
2. R packages: `tidyverse`, `arrow`, `sf`, `terra`, `tidymodels`, `umap`
   (see scripts for the full list)
3. Run the pull commands above to get `generated/` and `tool/data/`

That is enough to re-run scripts **03, 05, 07, 09, 11–16** (the analysis +
viz layer). It is enough to open `tool/index.html` in a browser.

To re-run the **build layer** (scripts 01, 02, 04, 06, 08, 10) you also
need raw FAO data at `~/dev/shared-data/fao/`:

| Script | Path |
| --- | --- |
| 01, 02, 04 | `fao/gaez/v5/global/AEZ57/` |
| 04 | `fao/gaez/v5/global/{LR-LCC,SQX,RES06-VAL,RES06-PRD}/` |
| 06 | `fao/gaez/v5/global/{RES07-QGA,RES06-PRD}/` |
| 08 | `fao/{ag_prod_data.parquet, ag_value_data.parquet, bulk/Prices_E_All_Data_(Normalized).csv}` |
| 10 | `fao/gaez/v5/global/{RES06-HAR,RES06-PRD}/` |

`01-download-aez.R` populates the AEZ57 cache from the FAO GISMGR API.

## Data manifest

`u13/agri-complexity/generated/`:

| file | what it is |
| --- | --- |
| `africa_aez_pixel_counts.csv` | AEZ57 class × Africa pixel counts |
| `africa_aez_thermal_soil_matrix.csv` | thermal regime × soil status share matrix |
| `country_aez_zones.parquet` | zone_id ↔ country/regime lookup |
| `country_aez_zone_raster.tif` | rasterized zone IDs for Africa |
| `country_aez_features_wide.parquet` | one row per zone, scalar features |
| `country_aez_area_share_wide.parquet` | zone × crop area share |
| `country_aez_har_long.parquet` | zone × crop, area + production (long) |
| `country_aez_yield_wide.parquet` | zone × crop yield, t/ha |
| `country_aez_crop_production.parquet` | zone × crop production, 1000 t |
| `country_aez_gap_long{,_usd}.parquet` | zone × crop actual + GAEZ gap |
| `country_aez_gap_wide{,_usd}.parquet` | zone-level rollup |
| `country_aez_gap_by_group{,_usd}.parquet` | gaps aggregated by crop group |
| `country_aez_main_crops.csv` | per-country flagged "main" crop set |
| `country_aez_zone_clusters.parquet` | k-means cluster assignment per zone |
| `country_aez_zone_clusters_labeled.csv` | + human-readable cluster labels |
| `country_aez_cluster_signatures.csv` | feature means per cluster |
| `country_aez_zone_perf.csv` | realized vs. potential yield per zone |
| `crop_prices_usd_per_tonne.csv` | producer prices, FAO bulk → USD/tonne |
| `country_crop_area_share.csv` | country crop area composition |
| `country_crop_export_share.csv` | country crop export composition |
| `country_staple_export_top1.csv` | top staple per country |
| `crop_captions_raw.csv` | crop name dictionary scrape |

`u13/agri-complexity/tool-data/`:

| file | what it is |
| --- | --- |
| `africa.geojson` | Africa country boundaries (simplified) |
| `zones.geojson` | dissolved zone polygons for the viewer |
| `zone_details.json` | per-zone payload consumed by `tool/app.js` |
| `meta.json` | tool build metadata |

## ggplot style

See `CLAUDE.md`. The repo uses the Growth Lab design system; do not
override the theme per chart.
