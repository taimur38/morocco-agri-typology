# 01-download-aez.R
#
# Download GAEZ v5's official 57-class Agro-Ecological Zone raster (AEZ57)
# to the shared-data cache. Skips download if already present.
#
# Source: https://data.apps.fao.org/catalog/dataset/22c4b002-ae80-41f7-9f36-463b546378a2

library(tidyverse)

aez_dir <- "~/dev/shared-data/fao/gaez/v5/global/AEZ57"
aez_tif <- file.path(aez_dir, "GAEZ-V5.AEZ57.tif")
aez_url <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAP/GAEZ-V5.AEZ57.tif"

dir.create(aez_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(aez_tif)) {
  message("Downloading AEZ57 raster (~1 km global, expect ~hundreds of MB)...")
  download.file(aez_url, aez_tif, mode = "wb")
  message("  Saved: ", aez_tif, " (", round(file.size(aez_tif) / 1e6, 1), " MB)")
} else {
  message("AEZ57 raster already present: ", aez_tif)
}
