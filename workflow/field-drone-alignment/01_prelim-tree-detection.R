# Purpose: Perform preliminary tree detection on CHM to create a preliminary drone-detected tree
# dataset for determining how to align the field data to the drone data

library(sf)
library(tidyverse)
library(terra)
library(furrr)

# Load functions from the 'ofo' R package. The copy in "/ofo-share/utils" is intended to contain the
# latest version of the 'main' branch. If you want to make edits and test their effect here as you
# edit ofo-r, you could instead clone the 'ofo-r' repo to your own 'repos' folder and change the
# path below to match where you cloned it to.
# devtools::install("/ofo-share/repos-derek/ofo-r")
devtools::load_all("/ofo-share/utils/ofo-r")

# Data paths
CHM_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/chms/"
# ^ within this folder, we assume there are 'chm-mesh' and 'chm-ptcloud' subfolders

PRELIM_DETECTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/detected-trees_prelim/"

# Determine which plots need trees detected, based on the CHMs that have been generated
plot_ids = list.files(file.path(CHM_DIR, "chm-mesh"), pattern = ".tif") |>
  str_remove(".tif")


# Define the window size function for tree detection
ws = function(x) {
  win = x * 0.11 + 0
  win[win < 0.5] = 0.5
  win[win > 100] = 100
  return(win)
}


detect_and_write = function(plot_id) {

  chm = rast(file.path(CHM_DIR, "chm-mesh", str_c(plot_id, ".tif")))

  ttops = detect_trees2(chm, ws)

  out_filepath = file.path(PRELIM_DETECTED_TREES_DIR, str_c(plot_id, ".gpkg"))
  sf::st_write(ttops, out_filepath, delete_dsn = TRUE)
}

walk(plot_ids, detect_and_write)
