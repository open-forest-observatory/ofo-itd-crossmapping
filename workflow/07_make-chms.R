# Purpose: Compute CHMs for each drone mission. Two CHMs: one from the mesh DSM and one from the
# point cloud DSM (both using the point cloud DTM). Crop each one to the extent of the field plots
# (plus some buffer to account for misalignment).

library(terra)
library(sf)
library(tidyverse)
library(furrr)

# Load functions from the 'ofo' R package. The copy in "/ofo-share/utils" is intended to contain the
# latest version of the 'main' branch. If you want to make edits and test their effect here as you
# edit ofo-r, you could instead clone the 'ofo-r' repo to your own 'repos' folder and change the
# path below to match where you cloned it to.
# devtools::install("/ofo-share/repos-derek/ofo-r", quick = TRUE); library(ofo)
devtools::load_all("/ofo-share/utils/ofo-r/")

# Data paths
PHOTOGRAMMETRY_OUTPUTS_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/photogrammetry-outputs/"
# ^ within this folder, we assume there is 'dsm-mesh', 'dsm-ptcloud', and 'dtm-ptcloud' subfolders

PLOT_BOUNDS_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/plot-bounds/"

CHM_OUTPUTS_UNCROPPED_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/chms-uncropped/"
# ^ the generated chms will be saved here

ASSOC_TABLE_DIR = "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/"

# Workflow

# # Read in the association table to get the imagery IDs, for later filename lookup. But for now, we
# # are egtting the imagery IDs from the DSM filenames
# assoc = read_csv(file.path(ASSOC_TABLE_DIR, "field-plot_drone-mission_crosswalk.csv"))
# plot_ids = assoc$field_plot_id

# Get the plot IDs from the filenames in the photogrammetry outputs directory
plot_ids = list.files(file.path(PHOTOGRAMMETRY_OUTPUTS_DIR, "dtm-ptcloud"), pattern = ".tif") |>
  str_remove(".tif")

# Calculate CHMs for each plot
calc_chm = function(plot_id) {

  dsm_mesh = rast(file.path(PHOTOGRAMMETRY_OUTPUTS_DIR, "dsm-mesh", str_c(plot_id, ".tif")))
  dsm_ptcloud = rast(file.path(PHOTOGRAMMETRY_OUTPUTS_DIR, "dsm-ptcloud", str_c(plot_id, ".tif")))
  dtm_ptcloud = rast(file.path(PHOTOGRAMMETRY_OUTPUTS_DIR, "dtm-ptcloud", str_c(plot_id, ".tif")))

  # Plot bounds to buffer by
  plot_bounds = st_read(file.path(PLOT_BOUNDS_DIR, str_c(plot_id, ".gpkg")))
  
  # Buffer out the bounds for cropping the CHM. Use a broad radius because we still have to align
  # the field data to this, and there may be a lot of spatial error/misalignment
  crop_bounds = st_buffer(plot_bounds, dist = 100)

  chm_mesh = chm_from_coregistered_dsm_dtm(dsm_mesh, dtm_ptcloud, bounds = crop_bounds)
  chm_ptcloud = chm_from_coregistered_dsm_dtm(dsm_ptcloud, dtm_ptcloud, bounds = crop_bounds)

  outfilepath = file.path(CHM_OUTPUTS_DIR, "chm-mesh", str_c(plot_id, ".tif"))
  writeRaster(chm_mesh, outfilepath, overwrite = TRUE)

  outfilepath = file.path(CHM_OUTPUTS_DIR, "chm-ptcloud", str_c(plot_id, ".tif"))
  writeRaster(chm_ptcloud, outfilepath, overwrite = TRUE)

}

future::plan("multisession")
furrr::future_walk(plot_ids, calc_chm)
