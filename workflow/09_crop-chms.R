# Purpose: Take all the CHMs and the *aligned* plot bounds, and crop the CHMs to the plot bounds.
# This will resulg in CHMs that are only as large as they need to be for the purposes of tree
# detection and evaluation.

library(terra)
library(sf)
library(furrr)
library(stringr)

# Load functions from the 'ofo' R package. The copy in "/ofo-share/utils" is intended to contain the
# latest version of the 'main' branch. If you want to make edits and test their effect here as you
# edit ofo-r, you could instead clone the 'ofo-r' repo to your own 'repos' folder and change the
# path below to match where you cloned it to.
# devtools::install("/ofo-share/repos-derek/ofo-r", quick = TRUE); library(ofo)
devtools::load_all("/ofo-share/utils/ofo-r/")

PLOT_BOUNDS_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/plot-bounds/"

CHM_UNCROPPED_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/chms-uncropped/"
CHM_CROPPED_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/chms-cropped/"

ASSOC_TABLE_DIR = "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/"

# Workflow

# # Read in the association table to get the imagery IDs, for later filename lookup. But for now, we
# # are egtting the imagery IDs from the DSM filenames
# assoc = read_csv(file.path(ASSOC_TABLE_DIR, "field-plot_drone-mission_crosswalk.csv"))
# plot_ids = assoc$field_plot_id

# Get the plot IDs from the filenames in the CHM outputs directory
plot_ids = list.files(file.path(CHM_UNCROPPED_DIR, "chm-mesh"), pattern = ".tif") |>
  str_remove(".tif")

# Crop the CHMs for each plot
calc_chm = function(plot_id) {

  chm_mesh = rast(file.path(CHM_UNCROPPED_DIR, "chm-mesh", str_c(plot_id, ".tif")))
  chm_ptcloud = rast(file.path(CHM_UNCROPPED_DIR, "chm-ptcloud", str_c(plot_id, ".tif")))

  # Plot bounds to buffer by
  plot_bounds = st_read(file.path(PLOT_BOUNDS_DIR, str_c(plot_id, ".gpkg")))

  # Buffer out the bounds for cropping the CHM. Use a broad radius because we still have to align
  # the field data to this, and there may be a lot of spatial error/misalignment
  crop_bounds = st_buffer(plot_bounds, dist = 20)

  # Crop the CHMs
  chm_mesh = crop(chm_mesh, crop_bounds)
  chm_ptcloud = crop(chm_ptcloud, crop_bounds)

  # Mask them
  chm_mesh = mask(chm_mesh, crop_bounds)
  chm_ptcloud = mask(chm_ptcloud, crop_bounds)

  outfilepath = file.path(CHM_CROPPED_DIR, "chm-mesh", str_c(plot_id, ".tif"))
  writeRaster(chm_mesh, outfilepath, overwrite = TRUE)

  outfilepath = file.path(CHM_CROPPED_DIR, "chm-ptcloud", str_c(plot_id, ".tif"))
  writeRaster(chm_ptcloud, outfilepath, overwrite = TRUE)

}

future::plan("multisession")
furrr::future_walk(plot_ids, calc_chm)
