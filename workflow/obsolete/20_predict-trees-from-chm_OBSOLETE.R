# Purpose: For a given set of parameters (specified as constants below), detect trees from a CHM and
# write the detected treetops to a .gpkg file

#### Setup

## Load packages
library(tidyverse)
library(lidR)
library(terra)
library(sf)
library(stringr)

## Set constants

# File paths
CHM_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/chms-cropped/chm-mesh"
PREDICTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees"

## Constants for ITD

# CHM preprocessing ITD constants
CHM_RES = 0.25
CHM_SMOOTH = 3

# ITD window size coefficients (these are coefs to the formula: win = a + b * h + c * h^2). Setting
# b and c to 0 is equivalent to a fixed window size.
ITD_A = 0
ITD_B = 0.11
ITD_C = 0

# What is the ID of the set of parameters above?
ITD_PARAMS_ID = "0001"

#### Functions
# These are functions that are specific to this workflow and therefore don't make sense to include
# in the ofo package. These functions could be moved so a separate R file and sourced to keep this
# workflow script cleaner.

# Define an ITD variable radius window function based on the coefficients a, b, and c defined above
# to the formula: win = a + b * h + c * h^2. Also define a minimum and maximum search radius.
# Currently these latter two are hard-coded but could be added to the search space in the future.
make_win_fun = function(a, b, c, min_ht = 2, max_ht = 50, min_rad = 1, max_rad = 10) {
  win_fun = function(x) {
    win = a + b * x + c * x^2
    win[win < min_rad] = min_rad
    win[win > max_rad] = max_rad
    return(win)
  }

  return(win_fun)
}

# For a provided plot ID, and set of ITD parameter constants, detect trees from a CHM and
# return the result as a sf object.
predict_trees_from_chm = function(chm,
                                  chm_res,
                                  chm_smooth,
                                  itd_a,
                                  itd_b,
                                  itd_c,
                                  itd_params_id,
                                  datadir = datadir) {

  # Resample it to the specified res
  chm_resamp = terra::project(chm, terra::crs(chm), res = chm_res, method = "bilinear")

  # Smooth it with the specified smoothing window
  chm_smooth = terra::focal(chm_resamp, w = matrix(1, chm_smooth, chm_smooth), mean)

  # Detect treetops from it with the specified window size parameters
  win_fun = make_win_fun(itd_a, itd_b, itd_c)
  ttops = lidR::locate_trees(chm_smooth, algorithm = lmf(ws = win_fun, shape = "circular", hmin = 3))


}

# This function is a wrapper for the more generalized tree detection function that addes the
# functionality specifie to the crossmapping project, like interfacing to our file storage system
# and reading and writing files in the correct format and directory structure.
itd_chm_to_ttops_gpkg = function(plot_id,
                                 chm_dir,
                                 chm_res,
                                 chm_smooth,
                                 itd_a,
                                 itd_b,
                                 itd_c,
                                 itd_params_id,
                                 datadir = datadir) {

  # Get the CHM filename based on the plot ID
  chm_file = file.path(chm_dir, str_c(plot_id, ".tif"))

  # Load it
  chm = rast(chm_file)

  # Detect trees
  ttops = predict_trees_from_chm(chm = chm,
                      chm_res = chm_res,
                      chm_smooth = chm_smooth,
                      itd_a = itd_a,
                      itd_b = itd_b,
                      itd_c = itd_c,
                      itd_params_id = itd_params_id,
                      datadir = datadir)

  # Write the treetops as a .gpkg
  out_filepath = file.path(PREDICTED_TREES_DIR, str_c("params-", itd_params_id, "_plot-", plot_id, ".gpkg"))
  st_write(ttops, out_filepath, delete_dsn = TRUE)

}


#### Workflow

# Get list of plot IDs (based on filenames of the CHMs)
chm_files = list.files(CHM_DIR, pattern = "tif$")
plot_ids = str_remove(chm_files, ".tif")

# Detect trees for each plot. This could be parallelized with furrr::future_walk.
walk(plot_ids, itd_chm_to_ttops_gpkg,
     chm_dir = CHM_DIR,
     chm_res = CHM_RES,
     chm_smooth = CHM_SMOOTH,
     itd_a = ITD_A,
     itd_b = ITD_B,
     itd_c = ITD_C,
     itd_params_id = ITD_PARAMS_ID,
     datadir = datadir)
