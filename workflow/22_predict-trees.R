# Purpose: Take a set of ITD parameter sets, and for each plot X parameter set, detect trees

#### Setup

# Load necessary packages
library(tidyverse)
library(sf)
library(furrr)
library(lidR)

# Configure packages
set_lidr_threads(0)

## Data paths

# Directory in which to store the parameter definitions (as a CSV)
ITD_PARAMS_DEF_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"

# Directory in which to store the predicted trees
PREDICTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees/"

# Dir where the CHMs are stored
CHM_OUTPUTS_CROPPED_DIR <- "/ofo-share/ofo-itd-crossmapping_data/drone/chms-cropped/chm-mesh/"


## Processing constants for user to define

# ID of the previously-generated set of parameter sets to use
PARAM_GROUP_ID = "01"



## Functions

# Function to create a variable radius window function for LMF
make_win_fun <- function(a, b, c, rad_min, rad_max) {
  win_fun <- function(x) {
    win <- a + b * x + c * x^2
    win[win < rad_min] <- rad_min
    win[win > rad_max] <- rad_max
    return(win)
  }
  return(win_fun)
}

# Function to resample and smooth a CHM
resample_and_smooth_chm = function(chm, chm_res, chm_smooth_width) {
  chm_resamp <- terra::project(chm, terra::crs(chm), res = chm_res, method = "bilinear")
  chm_smooth <- terra::focal(chm_resamp, w = matrix(1, chm_smooth_width, chm_smooth_width), mean, na.rm = TRUE)
  return(chm_smooth)
}

# Function to predict trees from a prepped (resampled and smoothed) CHM
predict_trees_from_chm <- function(chm, itd_a, itd_b, itd_c, rad_min, rad_max) {
  win_fun <- make_win_fun(itd_a, itd_b, itd_c, rad_min, rad_max)
  ttops <- lidR::locate_trees(chm, algorithm = lmf(ws = win_fun, shape = "circular", hmin = 5), )
  return(ttops)
}

# Function to predict trees for a single plot, using multiple parameter sets yielding one predicted tree map for every parameter set
predict_trees_oneplot_multiparamsets = function(plot_id, paramsets, param_group_id) {
  # Load the CHM
  chm_file <- file.path(CHM_OUTPUTS_CROPPED_DIR, paste0(plot_id, ".tif"))
  chm <- terra::rast(chm_file)

  chm_smooth = resample_and_smooth_chm(chm, chm_res = 0.25, chm_smooth_width = 3)

  # TODO: If we end up tuning the CHM res and smoothing params, then here, we could pre-prep the various resampled/smoothed versions of the CHM, so they don't have to be recomputed for every ITD set. But the compute for resampling seems like very low overhead.

  # Loop through each parameter set

  for (j in 1:nrow(paramsets)) {
    paramset <- paramsets[j, ]

    # Predict trees
    ttops <- predict_trees_from_chm(
      chm = chm_smooth,
      itd_a = paramset$itd_a,
      itd_b = paramset$itd_b,
      itd_c = paramset$itd_c,
      rad_min = paramset$rad_min,
      rad_max = paramset$rad_max)


    # Write predicted trees
    ttops_filename = paste0("paramgroup-", param_group_id, "_paramset-", paramset$paramset_id, "_plot-", plot_id, ".gpkg")
    out_filepath <- paste0(PREDICTED_TREES_DIR, ttops_filename)
    st_write(ttops, out_filepath, delete_dsn = TRUE)
  }
}


#### Workflow

# Load the parameter set defs
filename = paste0("itd-paramsets_", PARAM_GROUP_ID, ".csv")
paramsets = read_csv(file.path(ITD_PARAMS_DEF_DIR, filename))

# Load the list of plots (i.e., all that have a CHM produced for them)
chm_files <- list.files(CHM_OUTPUTS_CROPPED_DIR, pattern = "tif$", recursive = TRUE, full.names = FALSE)
plot_ids <- substr(chm_files, 1, nchar(chm_files) - 4)


future::plan("multisession")

future_walk(plot_ids, predict_trees_oneplot_multiparamsets, paramsets = paramsets, param_group_id = PARAM_GROUP_ID)
