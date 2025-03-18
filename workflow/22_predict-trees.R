# Purpose: Take a set of ITD parameter sets, and for each plot X parameter set, detect trees

#### Setup

# Load necessary packages
library(tidyverse)
library(sf)
library(furrr)
library(purrr)
library(lidR)

# Configure packages
set_lidr_threads(8)

## Data paths

# Directory in which to store the parameter definitions (as a CSV)
ITD_PARAMS_DEF_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"

# Directory in which to store the predicted trees
PREDICTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees/"

# Dir where the CHMs are stored
CHM_OUTPUTS_CROPPED_DIR <- "/ofo-share/ofo-itd-crossmapping_data/drone/chms-cropped/chm-mesh/"


## Processing constants for user to define

# ID of the previously-generated group of parameter sets to use. Set this to the same value as in
# script 21.
PARAM_GROUP_ID = "41"



## Functions

# Function to create a variable radius window function for LMF
make_win_fun <- function(a, b, c, diam_min, diam_max) {
  win_fun <- function(x) {
    win <- a + b*x + c*x^2
    win[win < diam_min] = diam_min
    win[win > diam_max] = diam_max
    return(win)
  }
  return(win_fun)
}

# Function to resample and smooth a CHM
resample_and_smooth_chm = function(chm, res, smooth_width) {
  chm_resamp <- terra::project(chm, terra::crs(chm), res = res, method = "bilinear")
  chm_smooth <- terra::focal(chm_resamp, w = matrix(1, smooth_width, smooth_width), mean, na.rm = TRUE)
  return(chm_smooth)
}

# Function to predict trees from a prepped (resampled and smoothed) CHM
predict_trees_from_chm <- function(chm, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) {
  win_fun <- make_win_fun(lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max)
  ttops <- lidR::locate_trees(chm, algorithm = lmf(ws = win_fun, shape = "circular", hmin = 5), )
  return(ttops)
}

# Function to predict trees for a single plot, using a single parameter set and based on a CHM that
# is passed in to it
predict_trees_oneplot_oneparamset = function(paramset, chm_smooth, out_folder, plot_id) {
  # Predict trees
  ttops <- predict_trees_from_chm(
    chm = chm_smooth,
    lmf_a = paramset$lmf_a,
    lmf_b = paramset$lmf_b,
    lmf_c = paramset$lmf_c,
    lmf_diam_min = paramset$lmf_diam_min,
    lmf_diam_max = paramset$lmf_diam_max
  )

  # Write predicted trees
  ttops_filename = paste0("paramset-", paramset$paramset_id, "_plot-", plot_id, ".gpkg")
  out_filepath = file.path(out_folder, ttops_filename)

  st_write(ttops, out_filepath, delete_dsn = TRUE)
}



# Function to predict trees for a single plot, using multiple parameter sets yielding one predicted tree map for every parameter set
predict_trees_oneplot_multiparamsets = function(plot_id, paramsets, out_folder) {

  cat("Predicting trees across all paramsets for plot ID", plot_id)  # "(", i, "of", length(plot_ids), ")\n")

  # Load the CHM
  chm_file <- file.path(CHM_OUTPUTS_CROPPED_DIR, paste0(plot_id, ".tif"))
  chm <- terra::rast(chm_file)

  chm_smooth = resample_and_smooth_chm(chm, res = 0.25, smooth_width = 3)

  # TODO: If we end up tuning the CHM res and smoothing params, then here, we could pre-prep the various resampled/smoothed versions of the CHM, so they don't have to be recomputed for every ITD set. But the compute for resampling seems like very low overhead.

  # Turn the paramset dataframe rows into a list (each with one row) so it can be used to parallelize
  paramset_list = split(paramsets, seq(nrow(paramsets)))

  # Predict trees for each parameter set
  walk(paramset_list, predict_trees_oneplot_oneparamset, chm_smooth = chm_smooth, out_folder = out_folder, plot_id = plot_id)

}



#### Workflow

# Create the output directory
out_folder <- file.path(PREDICTED_TREES_DIR, paste0("paramgroup-", PARAM_GROUP_ID))
if (!dir.exists(out_folder)) dir.create(out_folder, recursive = TRUE)

# Load the list of plots (i.e., all that have a CHM produced for them)
chm_files <- list.files(CHM_OUTPUTS_CROPPED_DIR, pattern = "tif$", recursive = TRUE, full.names = FALSE)
plot_ids <- substr(chm_files, 1, nchar(chm_files) - 4)

# Load the parameter set defs
filename = paste0("itd-paramsets_", PARAM_GROUP_ID, ".csv")
paramsets = read_csv(file.path(ITD_PARAMS_DEF_DIR, filename))

# Predict trees for each plot X parameter set, in parallel across plots (takes about 10 min for 100 paramsets)
future::plan("multisession")
future_walk(plot_ids, predict_trees_oneplot_multiparamsets, paramsets = paramsets, out_folder = out_folder, .progress = TRUE)
