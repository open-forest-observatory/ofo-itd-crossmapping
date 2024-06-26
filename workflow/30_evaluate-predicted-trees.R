# Purpose: For a given set of parameters (specified as constants below), detect trees from a CHM and
# write the detected treetops to a .gpkg file

#### Setup

## Load packages
library(tidyverse)
library(lidR)
library(sf)

# Load functions from the 'ofo' R package. The copy in "/ofo-share/utils" is intended to contain the
# latest version of the 'main' branch. If you want to make edits and test their effect here as you
# edit ofo-r, you could instead clone the 'ofo-r' repo to your own 'repos' folder and change the
# path below to match where you cloned it to.
# devtools::install("/ofo-share/repos-derek/ofo-r", quick = TRUE); library(ofo)
# devtools::load_all("/ofo-share/repos-derek/ofo-r")
devtools::load_all("/ofo-share/utils/ofo-r/")

## Set data directory
datadir <- readLines(file.path("datadirs", "js2-itd-crossmapping.txt"), n = 1)

## Set constants

# File paths
CHM_DIR = file.path(datadir, "drone", "chms-cropped", "chm-mesh") # The folder containing the CHMs to use
PREDICTED_TREES_DIR = file.path(datadir, "drone", "predicted-trees")
OBSERVED_ALIGNED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/trees/"
OBSERVED_ALIGNED_PLOTBOUNDS_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/plot-bounds/"


#### Functions


#### Workflow

# Get list of predicted tree maps to evaluate
pred_tree_files = list.files(PREDICTED_TREES_DIR, pattern = "gpkg$")

# TEMPORARY: Select a single predicted tree file to evaluate. TODO: Make this into a function that
# can be applied in parallel over all predicted tree files.
pred_tree_file = pred_tree_files[1]

# Load the predicted tree map
pred_trees = st_read(file.path(PREDICTED_TREES_DIR, pred_tree_file))

# Load the corresponding observed tree map and plot bounds
plot_id = str_sub(pred_tree_file, 18, 21)
obs_trees = st_read(file.path(OBSERVED_ALIGNED_TREES_DIR, paste0(plot_id, ".gpkg")))
obs_bounds = st_read(file.path(OBSERVED_ALIGNED_PLOTBOUNDS_DIR, paste0(plot_id, ".gpkg")))

# Add the required x, y, and z columns to the predicted tree map
pred_trees = st_transform(pred_trees, st_crs(obs_trees))
coords_pred = st_coordinates(pred_trees)
pred_trees$x = coords_pred[, "X"]
pred_trees$y = coords_pred[, "Y"]
pred_trees$z = pred_trees$Z

# Visualize the two tree maps
ofo::vis2(pred = pred_trees,
          obs = obs_trees,
          coords_arbitrary = TRUE)

obs_trees = ofo::prep_obs_map(obs_trees, obs_bounds, edge_buffer = 5)
pred_trees = ofo::prep_pred_map(pred_trees, obs_bounds, edge_buffer = 5)

obs_trees_matched = ofo::match_obs_to_pred_mee(obs_trees,
                                          pred_trees,
                                          search_distance_fun_intercept = 1,
                                          search_distance_fun_slope = 0.1,
                                          search_height_proportion = 0.5)

match_stats = ofo::compute_match_stats(pred_trees,
                                  obs_trees_matched,
                                  min_height = 10)

match_stats
