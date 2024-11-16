# Purpose: For a set of detected tree maps (one per .gpkg file), compare the detections against the observed trees from the field plot and compute accuracy metrics.

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

## Set constants

# File paths
PREDICTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees/"
OBSERVED_ALIGNED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/trees/"
OBSERVED_ALIGNED_PLOTBOUNDS_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/plot-bounds/"

# Processing constants for user to define
FOC_PARAMGROUPS = c("01")

#### Functions


#### Workflow

# Get list of predicted tree maps to evaluate
pred_tree_files = list.files(PREDICTED_TREES_DIR, pattern = "gpkg$")

# Make a data frame with one row per predicted tree map that also has columns for plot ID, parameter
# group, and parameter set.
preds_to_eval = data.frame(pred_tree_file = pred_tree_files) |>
  mutate(paramgroup = str_sub(pred_tree_file, 12, 13),
         paramset = str_sub(pred_tree_file, 24, 29),
         plot_id = str_sub(pred_tree_file, 36, 39)) |>
  mutate()

# Filter to the focal parameter groups
preds_to_eval = preds_to_eval |>
  filter(paramgroup %in% FOC_PARAMGROUPS)

# TODO: see which have already been run (based on existence of a results file?) and skip them

# Get a list of unique plot IDs
plot_ids = unique(preds_to_eval$plot_id)

# Loop through each plot ID for tree map evaluation
for (plot_id_foc in plot_ids) {
  # Load the observed tree map and plot bounds
  obs_trees = st_read(file.path(OBSERVED_ALIGNED_TREES_DIR, paste0(plot_id, ".gpkg")))
  obs_bounds = st_read(file.path(OBSERVED_ALIGNED_PLOTBOUNDS_DIR, paste0(plot_id, ".gpkg")))

  preds_to_eval_focplot = preds_to_eval |>
    filter(plot_id_foc == plot_id)
  
  # Loop through each parameter set (i.e. predicted tree map) for the focal plot ID and compute tree
  # detection accuracy
  for (i in 1:nrow(preds_to_eval_focplot)) {

    # Filter to the focal plot ID and parameter group
    preds_to_eval_focplot_focparamset = preds_to_eval_focplot[i, ]
    
    ## RESUME HERE
    
    # Load the predicted tree map
    pred_trees = st_read(file.path(PREDICTED_TREES_DIR, d_foc_paramset$pred_tree_file))
    
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
    
    # Prepare the observed and predicted tree maps for matching
    obs_trees = ofo::prep_obs_map(obs_trees, obs_bounds, edge_buffer = 5)
    pred_trees = ofo::prep_pred_map(pred_trees, obs_bounds, edge_buffer = 5)
    
    # Match the observed trees to the predicted trees
    obs_trees_matched = ofo::match_obs_to_pred_mee(obs_trees,
                                              pred_trees,
                                              search_distance_fun_intercept = 1,
                                              search_distance_fun_slope = 0.1,
                                              search_height_proportion = 0.5)
    
    # Compute the match statistics
    match_stats = ofo::compute_match_stats(pred_trees,
                                      obs_trees_matched,
                                      min_height = 10)
    
    # Print the match statistics
    print(match_stats)



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
