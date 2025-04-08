# Purpose: For a set of predicted tree maps (one predicted set per .gpkg file), compare the
# detections against the observed trees from the corresponding field plot and compute accuracy
# metrics.

#### Setup

## Load packages
library(tidyverse)
library(sf)
library(furrr)

# Load the 'ofo' R package
library(ofo)

## Configure packages

# Don't print dplyr messages
options(dpyr.inform = FALSE)

## Set constants

# File paths
PREDICTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees/"
OBSERVED_ALIGNED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/trees/"
OBSERVED_ALIGNED_PLOTBOUNDS_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/plot-bounds/"
MATCH_STATS_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-tree-evals/"

# Processing constants for user to define

# Which group of parameter sets to evaluate? Set to the same value as in the previous script (22).
FOC_PARAMGROUP = "41"
EVAL_OVERSTORY_ONLY = FALSE

#### Functions

# Compare a predicted tree map (specified by the file name) to the observed trees from the same plot
eval_preds = function(pred_to_eval, obs_trees, obs_bounds, predicted_trees_dir) {

  # Prepare to get the seconds elapsed during the evaluation
  start_time = Sys.time()

  # Load the predicted tree map
  pred_trees = st_read(file.path(predicted_trees_dir, pred_to_eval$pred_tree_file), quiet = TRUE)

  # Add the required x, y, and z columns to the predicted tree map
  pred_trees = st_transform(pred_trees, st_crs(obs_trees))
  coords_pred = st_coordinates(pred_trees)
  pred_trees$x = coords_pred[, "X"]
  pred_trees$y = coords_pred[, "Y"]
  pred_trees$z = pred_trees$Z

  # Prepare the predicted tree map for matching
  pred_trees = prep_pred_map(pred_trees, obs_bounds, edge_buffer = 5)

  #! If there are no trees in the predicted tree map, return a recall and precision of 0

  if(nrow(pred_trees) > 0) {
    # Match the observed trees to the predicted trees
    obs_trees_matched = match_obs_to_pred_mee(obs_trees,
                                              pred_trees,
                                              search_distance_fun_intercept = 1,
                                              search_distance_fun_slope = 0.1,
                                              search_height_proportion = 0.5)

    # Compute the match statistics
    match_stats = compute_match_stats(pred_trees,
                                      obs_trees_matched,
                                      min_height = 10)
  } else {
    # If there are no trees in the predicted tree map, return a recall and precision of 0
    match_stats = data.frame(recall = 0, precision = 0, f_score = 0)
  }

  # Add the predicted tree file, parameter group, parameter set, and plot ID to the match statistics
  match_stats = cbind(match_stats, pred_to_eval)

  # Get the seconds elapsed during the evaluation
  end_time = Sys.time()
  elapsed_time = end_time - start_time
  elapsed_secs = as.numeric(elapsed_time, units = "secs")

  # Add the evaluation time to the match statistics
  match_stats$eval_time = elapsed_secs

  return(match_stats)
}


# Function to see if any trees are taller than the focal tree
any_taller = function(i, observed_trees) {

  focal_height = observed_trees[i,]$obs_tree_height
  other_trees = 1:nrow(observed_trees) %>% setdiff(i)

  other_trees_sp = observed_trees[other_trees,]
  other_trees_sp$dist = st_distance(observed_trees[i,], other_trees_sp) %>% as.vector

  other_trees_sp$heightdiff = other_trees_sp$obs_tree_height - focal_height
  other_trees_sp$dist_thresh = other_trees_sp$heightdiff * 0.1 + 1
  other_trees_sp$focal_is_under = ((focal_height < other_trees_sp$obs_tree_height) & (other_trees_sp$dist < other_trees_sp$dist_thresh))

  focal_is_under = any(other_trees_sp$focal_is_under)

  return(focal_is_under)

}



#### Workflow

# Get list of predicted tree maps to evaluate
pred_trees_dir = file.path(PREDICTED_TREES_DIR, paste0("paramgroup-", FOC_PARAMGROUP))
pred_tree_files = list.files(pred_trees_dir, pattern = "gpkg$")

# Make a data frame with one row per predicted tree map that also has columns for plot ID, parameter
# group, and parameter set.
preds_to_eval = data.frame(pred_tree_file = pred_tree_files) |>
  mutate(paramset_id = str_sub(pred_tree_file, 10, 15),
         plot_id = str_sub(pred_tree_file, 22, 25))

# TODO: see which have already been run (based on existence of a results file?) and skip them?

# Prepare to parallelize (happens inside the loop below)
future::plan(future::multisession)

# Get a list of unique plot IDs
plot_ids = unique(preds_to_eval$plot_id)

# Loop through each plot ID for tree map evaluation
match_stats = data.frame()
for (i in 1:length(plot_ids)) {

  plot_id_foc = plot_ids[i]

  cat("Evaluating predicted tree maps across all paramsets for plot ID", plot_id_foc, "(", i, "of", length(plot_ids), ")\n")

  # Load the observed tree map and plot bounds
  obs_trees = st_read(file.path(OBSERVED_ALIGNED_TREES_DIR, paste0(plot_id_foc, ".gpkg")), quiet = TRUE)
  obs_bounds = st_read(file.path(OBSERVED_ALIGNED_PLOTBOUNDS_DIR, paste0(plot_id_foc, ".gpkg")), quiet = TRUE)

  preds_to_eval_focplot = preds_to_eval |>
    filter(plot_id == plot_id_foc)

  # Prepare the observed tree map for matching
  obs_trees = prep_obs_map(obs_trees, obs_bounds, edge_buffer = 5)

  # Optionally, filter the observed trees to include only overstory trees (presumed visible from overhead)
  if(EVAL_OVERSTORY_ONLY == TRUE) {
    obs_trees$under_neighbor = map_lgl(1:nrow(obs_trees), any_taller, observed_trees = obs_trees)
    obs_trees = obs_trees |>
      filter(!under_neighbor)
  }


  # Convert preds_to_eval dataframe to a list of dataframes, each list elemenent with one row (for
  # passing to the parallelized function)
  preds_to_eval_list = split(preds_to_eval_focplot, seq(nrow(preds_to_eval_focplot)))

  # Run the evaluation function in parallel across all parameter sets for the current focal plot
  match_stats_focplot_list = future_map(preds_to_eval_list, eval_preds, obs_trees, obs_bounds, pred_trees_dir, .options=furrr_options(chunk_size = 5))
  match_stats_focplot = bind_rows(match_stats_focplot_list)

  # Append the match statistics for the focal plot to the overall match statistics
  match_stats = bind_rows(match_stats, match_stats_focplot)

}

# Save the match statistics
filename = paste0("match-stats_paramgroup-", paste(FOC_PARAMGROUP, collapse = "-"), ".csv")
write_csv(match_stats, file.path(MATCH_STATS_DIR, filename))
