library(sf)
library(ofo)
library(tidyverse)
library(ggplot2)

PLOT_IDS = c(
  "0005",
  "0006",
  "0007",
  "0014",
  "0016",
  "0018",
  "0019",
  "0026",
  "0032",
  "0035",
  "0044",
  "0088",
  "0090",
  "0091",
  "0095",
  "0098",
  "0100",
  "0102",
  "0104"
)

# Compare a predicted tree map (specified by the file name) to the observed trees from the same plot
eval_preds = function(
    pred_trees,
    obs_trees,
    obs_bounds,
    edge_buffer,
    min_height_for_eval,
    min_height_pred,
    min_height_obs) {
  # Prepare to get the seconds elapsed during the evaluation
  start_time = Sys.time()

  # Prepare the predicted tree map for matching
  pred_trees = prep_pred_map(pred_trees, obs_bounds, edge_buffer = edge_buffer)
  # Prep the predicted and observed maps
  obs_trees = prep_obs_map(obs_trees, obs_bounds, edge_buffer = edge_buffer)

  pred_trees = pred_trees[pred_trees$pred_tree_height > min_height_pred, ]
  obs_trees = obs_trees[obs_trees$obs_tree_height > min_height_obs, ]

  # ! If there are no trees in the predicted tree map, return a recall and precision of 0

  if (nrow(pred_trees) > 0) {
    # Match the observed trees to the predicted trees
    obs_trees_matched = match_obs_to_pred_mee(obs_trees,
      pred_trees,
      search_distance_fun_intercept = 1,
      search_distance_fun_slope = 0.1,
      search_height_proportion = 0.5
    )
    if (TRUE) {
      tree_matched_obs_filtered = obs_trees_matched[!is.na(obs_trees_matched$matched_pred_tree_id), ]
      matches = match(tree_matched_obs_filtered$matched_pred_tree_id, pred_trees$pred_tree_id)
      tree_preds_ordered = pred_trees[matches, ]
      plot(tree_matched_obs_filtered$obs_tree_height, tree_preds_ordered$pred_tree_height)
    }

    # Compute the match statistics
    match_stats = compute_match_stats(
      pred_trees,
      obs_trees_matched,
      min_height = min_height_for_eval
    )
  } else {
    # If there are no trees in the predicted tree map, return a recall and precision of 0
    match_stats = data.frame(recall = 0, precision = 0, f_score = 0)
  }

  # Get the seconds elapsed during the evaluation
  end_time = Sys.time()
  elapsed_time = end_time - start_time
  elapsed_secs = as.numeric(elapsed_time, units = "secs")

  # Add the evaluation time to the match statistics
  match_stats$eval_time = elapsed_secs

  return(match_stats)
}

compute_plot_metrics = function(
    plot_ID,
    attribute_values,
    attribute_to_vary,
    default_edge_buffer = 5,
    default_min_height_for_eval = 10,
    default_min_height_pred = 0,
    default_min_height_obs = 0) {
  # Define the filenames
  preds_file = sprintf("/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees/paramgroup-41/paramset-000068_plot-%s.gpkg", plot_ID)
  reference_file = sprintf("/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/trees/%s.gpkg", plot_ID)
  plot_bounds = sprintf("/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/plot-bounds/%s.gpkg", plot_ID)

  obs_trees = st_read(reference_file)
  pred_trees = st_read(preds_file)
  obs_bounds = st_read(plot_bounds)

  pred_trees = st_transform(pred_trees, st_crs(obs_trees))
  coords_pred = st_coordinates(pred_trees)
  pred_trees$x = coords_pred[, "X"]
  pred_trees$y = coords_pred[, "Y"]
  pred_trees$z = pred_trees$Z

  n_experiments = length(attribute_values)

  evaluation_parameters = data.frame(
    edge_buffer = rep(default_edge_buffer, n_experiments),
    min_height_for_eval = rep(default_min_height_for_eval, n_experiments),
    min_height_pred = rep(default_min_height_pred, n_experiments),
    min_height_obs = rep(default_min_height_obs, n_experiments)
  )
  evaluation_parameters[attribute_to_vary] = attribute_values

  all_results = NULL

  for (i in 1:nrow(evaluation_parameters)) {
    row <- evaluation_parameters[i, ]
    # do stuff with row
    res = eval_preds(
      pred_trees,
      obs_trees,
      obs_bounds,
      edge_buffer = row$edge_buffer,
      min_height_for_eval = row$min_height_for_eval,
      min_height_pred = row$min_height_pred,
      min_height_obs = row$min_height_obs
    )

    if (is.null(all_results)) {
      all_results = res
    } else {
      all_results = rbind(all_results, res)
    }
  }

  return(all_results)
}

edge_buffers = 1:10 / 2
edge_buffer_experiments = lapply(
  PLOT_IDS,
  compute_plot_metrics,
  attribute_values = edge_buffers,
  attribute_to_vary = "edge_buffer"
)
edge_buffer_experiments_concatenated = bind_rows(edge_buffer_experiments)
edge_buffer_experiments_concatenated["edge_buffer"] = rep(edge_buffers, length(PLOT_IDS))
mean_across_plots = edge_buffer_experiments_concatenated %>%
  group_by(edge_buffer) %>%
  summarise(
    mean_recall = mean(recall),
    .groups = "drop"
  ) %>%
  as.data.frame()

x11()
ggplot(data = edge_buffer_experiments_concatenated, mapping = aes(x = edge_buffer, y = recall)) +
  geom_point() +
  geom_point(
    data = mean_across_plots,
    mapping = aes(x = edge_buffer, y = mean_recall), colour = "red", size = 3
  )
# plot(edge_buffer_experiments_concatenated$edge_buffer, edge_buffer_experiments_concatenated$recall)
# plot(mean_across_plots$edge_buffer, mean_across_plots$mean_recall, add = TRUE)
# https://stackoverflow.com/questions/24220676/r-script-using-x11-window-only-opens-for-a-second
while (names(dev.cur()) != "null device") Sys.sleep(1)
