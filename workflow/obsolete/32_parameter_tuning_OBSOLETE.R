# Load necessary libraries
library(tidyverse)
library(dplyr)
library(lidR)
library(sf)
library(terra)

# install.packages("sf")
# install.packages("dplyr")

# Load functions from the 'ofo' R package
devtools::load_all("/ofo-share/utils/ofo-r/")

# Data paths
# PHOTOGRAMMETRY_OUTPUTS_DIR <- "/ofo-share/ofo-itd-crossmapping_data/drone/photogrammetry-outputs/"
# PLOT_BOUNDS_DIR <- "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/plot-bounds/"
# CHM_OUTPUTS_UNCROPPED_DIR <- "/ofo-share/ofo-itd-crossmapping_data/drone/chms-uncropped/chm-ptcloud/"
# ASSOC_TABLE_DIR <- "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/"
CHM_OUTPUTS_CROPPED_DIR <- "/ofo-share/ofo-itd-crossmapping_data/drone/chms-cropped/chm-mesh/"
PREDICTED_TREES_DIR <- "/ofo-share/ofo-itd-crossmapping_data/drone/treetops-output-updated/"

# CHM files and plot IDs
chm_files <- list.files(CHM_OUTPUTS_CROPPED_DIR, pattern = ".tif", recursive = TRUE, full.names = FALSE)
PLOT_IDs <- substr(chm_files, 1, nchar(chm_files) - 4)

# CHM_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/chms-cropped/chm-mesh/"
# #PREDICTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees/"
# PREDICTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/treetops-output-updated/"
OBSERVED_ALIGNED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/trees/"
OBSERVED_ALIGNED_PLOTBOUNDS_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/plot-bounds/"

#PARAMETER_LIST = read.csv("/ofo-share/ofo-itd-crossmapping_data/drone/treetops-output/0005/0005_params.csv")

#### Functions

# ITD variable radius window function
make_win_fun <- function(a, b, c, min_ht = 2, max_ht = 60, min_rad = 1, max_rad = 10) {
  win_fun <- function(x) {
    win <- a + b * x + c * x^2
    win[win < min_rad] <- min_rad
    win[win > max_rad] <- max_rad
    return(win)
  }
  return(win_fun)
}

# Tree prediction function
predict_trees_from_chm <- function(plot_id, chm_dir, chm_res, hmin, chm_smooth, itd_a, itd_b, itd_c, ttops_out_dir) {
  chm_file <- file.path(chm_dir, paste0(plot_id, ".tif"))
  chm <- terra::rast(chm_file)
  chm_resamp <- terra::project(chm, terra::crs(chm), res = chm_res, method = "bilinear")
  chm_smooth <- terra::focal(chm_resamp, w = matrix(1, chm_smooth, chm_smooth), mean)
  win_fun <- make_win_fun(itd_a, itd_b, itd_c)
  ttops <- lidR::locate_trees(chm_smooth, algorithm = lmf(ws = win_fun, shape = "circular", hmin = hmin))
  out_filepath <- paste0(ttops_out_dir, plot_id, ".gpkg")
  st_write(ttops, out_filepath, delete_dsn = TRUE)
  return(nrow(ttops))
}

#### Workflow

#Plot_ID_list = list.files("/ofo-share/ofo-itd-crossmapping_data/drone/treetops-output/")

# Define parameter ranges
param_ranges <- list(
  hmin = seq(2, 15, by = 1),
  hmax = seq(20, 60, by = 10),
  itd_a = seq(0.1, 1.0, by = 0.1),
  itd_b = seq(0, 1.0, by = 0.1),
  itd_c = seq(0, 1.0, by = 0.1),
  ws = seq(1, 15, by = 1),
  algorithm = c("lmf")
)

# Generate full parameter grid
param_grid <- expand.grid(param_ranges)








# Initialize tracking for best score and parameters
sample_size <- 300
max_iterations <- 100
tolerance <- 0.01
best_score <- 0.50
best_params <- NULL
results <- data.frame()

# Sample a subset of the parameter sets
set.seed(123)
sampled_params <- param_grid %>% sample_n(sample_size)


# Iteratively evaluate each parameter set selected in the subset




for (i in 1:length(PLOT_IDs)) {
  
  
  plot_id <- PLOT_IDs[i]
  print(plot_id)
  
  for (j in 1:nrow(sampled_params)) {
    
    params <- sampled_params[j, ]
    
    print(params)
    
    
    
    n_trees <- predict_trees_from_chm(plot_id = plot_id, chm_dir = CHM_OUTPUTS_CROPPED_DIR,
                                      chm_res = 0.25, hmin = params$hmin, chm_smooth = 3,
                                      itd_a = params$itd_a, itd_b = params$itd_b, itd_c = params$itd_c,
                                      ttops_out_dir = here::here(PREDICTED_TREES_DIR))

    pred_tree_file <- paste0(PREDICTED_TREES_DIR, plot_id, ".gpkg")
    pred_trees <- st_read(pred_tree_file)
    obs_trees <- st_read(file.path(OBSERVED_ALIGNED_TREES_DIR, paste0(plot_id, ".gpkg")))
    obs_bounds <- st_read(file.path(OBSERVED_ALIGNED_PLOTBOUNDS_DIR, paste0(plot_id, ".gpkg")))
    
    pred_trees <- st_transform(pred_trees, st_crs(obs_trees))
    coords_pred <- st_coordinates(pred_trees)
    pred_trees$x <- coords_pred[, "X"]
    pred_trees$y <- coords_pred[, "Y"]
    pred_trees$z <- pred_trees$Z
    
    
    
    obs_trees_prepped <- prep_obs_map(obs_trees, obs_bounds, edge_buffer = 5)
    pred_trees_prepped <- prep_pred_map(pred_trees, obs_bounds, edge_buffer = 5)
    
    
    
    obs_trees_matched <- ofo:::match_obs_to_pred_mee(obs_trees_prepped, pred_trees_prepped,
                                                     search_distance_fun_intercept = 1,
                                                     search_distance_fun_slope = 0.1,
                                                     search_height_proportion = 0.5)
    
    match_stats <- ofo:::compute_match_stats(pred_trees_prepped, obs_trees_matched, min_height = min(obs_trees$height))
    
    results <- rbind(results, match_stats)
    
    
    
  }
  
  # if (iteration > 1 && (best_score - prev_best_score) < tolerance) {
  #   message("Converged on best parameters with minimal improvement.")
  #   break
  # }
  # 
  # prev_best_score <- best_score
}



# # Iteratively evaluate random subsets of the parameter grid
# for (iteration in 1:max_iterations) {
#   
#   print (iteration)
#   
#   sampled_params <- param_grid %>% sample_n(sample_size)
#   
#   for (plot in 1:length(PLOT_IDs)) {
#     
#     
#     plot_id <- PLOT_IDs[plot]
#     print(plot_id)
#     
#     for (i in 1:nrow(sampled_params)) {
#       
#       params <- sampled_params[i, ]
#       
#       print(params)
#     
#    
#  
#       n_trees <- predict_trees_from_chm(plot_id = plot_id, chm_dir = CHM_OUTPUTS_CROPPED_DIR,
#                                         chm_res = 0.25, hmin = params$hmin, chm_smooth = 3,
#                                         itd_a = params$itd_a, itd_b = params$itd_b, itd_c = params$itd_c,
#                                         datadir = here::here(PREDICTED_TREES_DIR))
#       pred_tree_file <- paste0(PREDICTED_TREES_DIR, plot_id, ".gpkg")
#       pred_trees <- st_read(pred_tree_file)
#       obs_trees <- st_read(file.path(OBSERVED_ALIGNED_TREES_DIR, paste0(plot_id, ".gpkg")))
#       obs_bounds <- st_read(file.path(OBSERVED_ALIGNED_PLOTBOUNDS_DIR, paste0(plot_id, ".gpkg")))
#       
#       pred_trees <- st_transform(pred_trees, st_crs(obs_trees))
#       coords_pred <- st_coordinates(pred_trees)
#       pred_trees$x <- coords_pred[, "X"]
#       pred_trees$y <- coords_pred[, "Y"]
#       pred_trees$z <- pred_trees$Z
#       
#       obs_trees <- ofo:::prep_obs_map(obs_trees, obs_bounds, edge_buffer = 5)
#       pred_trees <- ofo:::prep_pred_map(pred_trees, obs_bounds, edge_buffer = 5)
#       
#       obs_trees_matched <- ofo:::match_obs_to_pred_mee(obs_trees, pred_trees,
#                                                        search_distance_fun_intercept = 1,
#                                                        search_distance_fun_slope = 0.1,
#                                                        search_height_proportion = 0.5)
#       
#       match_stats <- ofo:::compute_match_stats(pred_trees, obs_trees_matched, min_height = min(obs_trees$height))
#       
#       f_score <- match_stats$f_score
#       precision <- match_stats$precision
#       recall <- match_stats$recall
#       composite_score <- (0.33 * f_score) + (0.33 * precision) + (0.33 * recall)
#       
#       
#       if (f_score > best_score) {
#         best_score <- f_score
#         best_params <- params
#       }
#       
#       results <- rbind(results, cbind(plot_id, best_score, best_params))
#       
#     }
#     
#     # if (iteration > 1 && (best_score - prev_best_score) < tolerance) {
#     #   message("Converged on best parameters with minimal improvement.")
#     #   break
#     # }
#     # 
#     # prev_best_score <- best_score
#   }
# }

# Save all evaluated parameter sets and scores for reference
write.csv(results, "parameter_tuning_results_11_12_2024.csv", row.names = FALSE)




# Define the evaluate function
evaluate <- function(results, attributes) {
  # Assuming a simple weighted sum of recall, precision, and f-score for example
  recall <- results$recall
  precision <- results$precision
  f_score <- results$f_score
  
  # Example of combining them into a single metric
  # You can adjust the weights or formula as desired
  score <- 0.33 * recall + 0.33 * precision + 0.33 * f_score
  
  # Optionally, incorporate attributes if they affect the metric
  # (e.g., scaling the score based on parameter values)
  
  return(score)
}

# Updated compute_metric_per_trial function
compute_metric_per_trial <- function(results_df, eval_function, params_df = NULL) {
  metric_values <- vector()
  
  for (i in seq_len(nrow(results_df))) {
    results <- results_df[i, ]
    params <- if (is.null(params_df)) NULL else params_df[i, ]
    metric <- eval_function(results, params)
    metric_values[i] <- metric
  }
  return(metric_values)
}


# Run the evaluation
metric_values <- compute_metric_per_trial(results, evaluate, results[,c(2:7)])

# Plot the performance across trials
plot(metric_values, type = "b", main = "Performance Metric Across Trials",
     xlab = "Trial", ylab = "Performance Metric", col = "blue", pch = 19)

