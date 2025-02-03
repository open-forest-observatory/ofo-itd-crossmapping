
library(tidyverse)
library(lidR)
library(terra)
library(raster)
library(rgdal)
library(ForestTools)
library(RCSF)
library(sp)
library(sf)
library(stars)
library(rgl)
library(lhs)
library(caret)
library(mlr3)
library(mlr3tuning)
library(here)
library(tidyverse)
library(lidR)
library(sf)



wd = "C:/Users/nayani/mydata/other_projects/Open_forest_observatory/Data/set_14_thin22/"

setwd(wd)
# Define expanded ranges for parameters a, b, and c
a_range <- seq(0.1, 1.0, by = 0.1)  # Expanded range for 'a' with finer steps
b_range <- seq(0, 1.0, by = 0.1)    # Expanded range for 'b'
c_range <- seq(0, 1.0, by = 0.1)    # Expanded range for 'c'

# Define the minimum and maximum range for the window size parameter (ws)
min_range <- 1
max_range <- 15   # Increased max window size from 10 to 15 for more testing

# Define parameter ranges in a list for grid search
param_ranges <- list(
  hmin = seq(2, 15, by = 1),   # Expanded range for minimum height, now 2-15
  hmax = seq(20, 60, by = 10), # Range for maximum height, added incremental values
  itd_a = a_range,             # Range for parameter a
  itd_b = b_range,             # Range for parameter b
  itd_c = c_range,             # Range for parameter c
  ws = seq(min_range, max_range, by = 1), # Window size range with increments
  algorithm = c("lmf") # Multiple algorithms to test
)



# Define an ITD variable radius window function based on the coefficients a, b, and c defined above
# to the formula: win = a + b * h + c * h^2. Also define a minimum and maximum search radius.
# Currently these latter two are hard-coded but could be added to the search space in the future.
make_win_fun = function(a, b, c, min_ht = 2, max_ht = 60, min_rad = 1, max_rad = 10) {
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
predict_trees_from_chm = function(plot_id,
                                  chm_dir,
                                  chm_res,
                                  hmin,
                                  chm_smooth,
                                  itd_a,
                                  itd_b,
                                  itd_c,
                                  datadir = datadir)
{
  
  # Get the CHM filename based on the plot ID
  chm_file = file.path(chm_dir, str_c(plot_id, ".tif"))
  
  chm <- terra::rast(chm_file)
  # Resample it to the specified res
  chm_resamp = terra::project(chm, terra::crs(chm), res = chm_res, method = "bilinear")
  
  # Smooth it with the specified smoothing window
  chm_smooth = terra::focal(chm_resamp, w = matrix(1, chm_smooth, chm_smooth), mean)
  
  # Detect treetops from it with the specified window size parameters
  win_fun = make_win_fun(itd_a, itd_b, itd_c)
  ttops = lidR::locate_trees(chm_smooth, algorithm = lmf(ws = win_fun, shape = "circular", hmin = hmin))
  out_filepath = file.path(datadir, str_c(str_c(plot_id, sep="_"),".gpkg"))
  st_write(ttops, out_filepath, delete_dsn = TRUE)
  return(nrow(ttops))
  
}

# Evaluate all sampled parameter sets


# for (r in 1:length(PLOT_IDs)) {
#   results <- data.frame()
#   plot_id = PLOT_IDs[r]
#   chm_dir = CHM_OUTPUTS_CROPPED_DIR
#   data_dir = here::here(paste0(PREDICTED_TREES_DIR, plot_id))
#   if(!dir.exists(data_dir)){
#     dir.create(data_dir)
#   }
#   
#   for (i in 1:n_samples) {
#     print(i)
#     params <- transformed_samples[i, ]
#     #n_trees <- evaluate_detection(chm, params$hmin, params$itd_a, params$itd_b,params$itd_c, params$algorithm,plot_num)
#     n_trees = predict_trees_from_chm(plot_id = plot_id, chm_dir = chm_dir,chm_res = chm_res, hmin=params$hmin, chm_smooth = 3,itd_a = params$itd_a, itd_b = params$itd_b, itd_c = params$itd_c, itd_params_id=params$param_ID, datadir=data_dir)
#     results <- rbind(results, cbind(params, n_trees))
#     
#   }
#   
#   csv_file_path  = file.path(data_dir, str_c(str_c(plot_id,"params",sep = "_"),".csv"))
#   write.csv(results, csv_file_path)
#   
# }



# Load necessary scripts and libraries
source("parameter-tuning.R")              # For generating param_grid
source("06_UAS_IDT_parameter_testing2.R") # For tree delineation
source("30_evaluate-predicted-trees.R")   # For evaluation function
library(dplyr)

# Generate the full parameter grid
param_grid <- expand.grid(param_ranges)

# Define the random sampling size and maximum iterations
sample_size <- 30  # Number of parameter sets to sample per iteration
max_iterations <- 10
tolerance <- 0.01  # Minimum improvement in score to continue

# Initialize tracking for best score and parameters
best_score <- -Inf
best_params <- NULL
results <- data.frame()  # To store all evaluated parameter sets and their scores

set.seed(123)  # For reproducibility of random sampling


plot_id = 1
chm_res = 0.25
chm_smooth = 3
chm_dir = wd
data_dir = wd


# Iteratively evaluate random subsets of the parameter grid
for (iteration in 1:max_iterations) {
  # Randomly sample a subset of parameter sets from the grid
  sampled_params <- param_grid %>% sample_n(sample_size)
  
  for (i in 1:nrow(sampled_params)) {
    params <- sampled_params[i, ]
    
    # Delineate trees with the current parameter set
    #delineated_trees <- delineate_trees(params)
    n_trees = predict_trees_from_chm(plot_id = plot_id, chm_dir = chm_dir,chm_res = chm_res, hmin=params$hmin, chm_smooth = 3,itd_a = params$itd_a, itd_b = params$itd_b, itd_c = params$itd_c, itd_params_id=params$param_ID, datadir=data_dir)
    # Evaluate with F-score, precision, recall
    metrics <- evaluate_trees(delineated_trees)
    f_score <- metrics$f_score
    precision <- metrics$precision
    recall <- metrics$recall
    
    # Calculate composite score (using weights if necessary)
    composite_score <- (0.5 * f_score) + (0.25 * precision) + (0.25 * recall)
    
    # Store each parameter set and score for later analysis
    results <- rbind(results, cbind(params, f_score, precision, recall, composite_score))
    
    # Update the best score and parameters if the current score is better
    if (composite_score > best_score) {
      best_score <- composite_score
      best_params <- params
    }
  }
  
  # Check if improvement is less than tolerance to stop
  if (iteration > 1 && (best_score - prev_best_score) < tolerance) {
    message("Converged on best parameters with minimal improvement.")
    break
  }
  
  # Update previous best score to check for improvement
  prev_best_score <- best_score
}

# Output the best parameter set and its score
cat("Best Parameters:\n", best_params, "\n")
cat("Best Composite Score:\n", best_score, "\n")

# Save all evaluated parameter sets and their scores for reference
write.csv(results, "parameter_tuning_results.csv", row.names = FALSE)














#######################################################################
create_parameter_grid = function(parameter_list, n_random_samples = NULL) {
  #' Create a grid search of parameters and optionally take a random subset
  #'
  #' @param parameter_list A named list of parameters. Each element is either a list of values or a single value
  #' @param n_random_samples How many samples to draw from the cross product list. If NULL, all elements will be taken
  #'
  #' @return A dataframe where each row contains one value from each parameter drawn from the list
  
  # Create the cross product of all elements in the list. The column names will be taken from the
  # names in the list
  param_grid = expand.grid(parameter_list)
  
  # Take a random sampling of rows if requested and the number is lower than the number of rows
  if (!is.null(n_random_samples) && n_random_samples < nrow(param_grid)) {
    param_grid = param_grid[sample(nrow(param_grid), n_random_samples), ]
  }
  return(param_grid)
}



all_map_param_configurations = create_parameter_grid(param_ranges, n_random_samples = 30)
# Randomize the order so it's more likely that we hit errors early if they exist
all_map_param_configurations = all_map_param_configurations[
  sample(nrow(all_map_param_configurations)),
]



# Optional: Generate all combinations of parameters using expand.grid for full grid search
param_grid <- expand.grid(param_ranges)

# Load necessary scripts
source("parameter-tuning.R")              # For generating param_grid
source("06_UAS_IDT_parameter_testing2.R") # For tree delineation
source("30_evaluate-predicted-trees.R")   # For evaluation function

# Initialize tracking for best parameter set and score
best_score <- -Inf
best_params <- NULL
results <- data.frame()  # To store all scores and parameter sets for reference

# Define weights for each metric (adjust as necessary)
weight_fscore <- 0.5
weight_precision <- 0.25
weight_recall <- 0.25

# Iterate over each parameter set
for (i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  
  # Delineate trees with the current parameter set
  delineated_trees <- delineate_trees(params)
  
  # Evaluate the results and retrieve F-score, precision, and recall
  metrics <- evaluate_trees(delineated_trees)
  f_score <- metrics$f_score
  precision <- metrics$precision
  recall <- metrics$recall
  
  # Calculate a composite score based on weights
  composite_score <- (weight_fscore * f_score) + 
    (weight_precision * precision) + 
    (weight_recall * recall)
  
  # Store results for reference
  results <- rbind(results, cbind(params, f_score, precision, recall, composite_score))
  
  # Update best parameters if the current composite score is higher
  if (composite_score > best_score) {
    best_score <- composite_score
    best_params <- params
  }
}

# Output the best parameters and their score
cat("Best Parameters:\n", best_params, "\n")
cat("Best Composite Score:\n", best_score, "\n")

# Optionally save all results for analysis
write.csv(results, "parameter_tuning_results.csv", row.names = FALSE)





test_tree_map_alignment = function(
    map_params,
    alignment_methods,
    per_method_alignment_arguments = NULL,
    alignment_method_names = NULL,
    n_random_samples = NULL) {
  #' Test the different tree alignment algorithms in a variety of situations
  #'
  #' @param map_params Named list. The names correspond to arguments to simulate_tree_maps. The values
  #' correspond to potential values of this argument to be tried
  #' @param alignment_methods List of alignment algorithms
  #' @param per_method_alignment_arguments A list of named lists, one for each alignment method.
  #' The arguments in each list will be passed to the corresponding function
  #' @param alignment_method_names The human-readable names for each method
  #' @param n_random_samples The number of random samples to draw from the grid of paramters
  #'
  #' @returns Named list with the following fields
  #'   per_method_results: Named list, where each name is from alignment_method_names. The value is a data
  #'     frame of results from the experiments using that method.
  #'   all_map_param_configurations: Dataframe of paramters used in experiments, one row per trial
  
  # Get the names of all arguments of the function and their default values
  simulate_tree_maps_default_args = formals(simulate_tree_maps)
  
  # Build the list of all provided values while using defaults for unspecified values
  # Create an empty list
  all_map_params = list()
  # Iterate over the function arguments
  for (name in names(simulate_tree_maps_default_args)) {
    if (name %in% names(map_params)) {
      # If the name is in the named list of parameters use that value
      all_map_params[name] = map_params[name]
    } else {
      # Else, use the default value
      all_map_params[name] = simulate_tree_maps_default_args[name]
    }
    # If the value isn't a vector already make it a one-length vector containing that value
    if (!is.vector(all_map_params[[name]])) {
      all_map_params[name] = c(all_map_params[[name]])
    }
  }
  # Create the cross product of all parameter configurations
  all_map_param_configurations = create_parameter_grid(all_map_params, n_random_samples = n_random_samples)
  # Randomize the order so it's more likely that we hit errors early if they exist
  all_map_param_configurations = all_map_param_configurations[
    sample(nrow(all_map_param_configurations)),
  ]
  
  # Create names for each method if not provided
  if (is.null(alignment_method_names)) {
    alignment_method_names = as.character(seq_along(alignment_methods))
    # Define a function to zero pad to two characters
    zero_pad_2 = function(str){
      return(stringr::str_pad(str, width=2, side="left", pad = "0"))
    }
    alignment_method_names = lapply(alignment_method_names, FUN=zero_pad_2)
  }
  # Raise error if the list of names isn't the same length as the list of methods
  # This should only happen if a list is provided but it is spurious
  if (length(alignment_method_names) != length(alignment_methods)) {
    stop("Number of methods and method names do not match")
  }
  
  # Initialize the list of lists for the results
  # The sub-lists for each alignment method are indexed by the name of the method
  per_method_results = list()
  for (method_name in alignment_method_names) {
    per_method_results[[method_name]] = vector(mode = "list", length = nrow(all_map_param_configurations))
  }
  
  # Iterate over the parameter configurations and conduct the alignment for each one
  for (map_param_ind in seq_len(nrow(all_map_param_configurations))) {
    message(paste0("iteration: ", map_param_ind, " / ", nrow(all_map_param_configurations)))
    # Get one set of map parameters
    map_params = all_map_param_configurations[map_param_ind, ]
    # Create a simulated map based on these parameters
    simulated_obs_and_pred_map = do.call(simulate_tree_maps, map_params)
    
    # Iterate over the different alignment methods
    for (reg_ind in seq_along(alignment_methods)) {
      # Extract the method and method name
      alignment_method = alignment_methods[[reg_ind]]
      alignment_method_name = alignment_method_names[[reg_ind]]
      alignment_arguments = per_method_alignment_arguments[[reg_ind]]
      
      # Take the important arguments from the simulated map
      all_alignment_args = simulated_obs_and_pred_map[cbind("pred", "obs", "obs_bounds")]
      if (!is.null(per_method_alignment_arguments)) {
        all_alignment_args = c(all_alignment_args, alignment_arguments)
      }
      all_alignment_args = all_alignment_args[!duplicated(names(all_alignment_args))]
      
      # EXPENSIVE LINE
      # This is where alignment actually occurs
      predicted_shift = do.call(alignment_method, all_alignment_args)
      # Save the result in the output list for the appropriate method
      per_method_results[[alignment_method_name]][[map_param_ind]] = predicted_shift
    }
  }
  
  # Concatenate each row into a single dataframe
  for (alignment_method_name in alignment_method_names) {
    per_method_results[[alignment_method_name]] = as.data.frame(
      dplyr::bind_rows(per_method_results[[alignment_method_name]])
    )
  }
  
  # Return both the results and the parameters used to generate them
  return(
    list(
      per_method_results = per_method_results,
      all_map_param_configurations = all_map_param_configurations
    )
  )
}

