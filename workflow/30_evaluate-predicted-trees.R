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

## Set constants

# File paths
CHM_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/chms-cropped/chm-mesh/"
#PREDICTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees/"
PREDICTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/treetops-output/"
OBSERVED_ALIGNED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/trees/"
OBSERVED_ALIGNED_PLOTBOUNDS_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/plot-bounds/"

PARAMETER_LIST = read.csv("/ofo-share/ofo-itd-crossmapping_data/drone/treetops-output/0005/0005_params.csv")

#### Functions


#### Workflow

Plot_ID_list = list.files("/ofo-share/ofo-itd-crossmapping_data/drone/treetops-output/")
  

# Dataframe to save the resulsts



column_names = c("plot_ID", "param_ID", "n_obs_match_pred" ,"n_obs", "n_pred_match_obs", "n_pred" , "min_height", "recall", "precision", "f_score")
  
pred_results = setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

for (i in 1: length(Plot_ID_list)){
  
  print(i)
  
  # Get list of predicted tree maps to evaluate
  pred_tree_files = list.files(paste0(PREDICTED_TREES_DIR,Plot_ID_list[[i]]), pattern = "gpkg$")
  
  for (r in 1: length(pred_tree_files)){
    
    print(r)
    
    # TEMPORARY: Select a single predicted tree file to evaluate. TODO: Make this into a function that
    # can be applied in parallel over all predicted tree files.
    pred_tree_file = pred_tree_files[r]
    plot_id = str_sub(pred_tree_file, 1, 4)
    parameter_ID = as.numeric(substr(pred_tree_file,6,9))
    
    # Load the predicted tree map
    pred_trees = st_read(file.path(paste0(PREDICTED_TREES_DIR,plot_id), pred_tree_file))
    
    hmin = PARAMETER_LIST$hmin[PARAMETER_LIST$param_ID==parameter_ID]
    
    # Load the corresponding observed tree map and plot bounds
    #plot_id = str_sub(pred_tree_file, 18, 21)
    
    
    
    obs_trees = st_read(file.path(OBSERVED_ALIGNED_TREES_DIR, paste0(plot_id, ".gpkg")))
    obs_bounds = st_read(file.path(OBSERVED_ALIGNED_PLOTBOUNDS_DIR, paste0(plot_id, ".gpkg")))
    
    # Add the required x, y, and z columns to the predicted tree map
    pred_trees = st_transform(pred_trees, st_crs(obs_trees))
    coords_pred = st_coordinates(pred_trees)
    pred_trees$x = coords_pred[, "X"]
    pred_trees$y = coords_pred[, "Y"]
    pred_trees$z = pred_trees$Z
    
    # # Visualize the two tree maps
    # ofo:::vis2(pred = pred_trees,
    #           obs = obs_trees, 
    #           coords_arbitrary = FALSE)
    
    obs_trees = ofo:::prep_obs_map(obs_trees, obs_bounds, edge_buffer = 5)
    pred_trees = ofo:::prep_pred_map(pred_trees, obs_bounds, edge_buffer = 5)
    
    obs_trees_matched = ofo:::match_obs_to_pred_mee(obs_trees,
                                                    pred_trees,
                                                    search_distance_fun_intercept = 1,
                                                    search_distance_fun_slope = 0.1,
                                                    search_height_proportion = 0.5)
    
    match_stats = ofo:::compute_match_stats(pred_trees,
                                            obs_trees_matched,
                                            min_height = hmin)
    
    
    pred_results = rbind(pred_results, data.frame(plot_ID = plot_id, param_ID = parameter_ID, n_obs_match_pred = match_stats$n_obs_match_pred,
                                                  n_obs = match_stats$n_obs, n_pred_match_obs = match_stats$n_pred_match_obs, n_pred= match_stats$n_pred,
                                                  min_height = match_stats$min_height, recall = match_stats$recall, precision = match_stats$precision,
                                                  f_score = match_stats$f_score))
    
    
    
    
  }
  

 
  
  
}

file_name = paste0("/ofo-share/ofo-itd-crossmapping_data/drone/treetops-output/", "_","pred_results.csv")
write.csv( pred_results, file_name)


match_stats
