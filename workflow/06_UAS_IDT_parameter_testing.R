
##title: "Tree delineations using drone based point cloud data"

##########################################

## In this module we use unmanned aerial system derived point cloud data to delineate individual tree locations.

#check the required libraries are available. If not install required libraries.

list.of.packages <- c("tidyverse","lidR","lhs","terra","raster","rgdal","ForestTools","RCSF","sp","sf","stars","rgl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


## Load the required libraries. We use several libraries that can help with loading, reading spatial data including raster and point cloud data, 
## analyse those data, and to visualize and write the spatial data back into a desired location.

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
#rgl::setupKnitr(autoprint = TRUE)

################################################################

# Load functions from the 'ofo' R package. The copy in "/ofo-share/utils" is intended to contain the
# latest version of the 'main' branch. If you want to make edits and test their effect here as you
# edit ofo-r, you could instead clone the 'ofo-r' repo to your own 'repos' folder and change the
# path below to match where you cloned it to.
# devtools::install("/ofo-share/repos-derek/ofo-r", quick = TRUE); library(ofo)
devtools::load_all("/ofo-share/utils/ofo-r/")

# Data paths
PHOTOGRAMMETRY_OUTPUTS_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/photogrammetry-outputs/"
# ^ within this folder, we assume there is 'dsm-mesh', 'dsm-ptcloud', and 'dtm-ptcloud' subfolders

PLOT_BOUNDS_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/plot-bounds/"

CHM_OUTPUTS_UNCROPPED_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/chms-uncropped/"
# ^ the generated chms will be saved here

ASSOC_TABLE_DIR = "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/"


# set working environment
# wd <- "C:/Users/nayani/mydata/other_projects/Open_forest_observatory/outputs/dsm_dtm"
# setwd(wd)


####to test the code all input parameters such as dsm, dtm, and chm smooting parameter have been hard corded.

# dtm.files = list.files(wd, pattern = "dtm-ptcloud", recursive = TRUE, full.names = FALSE)
# 
# dtm = raster("20220622-0038_20240324T1905_dtm-ptcloud.tif")
# dsm = raster("20220622-0038_20240324T1905_dsm-ptcloud.tif")

#Get the chm generated from the ofo-r and saved in the folder already
chm = dsm - dtm # get the chm from already generated ones


# resampling and smoothing the chm for parameter testing
chm_res = 0.25
chm_smooth = 3

res_chm  <- res(chm) #[1] 0.1336096 0.1336096)
extent_chm <- extent(chm)
# Define the new resolution
new_res <- chm_res # Change this value to your desired resolution

# Create a template raster with the new resolution
extent_r <- extent(chm)
ncol_new <- ceiling((extent_r@xmax - extent_r@xmin) / new_res)
nrow_new <- ceiling((extent_r@ymax - extent_r@ymin) / new_res)
resampled_chm <- raster(ncol = ncol_new, nrow = nrow_new)

# Set the extent of the new raster to match the original raster
extent(resampled_chm) <- extent_chm

# Set the resolution of the new raster
res(resampled_chm) <- new_res

# Resample the original raster to the new raster
resampled_chm <- raster::resample(chm, resampled_chm, method = "ngb")  # Use "bilinear" or "ngb"


# Smooth it with the specified smoothing window
chm_smooth = terra::focal(resampled_chm, w = matrix(1, chm_smooth, chm_smooth), mean)

chm_smooth[raster::getValues(chm_smooth) < 0] <- 0


chm[raster::getValues(chm) < 0] <- 0



####################Test with one sample parameter test

# Create a function for window generation 

# Define the function to generate the window size function

win_fun = function(x) {
  win = a + b * x + c * x^2
  win[win < min_rad] = min_rad
  win[win > max_rad] = max_rad
  return(win)
}

make_win_fun = function(a, b, c, min_ht = 2, max_ht = 50, min_rad = 1, max_rad = 10) {
  win_fun = function(x) {
    win = a + b * x + c * x^2
    win[win < min_rad] = min_rad
    win[win > max_rad] = max_rad
    return(win)
  }
  return(win_fun)
}

# Define ranges for the parameters a, b, and c
a_range <- seq(0.1, 1.0, by= 0.2)

# start b and c with 0 so that can have fixed window as well for tree top detection within selected algorithms
b_range <- seq(0, 1.0, by= 0.2) 
c_range <- seq(0, 1.0, by= 0.2)

# Define the min and max range for window size parameter (ws)
min_range <- 1
max_range <- 10
# 
# # Randomly generate samples for a, b, and c
# set.seed(42)  # For reproducibility
# itd_a <- runif(1, a_range[1], a_range[2])
# itd_b <- runif(1, b_range[1], b_range[2])
# itd_c <- runif(1, c_range[1], c_range[2])

# Create the window function using the generated parameters
win_fun <- make_win_fun(itd_a, itd_b, itd_c)

# Generate a range of window values to evaluate the window function
win_values <- seq(min_range, max_range, by = 1)

# Apply the window function to generate ws values
ws_values <- win_fun(win_values)

# Ensure ws_values are within min_range and max_range
ws_values <- ws_values[ws_values >= min_range & ws_values <= max_range]


# Define parameter ranges
param_ranges <- list(
  hmin =  seq(2, 10, by = 1),        # Example range for minimum height
  hmax = 50, # Example range for maximum height
  itd_a = a_range,
  itd_b  = b_range,
  itd_c = c_range,
  #ws = c(3, 15),          # Example range for window size
  algorithm = "lmf" # Example algorithms
)

# Number of random samples
n_samples <- 30



# Generate Latin Hypercube samples for numerical parameters
lhs_samples <- randomLHS(n_samples, length(param_ranges)+1) #

# Transform samples to the defined parameter ranges
transformed_samples <- as.data.frame(lhs_samples)

names(transformed_samples) <- c(names(param_ranges)[1:6],"param_ID")

transformed_samples$hmin <- sample(param_ranges$hmin, 30, replace = TRUE)
transformed_samples$hmax <- 50
transformed_samples$itd_a <- sample(param_ranges$itd_a, 30, replace = TRUE)
transformed_samples$itd_b <- sample(param_ranges$itd_b, 30, replace = TRUE)
transformed_samples$itd_c <- sample(param_ranges$itd_c, 30, replace = TRUE)
transformed_samples$algorithm <-  sample(param_ranges$algorithm, n_samples, replace = TRUE)
transformed_samples$param_ID <- sprintf("%04d", 1:n_samples)


# Randomly assign algorithms
set.seed(123)
transformed_samples$algorithm <- sample(param_ranges$algorithm, n_samples, replace = TRUE)

plot_num = 1

# win_fun <- function(x, a, b, c){x^2*c + x*b + a} # window filter function to use in next step
# 
# 
# # Function to evaluate individual tree detection
# evaluate_detection <- function(chm, hmin, a, b,c, algorithm, plot_num) {
#   if (algorithm == "lmf") {
#     
#     algo <- lmf(win_fun,shape = "circular", hmin = hmin)
#   } else if (algorithm == "dalponte") {
#     print("cannot_run") 
#   }
#   ttops = lidR::locate_trees(chm, algo)
  out_filepath = file.path(wd, str_c(str_c("plot",plot_num,i, params$algorithm, sep="_"),".gpkg"))
  st_write(ttops, out_filepath, delete_dsn = TRUE)
#   return(nrow(ttops))  # Return number of detected trees as an example performance metric
# }




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
                                  itd_params_id
                                  #datadir = datadir
                                  ) {
  
  chm <- terra::rast(chm)
  # Resample it to the specified res
  chm_resamp = terra::project(chm, terra::crs(chm), res = chm_res, method = "bilinear")
  
  # Smooth it with the specified smoothing window
  chm_smooth = terra::focal(chm_resamp, w = matrix(1, chm_smooth, chm_smooth), mean)
  
  # Detect treetops from it with the specified window size parameters
  win_fun = make_win_fun(itd_a, itd_b, itd_c)
  ttops = lidR::locate_trees(chm_smooth, algorithm = lmf(ws = win_fun, shape = "circular", hmin = 3))
  out_filepath = file.path(wd, str_c(str_c("plot_", itd_params_id),".gpkg"))
  st_write(ttops, out_filepath, delete_dsn = TRUE)
  return(nrow(ttops))
  
}

# Evaluate all sampled parameter sets
results <- data.frame()
for (i in 1:n_samples) {
  print(i)
  params <- transformed_samples[i, ]
  #n_trees <- evaluate_detection(chm, params$hmin, params$itd_a, params$itd_b,params$itd_c, params$algorithm,plot_num)
  n_trees = predict_trees_from_chm(chm,chm_res = chm_res, chm_smooth = 3,itd_a = params$itd_a, itd_b = params$itd_b, itd_c = params$itd_c, itd_params_id=params$param_ID)
  results <- rbind(results, cbind(params, n_trees))
}

write.csv(results, "param_ID.csv")


# # Function to evaluate the performance of the tree detection
# performance_metric <- function(detected_trees, ground_truth) {
#   precision <- sum(detected_trees %in% ground_truth) / length(detected_trees)
#   recall <- sum(detected_trees %in% ground_truth) / length(ground_truth)
#   f1_score <- 2 * (precision * recall) / (precision + recall)
#   return(f1_score)
# }
# 
# # Loop through the parameter grid and evaluate each parameter set
# best_params <- NULL
# best_score <- -Inf
# 
# for (i in 1:nrow(param_grid)) {
#   params <- param_grid[i, ]
#   detected_trees <- detect_trees(las, params$ws, params$algorithm)
#   
