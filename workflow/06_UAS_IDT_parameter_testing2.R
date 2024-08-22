
##title: "Tree delineations using drone based point cloud data"

##########################################

## In this module we use unmanned aerial system derived point cloud data to delineate individual tree locations.

#check the required libraries are available. If not install required libraries.

list.of.packages <- c("tidyverse","lidR","lhs","terra","raster","rgdal","ForestTools","RCSF","sp","sf","stars","rgl","here")
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
library(here)
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

CHM_OUTPUTS_UNCROPPED_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/chms-uncropped/chm-ptcloud/"
# ^ the generated chms will be saved here

ASSOC_TABLE_DIR = "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/"

CHM_OUTPUTS_CROPPED_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/chms-mesh/"
# ^ the generated chms will be saved here

PREDICTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/treetops-output/"
# ^ the generated tree tops geopackages will be saved here

chm.files = list.files(CHM_OUTPUTS_CROPPED_DIR, pattern = "dtm-ptcloud", recursive = TRUE, full.names = FALSE)
# ^ read the cropped chms for the tree top delinietion 

PLOT_IDs = substr(chm.files,1,nchar(chm.files)-4)
####to test the code all input parameters such as dsm, dtm, and chm smooting parameter have been hard corded.


# resampling and smoothing the chm for parameter testing
chm_res = 0.25
chm_smooth = 3


# Define ranges for the parameters a, b, and c
a_range <- seq(0.1, 1.0, by= 0.2)

# start b and c with 0 so that can have fixed window as well for tree top detection within selected algorithms
b_range <- seq(0, 1.0, by= 0.2) 
c_range <- seq(0, 1.0, by= 0.2)

# Define the min and max range for window size parameter (ws)
min_range <- 1
max_range <- 10

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
transformed_samples$hmax <- rep(param_ranges$hmax, n_samples)
transformed_samples$itd_a <- sample(param_ranges$itd_a, 30, replace = TRUE)
transformed_samples$itd_b <- sample(param_ranges$itd_b, 30, replace = TRUE)
transformed_samples$itd_c <- sample(param_ranges$itd_c, 30, replace = TRUE)
transformed_samples$algorithm <-  sample(param_ranges$algorithm, n_samples, replace = TRUE)
transformed_samples$param_ID <- sprintf("%04d", 1:n_samples)


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
predict_trees_from_chm = function(plot_id,
                                  chm_dir,
                                  chm_res,
                                  chm_smooth,
                                  itd_a,
                                  itd_b,
                                  itd_c,
                                  itd_params_id,
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
  out_filepath = file.path(datadir, str_c(str_c(plot_id, itd_params_id),".gpkg"))
  st_write(ttops, out_filepath, delete_dsn = TRUE)
  return(nrow(ttops))
  
}

# Evaluate all sampled parameter sets


for (r in 1:length(PLOT_IDs)) {
  
  results <- data.frame()
  plot_id <- PLOT_IDs[r]
  chm_dir <- CHM_OUTPUTS_CROPPED_DIR
  data_dir = here::here(paste0(PREDICTED_TREES_DIR,plot_id))
  
  if(!dir.exists(data_dir)){
    dir.create(data_dir)
  }

  for (i in 1:n_samples) {
    print(i)
    params <- transformed_samples[i, ]
    #n_trees <- evaluate_detection(chm, params$hmin, params$itd_a, params$itd_b,params$itd_c, params$algorithm,plot_num)
    n_trees = predict_trees_from_chm(plot_id = plot_id,chm_dir = chm_dir, chm_res = chm_res, hmin= params$hmin, chm_smooth = 3,itd_a = params$itd_a, itd_b = params$itd_b, itd_c = params$itd_c, itd_params_id=params$param_ID, datadir=data_dir)
    results <- rbind(results, cbind(params, n_trees))
    
  }

csv_file_path  = file.path(data_dir, str_c(str_c(plot_id,"params", sep="_"),".csv"))
write.csv(results, csv_file_path)

}
