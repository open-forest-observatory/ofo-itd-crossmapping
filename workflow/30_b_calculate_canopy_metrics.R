install.packages(c("terra", "raster", "lidR", "dplyr", "purrr"))

library(terra)
library(raster)
library(lidR)
library(dplyr)
library(purrr)


# CHM_OUTPUTS_CROPPED_DIR <- "/ofo-share/ofo-itd-crossmapping_data/drone/chms-cropped/chm-mesh/"
# 
# 
# 
# compute_chi <- function(chm) {
#   focal_var <- terra::focal(chm, w = 3, fun = sd, na.policy = "omit", na.rm = TRUE)
#   mean(focal_var[], na.rm = TRUE)
# }
# 
# 
# 
# compute_clumping_index <- function(chm, threshold = 2) {
#   binary_canopy <- chm > threshold
#   clumps <- terra::patches(binary_canopy, directions = 8)
#   num_patches <- max(clumps[], na.rm = TRUE)
#   total_cells <- sum(!is.na(chm[]))
#   clumping_index <- num_patches / total_cells
#   return(clumping_index)
# }
# 
# 
# compute_height_metrics <- function(chm) {
#   values <- chm[]
#   values <- values[!is.na(values)]
#   tibble(
#     mean_height = mean(values),
#     max_height = max(values),
#     media_height = median(values),
#     sd_height = sd(values),
#     q25 = quantile(values, 0.25),
#     q50 = quantile(values, 0.5),
#     q75 = quantile(values, 0.75)
#   )
# }
# 
# 
# chm_files <- list.files(CHM_OUTPUTS_CROPPED_DIR, pattern = "\\.tif$", full.names = TRUE)
# 
# results <- map_df(chm_files, function(file) {
#   chm <- terra::rast(file)
#   
#   chi <- compute_chi(chm)
#   clumping <- compute_clumping_index(chm)
#   height_metrics <- compute_height_metrics(chm)
#   
#   tibble(
#     file = basename(file),
#     CHI = chi,
#     Clumping_Index = clumping
#   ) %>% bind_cols(height_metrics)
# })
# 
# 
# print(results)
# 
# library(dplyr)
# library(readr)
# 
# library(stringr)
# 
# results <- results %>%
#   mutate(plot_id = str_pad(parse_number(file), width = 4, pad = "0"))
# 

# results$plot_id <- as.integer(gsub("\\.tif$", "", results$file))


###############################################################################################################
###############################################################################################################


# Ensure required packages are installed
packages <- c("sf", "dplyr", "data.table", "stringr", "FNN", "dbscan", "purrr", "tibble")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

# Load necessary libraries
library(sf)
library(dplyr)
library(data.table)
library(stringr)
library(purrr)
library(tibble)

# Define directory with point data in GeoPackage format
GPKG_DIR <- "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/trees/"

# List all GeoPackage files
gpkg_files <- list.files(GPKG_DIR, pattern = "\\.gpkg$", full.names = TRUE)

# Function to compute CHI using k-nearest neighbors (local SD)
compute_chi_points <- function(df, k = 3) {
  # Filter out rows with NA in coordinates or height
  df_clean <- df %>% filter(!is.na(X), !is.na(Y), !is.na(height))
  
  coords <- as.matrix(df_clean[, c("X", "Y")])
  heights <- df_clean$height
  
  if (nrow(coords) < k) return(NA)  # Not enough points for KNN
  
  knn_result <- FNN::get.knn(coords, k = k)
  chi_vals <- sapply(1:nrow(knn_result$nn.index), function(i) {
    sd(heights[knn_result$nn.index[i, ]], na.rm = TRUE)
  })
  
  mean(chi_vals, na.rm = TRUE)
}


# Function to compute clumping index using DBSCAN
compute_clumping_index_points <- function(df, threshold = 2, eps = 1, minPts = 3) {
  canopy_points <- df %>% filter(height > threshold)
  total_points <- nrow(df)
  
  if (nrow(canopy_points) < minPts) return(0)  # Avoid errors in small samples
  
  coords <- as.matrix(canopy_points[, c("X", "Y")])
  db <- dbscan::dbscan(coords, eps = eps, minPts = minPts)
  
  num_clumps <- length(unique(db$cluster[db$cluster > 0]))
  clumping_index <- num_clumps / total_points
  return(clumping_index)
}

# Function to compute height metrics
compute_height_metrics_points <- function(df) {
  h <- df$height
  tibble(
    mean_height = mean(h, na.rm = TRUE),
    max_height = max(h, na.rm = TRUE),
    media_height = median(h, na.rm = TRUE),
    sd_height = sd(h, na.rm = TRUE),
    q25 = quantile(h, 0.25, na.rm = TRUE),
    q50 = quantile(h, 0.5, na.rm = TRUE),
    q75 = quantile(h, 0.75, na.rm = TRUE)
  )
}

library(dbscan)
# Process each GeoPackage file
results_gpkg <- map_df(gpkg_files, function(file) {
  print(file)
  pts <- sf::st_read(file, quiet = TRUE)
  pts = pts[pts$height>=5,]
  df <- sf::st_drop_geometry(pts)
  
  # Assume column for canopy height is named 'canopy_height'
  if (!"height" %in% names(df)) {
    if ("canopy_height" %in% names(df)) {
      df$height <- df$canopy_height
    } else {
      stop(paste("No height column found in", file))
    }
  }
  
  # Extract coordinates
  coords <- sf::st_coordinates(pts)
  df$X <- coords[, 1]
  df$Y <- coords[, 2]
  
  # Compute metrics
  chi <- compute_chi_points(df)
  clumping <- compute_clumping_index_points(df)
  height_metrics <- compute_height_metrics_points(df)
  
  # Combine into one result
  tibble(
    file = basename(file),
    CHI = chi,
    Clumping_Index = clumping
  ) %>% bind_cols(height_metrics)
})

# Extract numeric plot ID from filename
results_gpkg <- results_gpkg %>%
  mutate(plot_id = str_pad(parse_number(file), width = 4, pad = "0"))

# View results
print(results_gpkg)



library(dplyr)

# Merge based on plot_id — will replicate rows from results
merged_table <- d %>%
  left_join(results_gpkg, by = "plot_id")


library(dplyr)
library(stringr)  # also needed for str_pad()


# Bind plot-level density to the match stats
field_ref = read_csv(FIELD_REF)
dens = field_ref |>
  dplyr::select(plot_id = field_plot_id, obs_tree_density = tph) |>
  dplyr::mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))


d2 = left_join(merged_table, dens, by = join_by("plot_id" == "plot_id"))

merged_table2 = merged_table[merged_table$media_height>2,]


merged_table3 = merged_table[merged_table$media_height>5,]

merged_table4 = merged_table[merged_table$precision==1.0,]
