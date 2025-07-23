install.packages(c("terra", "raster", "lidR", "dplyr", "purrr"))

library(terra)
library(raster)
library(lidR)
library(dplyr)
library(purrr)


# CHM_OUTPUTS_CROPPED_DIR <- "/ofo-share/ofo-itd-crossmapping_data/drone/chms-cropped/chm-mesh/"

resample_and_smooth_chm = function(chm, res, smooth_width) {
  chm_resamp <- terra::project(chm, terra::crs(chm), res = res, method = "bilinear")
  chm_smooth <- terra::focal(chm_resamp, w = matrix(1, smooth_width, smooth_width), mean, na.rm = TRUE)
  return(chm_smooth)
}

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
library(readr)


library(tidyverse)
library(plotly)
library(htmlwidgets)
library(mgcv)

## Set constants

# File paths
MATCH_STATS_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-tree-evals/"
ITD_PARAMS_DEF_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"
EVAL_FIGURES_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramset-eval-figures/"

# Processing constants for user to define

# Plots to exclude (those with improperly aligned field reference data)
PLOTS_EXCLUDE = c("0015", "0046", "0100", "0110")

# Which group of parameter sets to evaluate. Set to the same value as in the previous script (30).
FOC_PARAMGROUP = "7003"

# Create output directory
dir.create("paramID_plots_second_refined", showWarnings = FALSE)

#### Workflow

## Load match stats
filename = "smooth_10cm_7002_pred_results.csv" #paste0("match-stats_paramgroup-", paste(FOC_PARAMGROUP, collapse = "-"), ".csv")
d = read_csv("/ofo-share/ofo-itd-crossmapping_data/drone/predicted-tree-evals/match-stats_paramgroup-refined7005_new.csv") #(file.path(MATCH_STATS_DIR, filename))

## Filter to the focal plots
d = d |>
  filter(!plot_ID %in% PLOTS_EXCLUDE)

## Load the parameter set definitions
filename = paste0("itd-paramsets_", paste(FOC_PARAMGROUP, collapse = "-"), ".csv")
param_defs = read_csv("/ofo-share/ofo-itd-crossmapping_data/itd-paramsets//itd-paramsets_refined_7005.csv") #(file.path(ITD_PARAMS_DEF_DIR, filename))

## Bind the parameter definitions to the match stats
d = d |>
  left_join(param_defs, by = join_by("paramset_id" == "paramset_id"))


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
  b <- df$dbh
  tibble(
    mean_height = mean(h, na.rm = TRUE),
    max_height = max(h, na.rm = TRUE),
    media_height = median(h, na.rm = TRUE),
    mean_dbh = mean(b, na.rm = TRUE),
    max_dbh = max(b, na.rm = TRUE),
    media_dbh = median(b, na.rm = TRUE),
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

# Rename only if 'plot_id' exists
if ("plot_ID" %in% colnames(d)) {
  d <- d %>% rename(plot_id = plot_ID)
}


merged_table <- d %>%
  left_join(results_gpkg, by = "plot_id")


library(dplyr)
library(stringr)  # also needed for str_pad()


# # Bind plot-level density to the match stats
# field_ref = read_csv(FIELD_REF)
# dens = field_ref |>
#   dplyr::select(plot_id = field_plot_id, obs_tree_density = tph) |>
#   dplyr::mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))
# 

# d2 = left_join(merged_table, dens, by = join_by("plot_id" == "plot_id"))
# 
# merged_table2 = merged_table[merged_table$media_height>2,]
# 
# 
# merged_table3 = merged_table[merged_table$media_height>5,]
# 
# merged_table4 = merged_table[merged_table$precision==1.0,]

# ## Bind the parameter definitions to the match stats
# ## Load the parameter set definitions
# filename = paste0("itd-paramsets_", paste(FOC_PARAMGROUP, collapse = "-"), ".csv")
# param_defs = read_csv(file.path(ITD_PARAMS_DEF_DIR, filename))
# 
# 
# 
# 
# 
# 
# merged_table = d |>
#   left_join(param_defs, by = join_by("paramset_id" == "paramset_id"))
# 

## Prepare to make a multi-facet plot, where each facet is a different parameter, and the x-axis is
# the parameter value, and the y-axis is the f-score

# Pivot longer so that there is a column with the parameter name and a column with the parameter value
d_long = merged_table |>
  pivot_longer(cols = starts_with("lmf_"), names_to = "param", values_to = "value")

# Bind the param values back onto the long-form data frame, so that we can look up what parameter
# values (besides the focal parameter) were associated with the f-score for the focal parameter
# value

d_long = d_long |>
  left_join(param_defs, by = join_by("paramset_id" == "paramset_id"))

## VIS OPTION 1: Scatter plot by parameter

# Filter to just a few plots for vis purposes. NOTE: will want to expand this to all plots eventually.
d_plot = d_long |>
  filter(plot_id %in% c("0005"))

# Make the plot
p = ggplot(d_plot, aes(x = value,
                       y = f_score,
                       color = lmf_diam_max,
                       # fake aesthetics so they appear in the plotly tooltip
                       label1 = lmf_a,
                       label2 = lmf_b,
                       label3 = lmf_c,
                       label4 = lmf_diam_min,
                       label5 = lmf_diam_max,
                       label6 = plot_id)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(end = 0.85) +
  facet_grid(plot_id~param, scales = "free")
p


# Make the plot
p = ggplot(d_long, aes(x = value,
                       y = f_score,
                       color = f_score,
                       # fake aesthetics so they appear in the plotly tooltip
                       label1 = lmf_a,
                       label2 = lmf_b,
                       label3 = lmf_c,
                       label4 = lmf_diam_min,
                       label5 = lmf_diam_max,
                       label6 = plot_id)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(end = 0.85) +
  facet_grid(plot_id~param, scales = "free")
p


ggsave(
  filename = paste0("paramID_plots_second_refined/summary", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")

# ## VIS OPTION 2: Scatter plot by parameter, with plotly for interactive parameter lookup for each point in the plot
# 
# plotly = ggplotly(p, tooltip = c("label1", "label2", "label3", "label4", "label5", "label6"))
# # plotly
# # If the interactive plot does not work well in your IDE, save it as an HTML file, which you can
# # then open in a browser.
# filename = paste0("smooth_10cm_paramgroup-", FOC_PARAMGROUP, "plot05.html")
# saveWidget(plotly, file.path(EVAL_FIGURES_DIR, filename), selfcontained = TRUE, libdir = "lib")
# 
# 
# ## VIS OPTION 3: Fit a GAM to the data and visualize the partial effects of the parameters on the f-score
# 
# # Filter to just one plot, though may also want to fit across all plots in the future
# # NOTE: Try different plots here to see if the fit is different
# d_mod = d |>
#   filter(plot_id == "0007")
# 
# m = gam(f_score ~ s(lmf_a) + s(lmf_b) + s(lmf_c) + s(lmf_diam_min) + s(lmf_diam_max), data = d_mod)
# plot(m, scheme = 1)
# 

## VIS OPTION 4: Select the top 10 parameter sets for a given plot and see what their parameter values have in common

# Filter to just one plot for now, though we should also see if the pattern is consistent or variable across all plots



FIELD_REF <- "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/selected-field-and-drone-plots_v1.csv"
#"/ofo-share/repos-nayani/ofo-itd-crossmapping/workflow/field_data_all.csv"



## VIS OPTION 5: For each parameter set, average the F-score across all plots, low-density plots, and high-density plots

# Bind plot-level density to the match stats
field_ref = read_csv(FIELD_REF)

dens = field_ref |>
  dplyr::select(plot_id = field_plot_id, obs_tree_density = tph) |>
  mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))

dens <- na.omit(dens)

d2 = left_join(merged_table, dens, by = join_by("plot_id" == "plot_id"))
d2 = na.omit(d2)

mid_dens = median(na.omit(d2$obs_tree_density))

# Summarize the f-score by parameter set, across all plots and low- and high-density plots separately
d_fig_overall = d2 |>
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
  summarize(allplots = mean(f_score),
            n_plots = n()) |>
  ungroup()

d_fig_lowdens = d2 |>
  filter(obs_tree_density < mid_dens) |>
  group_by(paramset_id) |>
  summarize(lowdens = mean(f_score),
            n_plots_lowdens = n()) |>
  ungroup()

d_fig_highdens = d2 |>
  filter(obs_tree_density >= mid_dens) |>
  group_by(paramset_id) |>
  summarize(highdens = mean(f_score),
            n_plots_highdens = n()) |>
  ungroup()

d_fig_lowhigh = left_join(d_fig_lowdens, d_fig_highdens, by = join_by("paramset_id" == "paramset_id"))
d_fig = left_join(d_fig_overall, d_fig_lowhigh, by = join_by("paramset_id" == "paramset_id"))

d_fig_long = d_fig |>
  pivot_longer(cols = c("allplots", "lowdens", "highdens"),
               names_to = "f_score_type",
               values_to = "f_score")

p= ggplot(d_fig_long, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point() +
  facet_wrap(~f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

p

ggsave(
  filename = paste0("paramID_plots_second_refined/low_high_tph", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")


###########################

# > colnames(field_ref)
# [1] "field_plot_id"             "project_name"              "survey_year"               "plot_area"                 "min_dbh"                   "min_ht"                    "min_ht_ohvis"             
# [8] "num_ohvis_trees_excluded"  "tph"                       "dbh_mean"                  "dbh_cv"                    "forest_type"               "forest_type_dy"            "top_species"              
# [15] "contributor_field_plot_id" "drone_dataset_id"          "drone_imagery_year"        "tph_cat"                   "dbh_cat"                   "selected_drone_dataset_id" "...21"                    
# > 


# min_height_d = field_ref |>
#   select(plot_id = field_plot_id, min_height = min_ht) |>
#   mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))
# 
# min_height_d <- na.omit(min_height_d)
# 
# d3 = left_join(d, min_height_d, by = join_by("plot_id" == "plot_id"))
# d3 = na.omit(d3)

mid_mht = median(na.omit(merged_table$media_height))


# Summarize the f-score by parameter set, across all plots and low- and high-density plots separately
d_fig_overall = d2|>
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
  summarize(allplots = mean(f_score),
            n_plots = n()) |>
  ungroup()

d_fig_lowht = d2 |>
  filter(media_height < mid_mht) |>
  group_by(paramset_id) |>
  summarize(lowht = mean(f_score),
            n_plots_lowheight = n()) |>
  ungroup()

d_fig_highht = d2 |>
  filter(media_height >= mid_mht) |>
  group_by(paramset_id) |>
  summarize(highht = mean(f_score),
            n_plots_highheight = n()) |>
  ungroup()

d_fig_lowhigh = left_join(d_fig_lowht, d_fig_highht, by = join_by("paramset_id" == "paramset_id"))
d_fig = left_join(d_fig_overall, d_fig_lowhigh, by = join_by("paramset_id" == "paramset_id"))

d_fig_long = d_fig |>
  pivot_longer(cols = c("allplots", "lowht", "highht"),
               names_to = "f_score_type",
               values_to = "f_score")

p=ggplot(d_fig_long, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point() +
  facet_wrap(~f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

p

ggsave(
  filename = paste0("paramID_plots_second_refined/low_high_height", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")


#################################################
# min_ht_ohvis_d = field_ref |>
#   select(plot_id = field_plot_id, min_ht_ohvis_x = min_ht_ohvis) |>
#   mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))
# 
# min_ht_ohvis_d <- na.omit(min_ht_ohvis_d)
# 
# d4 = left_join(d, min_ht_ohvis_d, by = join_by("plot_id" == "plot_id"))
# d4 = na.omit(d4)

mid_dbh = median(na.omit(d2$media_dbh))


# Summarize the f-score by parameter set, across all plots and low- and high-density plots separately
d_fig_overall = d2|>
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
  summarize(allplots = mean(f_score),
            n_plots = n()) |>
  ungroup()

d_fig_lowdbh = d2 |>
  filter(media_dbh < mid_dbh) |>
  group_by(paramset_id) |>
  summarize(lowdbh = mean(f_score),
            n_plots_lowdens = n()) |>
  ungroup()

d_fig_highdbh = d2 |>
  filter(media_dbh >= mid_dbh) |>
  group_by(paramset_id) |>
  summarize(higdbh = mean(f_score),
            n_plots_highdens = n()) |>
  ungroup()

d_fig_lowhigh = left_join(d_fig_lowdbh, d_fig_highdbh, by = join_by("paramset_id" == "paramset_id"))
d_fig = left_join(d_fig_overall, d_fig_lowhigh, by = join_by("paramset_id" == "paramset_id"))

d_fig_long = d_fig |>
  pivot_longer(cols = c("allplots", "lowdbh", "higdbh"),
               names_to = "f_score_type",
               values_to = "f_score")

p = ggplot(d_fig_long, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point() +
  facet_wrap(~f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

p

ggsave(
  filename = paste0("paramID_plots_second_refined/low_high_dbh", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")

###############################
veg_cover = field_ref |>
  dplyr::select(plot_id = field_plot_id, forest_ty = forest_type_dy) |>
  mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))


veg_cover <- na.omit(veg_cover)

d5 = left_join(merged_table, veg_cover, by = join_by("plot_id" == "plot_id"))
d5 = na.omit(d5)


d_fig_overall = d5|>
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
  summarize(allplots = mean(f_score),
            n_plots = n()) |>
  ungroup()

d_fig_cover <- d5 |>
  group_by(forest_ty, paramset_id) |>
  summarize(
    mean_f_score = mean(f_score, na.rm = TRUE),
    n_plots = n()
  ) |>
  ungroup()

d_fig = left_join(d_fig_overall, d_fig_cover, by = join_by("paramset_id" == "paramset_id"))


p = ggplot(d_fig, aes(x = lmf_a, y = lmf_b, color = mean_f_score)) +
  geom_point() +
  facet_wrap(~forest_ty) +
  scale_color_viridis_c() +
  theme_bw()

p

ggsave(
  filename = paste0("paramID_plots_second_refined/vege_cover", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")


#################################
mid_clump = median(na.omit(d2$Clumping_Index))


# Summarize the f-score by parameter set, across all plots and low- and high-density plots separately
d_fig_overall = d2|>
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
  summarize(allplots = mean(f_score),
            n_plots = n()) |>
  ungroup()

d_fig_lowclump = d2 |>
  filter(Clumping_Index < mid_clump) |>
  group_by(paramset_id) |>
  summarize(lowclump = mean(f_score),
            n_plots_lowdens = n()) |>
  ungroup()

d_fig_highclump = d2 |>
  filter(Clumping_Index >= mid_clump) |>
  group_by(paramset_id) |>
  summarize(higclump = mean(f_score),
            n_plots_highdens = n()) |>
  ungroup()

d_fig_lowhigh = left_join(d_fig_lowclump, d_fig_highclump, by = join_by("paramset_id" == "paramset_id"))
d_fig = left_join(d_fig_overall, d_fig_lowhigh, by = join_by("paramset_id" == "paramset_id"))

d_fig_long = d_fig |>
  pivot_longer(cols = c("allplots", "lowclump", "higclump"),
               names_to = "f_score_type",
               values_to = "f_score")

p = ggplot(d_fig_long, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point() +
  facet_wrap(~f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

p

ggsave(
  filename = paste0("paramID_plots_second_refined/clumping", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")
##############################

mid_sdh = median(na.omit(d2$sd_height))


# Summarize the f-score by parameter set, across all plots and low- and high-density plots separately
d_fig_overall = d2|>
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
  summarize(allplots = mean(f_score),
            n_plots = n()) |>
  ungroup()

d_fig_lowsdh = d2 |>
  filter(sd_height < mid_sdh) |>
  group_by(paramset_id) |>
  summarize(lowsdh = mean(f_score),
            n_plots_lowdens = n()) |>
  ungroup()

d_fig_highsdh = d2 |>
  filter(sd_height >= mid_sdh) |>
  group_by(paramset_id) |>
  summarize(higsdh = mean(f_score),
            n_plots_highdens = n()) |>
  ungroup()

d_fig_lowhigh = left_join(d_fig_lowsdh, d_fig_highsdh, by = join_by("paramset_id" == "paramset_id"))
d_fig = left_join(d_fig_overall, d_fig_lowhigh, by = join_by("paramset_id" == "paramset_id"))

d_fig_long = d_fig |>
  pivot_longer(cols = c("allplots", "lowsdh", "higsdh"),
               names_to = "f_score_type",
               values_to = "f_score")

p = ggplot(d_fig_long, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point() +
  facet_wrap(~f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

p

ggsave(
  filename = paste0("paramID_plots_second_refined/sdh", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")

###############################


merged_table_10cm$source <- "10cm"
merged_table_25$source <- "25cm"

df_combined <- rbind(merged_table_10cm, merged_table_25)


d_fig_overall_10cm = merged_table_10cm |>
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
  summarize(allplots = mean(f_score),
            n_plots = n()) |>
  ungroup()


d_fig_overall_25cm = merged_table_25 |>
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
  summarize(allplots = mean(f_score),
            n_plots = n()) |>
  ungroup()

d_fig_overall_10cm$source <- "10cm"
d_fig_overall_25cm$source <- "25cm"

df_combined_2 <- rbind(d_fig_overall_10cm, d_fig_overall_25cm)




library(ggplot2)

ggplot(df_combined_2 , aes(x = paramset_id, y = allplots, color = source)) +
  geom_point(size = 1) +
  # geom_line(aes(group = source), color = "gray70", linetype = "dashed") +  # connect points from the same ID
  labs(title = "Variation between 10 cm and 25 cm chms",
       x = "paramset_ID",
       y = "f_score",
       color = "Source") +
  theme_minimal()


ggplot(df_combined_2 , aes(x = lmf_a, y = allplots, color = source)) +
  geom_point(size = 1) +
  # geom_line(aes(group = source), color = "gray70", linetype = "dashed") +  # connect points from the same ID
  labs(title = "Variation between 10 cm and 25 cm chms",
       x = "lmf_a",
       y = "f_score",
       color = "Source") +
  theme_minimal()

ggplot(df_combined_2, aes(x = lmf_b, y = allplots, color = source)) +
  geom_point(size = 1) +
  # geom_line(aes(group = source), color = "gray70", linetype = "dashed") +  # connect points from the same ID
  labs(title = "Variation between 10 cm and 25 cm chms",
       x = "lmf_b",
       y = "f_score",
       color = "Source") +
  theme_minimal()


#################
ggplot(new_data , aes(x = lmf_a, y = fscore_diff)) +
  geom_point(size = 1) +
  # geom_line(aes(group = source), color = "gray70", linetype = "dashed") +  # connect points from the same ID
  labs(title = "Variation between 10 cm and 25 cm chms",
       x = "lmf_a",
       y = "f_score_diff"
  ) +
  theme_minimal()


ggplot(new_data , aes(x = lmf_b, y = fscore_diff)) +
  geom_point(size = 1) +
  # geom_line(aes(group = source), color = "gray70", linetype = "dashed") +  # connect points from the same ID
  labs(title = "Variation between 10 cm and 25 cm chms",
       x = "lmf_b",
       y = "f_score_diff"
  ) +
  theme_minimal()

################################
library(dplyr)

# Filter top 10% based on the 'value' column
top_90th_percent_10cm <- merged_table %>%
  filter(f_score >= quantile(f_score, 0.90, na.rm = TRUE))

top_75th_percent_10cm <- merged_table_10cm %>%
  filter(f_score >= quantile(f_score, 0.75, na.rm = TRUE))

top_75th_percent_10cm <- merged_table_10cm %>%
  filter(f_score >= quantile(f_score, 0.75, na.rm = TRUE))

bottom_10_percent_10cm <- merged_table_10cm %>%
  filter(f_score <= quantile(f_score, 0.10, na.rm = TRUE))


d_fig_top_90th_percent_10cm = top_90th_percent_10cm |>
  group_by(paramset_id, plot_id, f_score,lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
  summarize(allplots = mean(f_score),
            n_plots = n()) |>
  ungroup()

p = ggplot(d_fig_top_90th_percent_10cm , aes(x = lmf_a, y = allplots)) +
  geom_point(size = 1) +
  # geom_line(aes(group = source), color = "gray70", linetype = "dashed") +  # connect points from the same ID
  labs(title = "Variation f_score_top 10%",
       x = "lmf_a",
       y = "f_score",
       color = "Source") +
  theme_minimal()

p


p = ggplot(d_fig_top_90th_percent_10cm, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point(size = 1) +
  labs(title = "Variation f_score_top 10%",
       x = "lmf_a",
       y = "lmf_b",
       color = "Source") +
  theme_minimal() +
  scale_color_viridis_c() 

p

ggsave(
  filename = paste0("paramID_plots_second_refined/top_10_percent", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")


# Best paramset overall (highest average f_score across all plots)
best_overall <- merged_table %>%
  group_by(paramset_id) %>%
  summarise(avg_f_score = mean(f_score)) %>%
  arrange(desc(avg_f_score))

# View top 10
head(best_overall, 10)


# Best paramset per plot (highest f_score per plot_id)
best_per_plot <- merged_table %>%
  group_by(plot_id) %>%
  slice_max(f_score, n = 1, with_ties = FALSE) %>%
  ungroup()


library(ggplot2)

p = ggplot(best_per_plot, aes(x = plot_id, y = f_score, fill = paramset_id)) +
  geom_col() +
  labs(title = "Best paramset per plot (based on f_score)",
       x = "Plot ID", y = "F-score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p

ggsave(
  filename = paste0("paramID_plots_second_refined/bets_per_plot", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")



top10 <- best_overall %>% slice_max(avg_f_score, n = 10)

p = ggplot(top10, aes(x = reorder(paramset_id, avg_f_score), y = avg_f_score)) +
  geom_point(fill = "steelblue") +
  labs(title = "Top 10 Paramsets by Average F-score",
       x = "Paramset ID", y = "Avg F-score") +
  coord_flip() +
  theme_minimal()

p

ggsave(
  filename = paste0("paramID_plots_second_refined/top_10_overall", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")



top_sample <- merged_table %>%
  group_by(paramset_id) %>%
  summarise(avg_f_score = mean(f_score)) %>%
  arrange(desc(avg_f_score)) %>%
  slice_head(n = 10)  # or more


top_params <- d2 %>%
  filter(paramset_id %in% top_sample$paramset_id)


# Filter full grid by the parameter constraints
filtered_grid <- paramsets %>%
  filter(lmf_a >= 0.26, lmf_a <= 1.14,
         lmf_b >= 0.04, lmf_b <= 0.08)

# Randomly sample 100 rows from the filtered grid
sampled_params <- filtered_grid %>%
  slice_sample(n = 100)


write.csv(sampled_params,"/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/itd-paramsets_refined_7005.csv")



plot_infor <- read_csv("/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/field-plot_drone-mission_crosswalk.csv")


#plot 26 and plot 91 has top and lowest performace










# Plot frequency of plot_id
p = ggplot(d_fig_top_90th_percent_10cm, aes(x = plot_id)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Frequency of plot_id", x = "Plot ID", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p

ggsave(
  filename = paste0("paramID_plots_second_refined/plot_at_top_10_percent", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")


# Plot frequency of paramset_id
p = ggplot(d_fig_top_90th_percent_10cm, aes(x = paramset_id)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Frequency of paramset_id", x = "Paramset ID", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


p

ggsave(
  filename = paste0("paramID_plots_second_refined/param_ids_at_top_10_percent", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white")


# ggplot(d_fig_overall_top_75th_10cm , aes(x = lmf_b, y = allplots)) +
#   geom_point(size = 1) +
#   # geom_line(aes(group = source), color = "gray70", linetype = "dashed") +  # connect points from the same ID
#   labs(title = "Variation f_score_top 25%",
#        x = "lmf_b",
#        y = "f_score",
#        color = "Source") +
#   theme_minimal()
###############################

dens = field_ref |>
  select(plot_id = field_plot_id, obs_tree_density = tph, min_ht = min_ht,min_ht_ohvis = min_ht_ohvis, forest_type_dy = forest_type_dy) |>
  mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))

dens = na.omit(dens)

dens <- na.omit(dens)

d2 = left_join(d, dens, by = join_by("plot_id" == "plot_id"))
d2 = na.omit(d2)




#####################################################################

library(ggplot2)
library(dplyr)

# Example dataframe: results_df
# Columns: plot_ID, a, b, f_score

# Create a directory to save plots
dir.create("parameter_plots_before_refine", showWarnings = FALSE)

dens2 = field_ref |>
  dplyr::select(plot_id = field_plot_id) |>
  mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))

field_ref <- cbind(field_ref,dens2)
# field_ref <- field_ref[,-c(1,22)]


merged_all = merged_table |>
  left_join(field_ref, by = join_by("plot_id" == "plot_id"))
# Get unique plot IDs
plot_ids <- unique(merged_all$plot_id)

# Loop through each plot_ID
for (pid in plot_ids) {
  
  print(pid)
  
  # Filter data for current plot_ID
  df_plot <- merged_all %>% filter(plot_id == pid)
  
  # Generate the plot
  p <- ggplot(df_plot, aes(x = lmf_a, y = lmf_b, color = f_score)) +
    geom_point(size = 2, alpha = 0.8) +
    scale_color_viridis_c(option = "D", name = "F-score") +
    labs(
      title = paste("Parameter Space for Plot", pid),
      x = "Parameter a",
      y = "Parameter b"
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(
    filename = paste0("paramID_plots_second_refined/plot_", pid, ".png"),
    plot = p,
    width = 6, height = 5, dpi = 300,
    bg = "white"
  )
}



library(ggplot2)
library(dplyr)

# Sample 10 parameter_IDs (adjust n as needed)
set.seed(42)
sample_param_ids <- sample(unique(merged_all$paramset_id), size = 10)

# Create output directory
dir.create("paramID_plots", showWarnings = FALSE)


df_param <- merged_all[merged_all$paramset_id %in% sample_param_ids,]

# Extract unique a, b values for title
param_a <- unique(df_param$lmf_a)
param_b <- unique(df_param$lmf_b)

# Plot: Each point is a plot using this parameter set
p <- ggplot(df_param, aes(x = lmf_a, y = lmf_b, color = f_score, size = Clumping_Index)) +
  geom_point(alpha = 0.9) +
  scale_color_viridis_c(name = "F-score", option = "D") +
  scale_size_continuous(name = "Clumping_Index") +
  labs(
    title = paste0("Performance Across Plots "),
    # subtitle = paste0("a = ", round(param_a, 3), ", b = ", round(param_b, 3)),
    x = "Parameter a",
    y = "Parameter b"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10)
  )

p
# Save with white background
ggsave(
  filename = paste0("paramID_plots/param_clumping", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)


best_overall <- merged_all %>%
  group_by(paramset_id) %>%
  summarise(avg_f_score = mean(f_score)) %>%
  arrange(desc(avg_f_score))


top20 <- best_overall %>% slice_max(avg_f_score, n = 20)


# Best paramset per plot (highest f_score per plot_id)
best_per_plot <- merged_all %>%
  group_by(plot_id) %>%
  slice_max(f_score, n = 1, with_ties = FALSE) %>%
  ungroup()


# Extract unique a, b values for title
param_a <- unique(best_per_plot$lmf_a)
param_b <- unique(best_per_plot$lmf_b)

# # Plot: Each point is a plot using this parameter set
# p <- ggplot(best_per_plot, aes(x = lmf_a, y = lmf_b, color = f_score, size = Clumping_Index)) +
#   geom_point(alpha = 0.9) +
#   scale_color_viridis_c(name = "F-score", option = "D") +
#   scale_size_continuous(name = "Clumping_Index") +
#   labs(
#     title = paste0("Performance Across best Plots "),
#     # subtitle = paste0("a = ", round(param_a, 3), ", b = ", round(param_b, 3)),
#     x = "Parameter a",
#     y = "Parameter b"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(face = "bold", size = 12),
#     plot.subtitle = element_text(size = 10)
#   )


p <- ggplot(best_per_plot, aes(x = lmf_a, y = lmf_b, color = f_score, size = Clumping_Index)) +
  geom_point(alpha = 0.9) +
  geom_text(aes(label = plot_id), vjust = -0.8, size = 3, check_overlap = TRUE) +
  scale_color_viridis_c(name = "F-score", option = "D") +
  scale_size_continuous(name = "Clumping_Index") +
  labs(
    title = "Performance Across Best Plots",
    x = "Parameter a",
    y = "Parameter b"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10)
  )


p
# Save with white background
ggsave(
  filename = paste0("paramID_plots/best_plots_param_clumping", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)


p <- ggplot(best_per_plot, aes(x = lmf_a, y = lmf_b, color = f_score, size = max_height)) +
  geom_point(alpha = 0.9) +
  geom_text(aes(label = plot_id), vjust = -0.8, size = 3, check_overlap = TRUE) +
  scale_color_viridis_c(name = "F-score", option = "D") +
  scale_size_continuous(name = "max height") +
  labs(
    title = "Performance Across Best Plots",
    x = "Parameter a",
    y = "Parameter b"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10)
  )


p
# Save with white background
ggsave(
  filename = paste0("paramID_plots/best_plots_param_height", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)


p <- ggplot(best_per_plot, aes(x = lmf_a, y = lmf_b, color = f_score, size = dbh_mean)) +
  geom_point(alpha = 0.9) +
  geom_text(aes(label = plot_id), vjust = -0.8, size = 3, check_overlap = TRUE) +
  scale_color_viridis_c(name = "F-score", option = "D") +
  scale_size_continuous(name = "mean_dbh") +
  labs(
    title = "Performance Across Best Plots",
    x = "Parameter a",
    y = "Parameter b"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10)
  )


p
# Save with white background
ggsave(
  filename = paste0("paramID_plots/best_plots_param_dbh", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

# Get number of forest type classes
n_classes <- length(unique(best_per_plot$forest_type_dy))

# Pick n_shapes >= n_classes, from integers 0–25
my_shapes <- c(0, 1, 2, 3, 4, 5, 6, 7)  # extend this if needed

p <- ggplot(best_per_plot, aes(x = lmf_a, y = lmf_b,
                               color = f_score,
                               shape = as.factor(forest_type_dy))) +
  geom_point(size = 3, alpha = 0.9, na.rm = TRUE) +
  geom_text(aes(label = plot_id), vjust = -0.8, size = 3, check_overlap = TRUE) +
  scale_color_viridis_c(name = "F-score", option = "D") +
  scale_shape_manual(
    name = "Forest Type",
    values = my_shapes[1:n_classes]  # trim to number of classes
  ) +
  labs(
    title = "Performance Across Best Plots",
    x = "Parameter a",
    y = "Parameter b"
  ) +
  theme_minimal()




p
# Save with white background
ggsave(
  filename = paste0("paramID_plots/best_plots_param_forest_type", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

library(mgcv)

m <- gam(
  f_score ~ s(lmf_a) + s(lmf_b) + forest_type_dy +
    s(tph) + s(mean_height) + s(Clumping_Index),
  data = merged_all,
  method = "REML"
)

gam.check(m)
summary(m)

plot(m, scheme = 1)


m <- gam(
  f_score ~ s(lmf_a) + s(lmf_b) ,
  data = merged_all,
  method = "REML"
)

gam.check(m)
summary(m)



m <- gam(
  f_score ~ s(lmf_a) + s(lmf_b) + forest_type_dy 
  ,
  data = merged_all,
  method = "REML"
)

gam.check(m)
summary(m)


m <- gam(
  f_score ~ s(lmf_a) + s(lmf_b) + forest_type_dy +
    s(tph),
  data = merged_all,
  method = "REML"
)

gam.check(m)
summary(m)

m <- gam(
  f_score ~ s(lmf_a) + s(lmf_b) + forest_type_dy +
    s(tph) + s(mean_height),
  data = merged_all,
  method = "REML"
)

gam.check(m)
summary(m)


m <- gam(
  f_score ~ s(lmf_a) + s(lmf_b) + forest_type_dy +
    s(tph) +  s(Clumping_Index),
  data = merged_all,
  method = "REML"
)

gam.check(m)
summary(m)



library(dplyr)

best_params <- merged_all %>%
  filter(!is.na(f_score)) %>%
  group_by(forest_type_dy, mean_height, tph) %>%
  slice_max(order_by = f_score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(forest_type_dy, mean_height, tph, lmf_a, lmf_b, f_score)

write_csv(best_params, paste0("paramID_plots/best_param_groups", ".csv"))
