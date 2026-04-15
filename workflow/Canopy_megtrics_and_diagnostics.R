################################################################################
# PACKAGE INSTALLATION AND LIBRARIES
################################################################################

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

################################################################################
################################################################################
# REQUIRED PACKAGES
################################################################################
################################################################################

packages <- c("sf", "dplyr", "data.table", "stringr", "FNN", "dbscan", "purrr", "tibble")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

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

################################################################################
# CONSTANTS AND FILE PATHS
################################################################################

MATCH_STATS_DIR = "/ofo-share/project-data/ofo-itd-crossmapping_data/drone/predicted-tree-evals/"
ITD_PARAMS_DEF_DIR = "/ofo-share/project-data/ofo-itd-crossmapping_data/itd-paramsets/"
EVAL_FIGURES_DIR = "/ofo-share/project-data/ofo-itd-crossmapping_data/itd-paramset-eval-figures/"

PLOTS_EXCLUDE = c("0015", "0046", "0100", "0110")
FOC_PARAMGROUP = "7010-1"

dir.create("paramID_plots_1_22_26", showWarnings = FALSE)

################################################################################
# LOAD MATCH STATS AND PARAMETER DEFINITIONS
################################################################################

filename = "smooth_10cm_7002_pred_results.csv"
d = read_csv("/ofo-share/project-data/ofo-itd-crossmapping_data/drone/predicted-tree-evals/match-stats_paramgroup-7010-1.csv")

d = d |>
  filter(!plot_ID %in% PLOTS_EXCLUDE)

filename = paste0("itd-paramsets_", paste(FOC_PARAMGROUP, collapse = "-"), ".csv")
param_defs = read_csv("/ofo-share/project-data/ofo-itd-crossmapping_data/itd-paramsets/itd-paramsets_7010-1.csv")

d = d |>
  left_join(param_defs, by = join_by("paramset_id" == "paramset_id"))

################################################################################
# GPKG METRICS
################################################################################

GPKG_DIR <- "/ofo-share/project-data/ofo-itd-crossmapping_data/field-reference/aligned/trees/"
gpkg_files <- list.files(GPKG_DIR, pattern = "\\.gpkg$", full.names = TRUE)

compute_chi_points <- function(df, k = 3) {
  df_clean <- df %>% filter(!is.na(X), !is.na(Y), !is.na(height))
  
  coords <- as.matrix(df_clean[, c("X", "Y")])
  heights <- df_clean$height
  
  if (nrow(coords) < k) return(NA)
  
  knn_result <- FNN::get.knn(coords, k = k)
  chi_vals <- sapply(1:nrow(knn_result$nn.index), function(i) {
    sd(heights[knn_result$nn.index[i, ]], na.rm = TRUE)
  })
  
  mean(chi_vals, na.rm = TRUE)
}

compute_clumping_index_points <- function(df, threshold = 2, eps = 1, minPts = 3) {
  canopy_points <- df %>% filter(height > threshold)
  total_points <- nrow(df)
  
  if (nrow(canopy_points) < minPts) return(0)
  
  coords <- as.matrix(canopy_points[, c("X", "Y")])
  db <- dbscan::dbscan(coords, eps = eps, minPts = minPts)
  
  num_clumps <- length(unique(db$cluster[db$cluster > 0]))
  clumping_index <- num_clumps / total_points
  return(clumping_index)
}

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

results_gpkg <- map_df(gpkg_files, function(file) {
  print(file)
  pts <- sf::st_read(file, quiet = TRUE)
  pts = pts[pts$height>=5,]
  df <- sf::st_drop_geometry(pts)
  
  if (!"height" %in% names(df)) {
    if ("canopy_height" %in% names(df)) {
      df$height <- df$canopy_height
    } else {
      stop(paste("No height column found in", file))
    }
  }
  
  coords <- sf::st_coordinates(pts)
  df$X <- coords[, 1]
  df$Y <- coords[, 2]
  
  chi <- compute_chi_points(df)
  clumping <- compute_clumping_index_points(df)
  height_metrics <- compute_height_metrics_points(df)
  
  tibble(
    file = basename(file),
    CHI = chi,
    Clumping_Index = clumping
  ) %>% bind_cols(height_metrics)
})

results_gpkg <- results_gpkg %>%
  mutate(plot_id = str_pad(parse_number(file), width = 4, pad = "0"))

print(results_gpkg)

################################################################################
# MERGE MATCH STATS WITH GPKG METRICS
################################################################################

library(dplyr)

if ("plot_ID" %in% colnames(d)) {
  d <- d %>% rename(plot_id = plot_ID)
}

merged_table <- d %>%
  left_join(results_gpkg, by = "plot_id")

library(dplyr)
library(stringr)

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
# merged_table3 = merged_table[merged_table$media_height>5,]
# 
# merged_table4 = merged_table[merged_table$precision==1.0,]

# ## Bind the parameter definitions to the match stats
# ## Load the parameter set definitions
# filename = paste0("itd-paramsets_", paste(FOC_PARAMGROUP, collapse = "-"), ".csv")
# param_defs = read_csv(file.path(ITD_PARAMS_DEF_DIR, filename))
# 
# merged_table = d |>
#   left_join(param_defs, by = join_by("paramset_id" == "paramset_id"))

################################################################################
# LONG FORMAT FOR PARAMETER VISUALIZATION
################################################################################

d_long = merged_table |>
  pivot_longer(cols = starts_with("lmf_"), names_to = "param", values_to = "value")

d_long = d_long |>
  left_join(param_defs, by = join_by("paramset_id" == "paramset_id"))

################################################################################
# VIS OPTION 1: SCATTER PLOT BY PARAMETER
################################################################################

d_plot = d_long |>
  filter(plot_id %in% c("0005"))

p = ggplot(d_plot, aes(x = value,
                       y = f_score,
                       color = lmf_diam_max,
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

p = ggplot(d_long, aes(x = value,
                       y = f_score,
                       color = f_score,
                       label1 = lmf_a,
                       label2 = lmf_b,
                       label6 = plot_id)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(end = 0.85) +
  facet_grid(plot_id~param, scales = "free")
p

ggsave(
  filename = paste0("summary_7010-1_4_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

# ## VIS OPTION 2: Scatter plot by parameter, with plotly for interactive parameter lookup for each point in the plot
# plotly = ggplotly(p, tooltip = c("label1", "label2", "label3", "label4", "label5", "label6"))
# filename = paste0("smooth_10cm_paramgroup-", FOC_PARAMGROUP, "plot05.html")
# saveWidget(plotly, file.path(EVAL_FIGURES_DIR, filename), selfcontained = TRUE, libdir = "lib")
# 
# ## VIS OPTION 3: Fit a GAM to the data and visualize the partial effects of the parameters on the f-score
# d_mod = d |>
#   filter(plot_id == "0007")
# 
# m = gam(f_score ~ s(lmf_a) + s(lmf_b) + s(lmf_c) + s(lmf_diam_min) + s(lmf_diam_max), data = d_mod)
# plot(m, scheme = 1)

################################################################################
# FIELD REFERENCE
################################################################################

FIELD_REF <- "/ofo-share/project-data/ofo-itd-crossmapping_data/site-selection/processed/selected-field-and-drone-plots_v1.csv"

################################################################################
# VIS OPTION 5: PARAMSET PERFORMANCE BY TREE DENSITY
################################################################################

field_ref = read_csv(FIELD_REF)
field_ref <- field_ref[-c(27,28),]

dens = field_ref |>
  dplyr::select(plot_id = field_plot_id, obs_tree_density = tph) |>
  mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))

dens <- na.omit(dens)

d2 = left_join(merged_table, dens, by = join_by("plot_id" == "plot_id"))
d2 = na.omit(d2)

mid_dens = median(na.omit(d2$obs_tree_density))

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

library(viridisLite)

p <- ggplot(d_fig_long, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point(size = 2.5, alpha = 0.9) +
  facet_wrap(~f_score_type) +
  scale_color_viridis_c(
    option = "turbo",
    limits = c(0.4, 0.9),
    oob = scales::squish,
    breaks = seq(0.4, 0.9, by = 0.1),
    name = "F-score"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 14),
    strip.text = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 13),
    strip.background = element_rect(fill = "grey90", color = "grey60")
  )

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/low_high_tph_7010-1_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

################################################################################
# VIS: PERFORMANCE BY HEIGHT
################################################################################

# > colnames(field_ref)
# [1] "field_plot_id"             "project_name"              "survey_year"               "plot_area"                 "min_dbh"                   "min_ht"                    "min_ht_ohvis"             
# [8] "num_ohvis_trees_excluded"  "tph"                       "dbh_mean"                  "dbh_cv"                    "forest_type"               "forest_type_dy"            "top_species"              
# [15] "contributor_field_plot_id" "drone_dataset_id"          "drone_imagery_year"        "tph_cat"                   "dbh_cat"                   "selected_drone_dataset_id" "...21"                    

# min_height_d = field_ref |>
#   select(plot_id = field_plot_id, min_height = min_ht) |>
#   mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))
# 
# min_height_d <- na.omit(min_height_d)
# 
# d3 = left_join(d, min_height_d, by = join_by("plot_id" == "plot_id"))
# d3 = na.omit(d3)

mid_mht = median(na.omit(merged_table$media_height))

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
  geom_point(size = 2.5, alpha = 0.9) +
  facet_wrap(~f_score_type) +
  scale_color_viridis_c(
    option = "turbo",
    limits = c(0.4, 0.9),
    oob = scales::squish,
    breaks = seq(0.4, 0.9, by = 0.1),
    name = "F-score"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 14),
    strip.text = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 13),
    strip.background = element_rect(fill = "grey90", color = "grey60")
  )

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/low_high_height-7010-1_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

################################################################################
# VIS: PERFORMANCE BY DBH
################################################################################

# min_ht_ohvis_d = field_ref |>
#   select(plot_id = field_plot_id, min_ht_ohvis_x = min_ht_ohvis) |>
#   mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))
# 
# min_ht_ohvis_d <- na.omit(min_ht_ohvis_d)
# 
# d4 = left_join(d, min_ht_ohvis_d, by = join_by("plot_id" == "plot_id"))
# d4 = na.omit(d4)

mid_dbh = median(na.omit(d2$media_dbh))

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
  geom_point(size = 2.5, alpha = 0.9) +
  facet_wrap(~f_score_type) +
  scale_color_viridis_c(
    option = "turbo",
    limits = c(0.4, 0.9),
    oob = scales::squish,
    breaks = seq(0.4, 0.9, by = 0.1),
    name = "F-score"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 14),
    strip.text = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 13),
    strip.background = element_rect(fill = "grey90", color = "grey60")
  )

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/low_high_dbh-7010-1_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

################################################################################
# VIS: PERFORMANCE BY VEGETATION COVER / FOREST TYPE
################################################################################

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
  geom_point(size = 1.5, alpha = 0.9) +
  facet_wrap(~forest_ty) +
  scale_color_viridis_c(
    option = "turbo",
    limits = c(0.4, 0.9),
    oob = scales::squish,
    breaks = seq(0.4, 0.9, by = 0.1),
    name = "F-score"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12),
    strip.text = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    strip.background = element_rect(fill = "grey90", color = "grey60")
  )

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/vege_cover-7010-1_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

################################################################################
# VIS: PERFORMANCE BY CLUMPING
################################################################################

mid_clump = median(na.omit(d2$Clumping_Index))

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
  geom_point(size = 2.5, alpha = 0.9) +
  facet_wrap(~f_score_type) +
  scale_color_viridis_c(
    option = "turbo",
    limits = c(0.4, 0.9),
    oob = scales::squish,
    breaks = seq(0.4, 0.9, by = 0.1),
    name = "F-score"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 13),
    strip.background = element_rect(fill = "grey90", color = "grey60")
  )

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/clumping-7010-1_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

################################################################################
# VIS: PERFORMANCE BY HEIGHT SD
################################################################################

mid_sdh = median(na.omit(d2$sd_height))

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
  geom_point(size = 2.5, alpha = 0.9) +
  facet_wrap(~f_score_type) +
  scale_color_viridis_c(
    option = "turbo",
    limits = c(0.4, 0.9),
    oob = scales::squish,
    breaks = seq(0.4, 0.9, by = 0.1),
    name = "F-score"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 13),
    strip.background = element_rect(fill = "grey90", color = "grey60")
  )

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/sdh-7010-1_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

# ###############################
# merged_table_10cm$source <- "10cm"
# merged_table_25$source <- "25cm"
# 
# df_combined <- rbind(merged_table_10cm, merged_table_25)
# 
# d_fig_overall_10cm = merged_table_10cm |>
#   group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
#   summarize(allplots = mean(f_score),
#             n_plots = n()) |>
#   ungroup()
# 
# d_fig_overall_25cm = merged_table_25 |>
#   group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
#   summarize(allplots = mean(f_score),
#             n_plots = n()) |>
#   ungroup()
# 
# d_fig_overall_10cm$source <- "10cm"
# d_fig_overall_25cm$source <- "25cm"
# 
# df_combined_2 <- rbind(d_fig_overall_10cm, d_fig_overall_25cm)
# 
# library(ggplot2)
# 
# ggplot(df_combined_2 , aes(x = paramset_id, y = allplots, color = source)) +
#   geom_point(size = 1) +
#   labs(title = "Variation between 10 cm and 25 cm chms",
#        x = "paramset_ID",
#        y = "f_score",
#        color = "Source") +
#   theme_minimal()
# 
# ggplot(df_combined_2 , aes(x = lmf_a, y = allplots, color = source)) +
#   geom_point(size = 1) +
#   labs(title = "Variation between 10 cm and 25 cm chms",
#        x = "lmf_a",
#        y = "f_score",
#        color = "Source") +
#   theme_minimal()
# 
# ggplot(df_combined_2, aes(x = lmf_b, y = allplots, color = source)) +
#   geom_point(size = 1) +
#   labs(title = "Variation between 10 cm and 25 cm chms",
#        x = "lmf_b",
#        y = "f_score",
#        color = "Source") +
#   theme_minimal()

# #################
# ggplot(new_data , aes(x = lmf_a, y = fscore_diff)) +
#   geom_point(size = 1) +
#   labs(title = "Variation between 10 cm and 25 cm chms",
#        x = "lmf_a",
#        y = "f_score_diff"
#   ) +
#   theme_minimal()
# 
# ggplot(new_data , aes(x = lmf_b, y = fscore_diff)) +
#   geom_point(size = 1) +
#   labs(title = "Variation between 10 cm and 25 cm chms",
#        x = "lmf_b",
#        y = "f_score_diff"
#   ) +
#   theme_minimal()

################################################################################
# TOP 10% F-SCORES
################################################################################

library(dplyr)

top_90th_percent_10cm <- merged_table %>%
  filter(f_score >= quantile(f_score, 0.90, na.rm = TRUE))

# top_75th_percent_10cm <- merged_table_10cm %>%
#   filter(f_score >= quantile(f_score, 0.75, na.rm = TRUE))
# 
# top_75th_percent_10cm <- merged_table_10cm %>%
#   filter(f_score >= quantile(f_score, 0.75, na.rm = TRUE))
# 
# bottom_10_percent_10cm <- merged_table_10cm %>%
#   filter(f_score <= quantile(f_score, 0.10, na.rm = TRUE))

d_fig_top_90th_percent_10cm = top_90th_percent_10cm |>
  group_by(paramset_id, plot_id, f_score,lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
  summarize(allplots = mean(f_score),
            n_plots = n()) |>
  ungroup()

p = ggplot(d_fig_top_90th_percent_10cm , aes(x = lmf_a, y = allplots)) +
  geom_point(size = 1) +
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
  geom_point(size = 2.5, alpha = 0.9) +
  scale_color_viridis_c(
    option = "turbo",
    limits = c(0.8, 0.9),
    oob = scales::squish,
    breaks = seq(0.8, 0.9, by = 0.1),
    name = "F-score"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 13),
    strip.background = element_rect(fill = "grey90", color = "grey60")
  )

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/top_10_percent-7010-1_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

################################################################################
# TOP FOR EACH PLOT
################################################################################

library(dplyr)

filtered_df <- merged_table %>%
  group_by(plot_id) %>%
  slice_max(order_by = f_score, n = 1) %>%
  ungroup()

library(ggplot2)

filtered_df_1 <- filtered_df %>%
  mutate(plot_id = factor(plot_id),
         paramset_id = factor(paramset_id))

plot_dat <- filtered_df_1 %>%
  mutate(f_score_r = round(f_score, 3)) %>%
  add_count(f_score_r, name = "tie_count")

n_plots   <- nlevels(plot_dat$plot_id)
my_shapes <- 0:25

cols <- scales::hue_pal(l = 65, c = 100)(nlevels(filtered_df_1$plot_id))
names(cols) <- levels(filtered_df_1$plot_id)

p <- ggplot(plot_dat, aes(x = plot_id, y = f_score)) +
  geom_point(aes(color = f_score), size = 3.5, shape = 16, alpha = 0.9) +
  geom_label(
    data = subset(plot_dat, tie_count > 1),
    aes(label = tie_count),
    size = 3,
    label.size = 0,
    label.padding = unit(0.12, "lines"),
    vjust = -0.3
  ) +
  coord_flip() +
  labs(
    title = "Best F_score per plot",
    subtitle = "Numbers indicate ties (count of parameter sets scoring the best F-score)",
    x = "Plot ID", y = "F-score"
  ) +
  scale_y_continuous(limits = c(0.6, 1), breaks = seq(0.6, 1, 0.05)) +
  scale_color_viridis_c(
    option = "turbo",
    limits = c(0.6, 1),
    oob = scales::squish,
    breaks = seq(0.6, 1, by = 0.05),
    name = "F-score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(size = 11)
  )

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/bets_per_plot-70101-1_all_same_f_scores_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

################################################################################
# SINGLE PLOT PARAMSET VIEW
################################################################################

filtered_df_098<- filtered_df %>%
  filter(plot_id == "0098") %>%
  mutate(paramset_id = factor(paramset_id)) %>%
  droplevels()

p = ggplot(filtered_df_098 , aes(x = lmf_a, y = lmf_b, color = as.factor(paramset_id))) +
  geom_point(size = 1) +
  labs(title = "top F_score",
       x = "lmf_a",
       y = "lmf_b",
       color = "Source") +
  theme_minimal()

p

################################################################################
# BEST PARAMSET PER PLOT
################################################################################

filtered_df2 <- merged_table %>%
  group_by(plot_id) %>%
  slice_max(order_by = f_score, n = 1, with_ties = FALSE) %>%
  ungroup()

library(ggplot2)
library(scales)

filtered_df2$paramset_id <- factor(filtered_df2$paramset_id)
filtered_df2$plot_id     <- factor(filtered_df2$plot_id)

n_plots   <- length(levels(filtered_df2$plot_id))
my_shapes <- 0:25

cols <- hue_pal(l = 65, c = 100)(nlevels(filtered_df2$paramset_id))
names(cols) <- levels(filtered_df2$paramset_id)

# p <- ggplot(filtered_df2, aes(x = plot_id, y = f_score,
#                               shape = plot_id, color = paramset_id)) +
#   geom_point(size = 3, stroke = 1) +
#   geom_text(aes(label = round(f_score, 3)), vjust = -0.8, size = 3) +
#   scale_shape_manual(values = my_shapes[1:n_plots]) +
#   scale_color_manual(values = cols, name = "Parameter Set ID") +
#   labs(title = "Best paramset per plot (based on f_score)",
#        x = "Plot ID", y = "F-score", shape = "Plot ID") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# p

p <- ggplot(filtered_df2,
            aes(x = plot_id, y = f_score, shape = paramset_id)) +
  geom_point(size = 3, stroke = 1, color = "black") +
  geom_text(aes(label = round(f_score, 3)), vjust = -0.8, size = 3) +
  scale_shape_manual(values = my_shapes, name = "Parameter Set ID") +
  labs(title = "Best paramset per plot (based on F-score)",
       x = "Plot ID", y = "F-score") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/best_param_id_per_plot_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

################################################################################
# PARAMETER LABELS AND HEIGHT-DEPENDENT WINDOW CURVES
################################################################################

# param_labels <- filtered_df2 %>%
#   distinct(paramset_id, lmf_a, lmf_b) %>%
#   mutate(
#     label = paste0(
#       paramset_id,
#       ": a=", round(lmf_a, 2),
#       ", b=", round(lmf_b, 3)
#     )
#   )
# 
# label_text <- paste(param_labels$label, collapse = "\n")

param_labels <- filtered_df2 %>%
  distinct(paramset_id, lmf_a, lmf_b) %>%
  arrange(paramset_id) %>%
  mutate(
    label = paste0(
      paramset_id,
      ":  a = ", round(lmf_a, 2),
      ",  b = ", round(lmf_b, 3)
    )
  )

label_text <- paste(param_labels$label, collapse = "\n")

h_seq <- tibble(height = seq(0, 80, by = 1))

curves <- filtered_df2 %>%
  crossing(h_seq) %>%
  mutate(
    window_raw = lmf_a + lmf_b * height + lmf_c,
    window = pmin(pmax(window_raw, lmf_diam_min), lmf_diam_max)
  )

p <- ggplot(curves, aes(x = height, y = window, color = paramset_id)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Tree height (m)",
    y = "LMF window diameter (m)",
    color = "Paramset ID",
    title = "Height-dependent LMF window size for representative parameter sets"
  ) +
  theme_bw()

p +
  annotate(
    "label",
    x = -Inf, y = Inf,
    label = label_text,
    hjust = -0.02,
    vjust = 1.02,
    size = 3.2,
    family = "mono",
    fill = "white",
    label.size = 0.25
  )

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/Height-dependent_LMF_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

# ###############################
# # 1) Round f-scores to define "same" (adjust digits if needed)
# df <- filtered_df_1 %>%
#   mutate(f_score_r   = round(f_score, 8),
#          paramset_id = factor(paramset_id))
# 
# # 2) Build a color for each unique rounded F-score
# fs_levels   <- sort(unique(df$f_score_r))
# cols_by_fs  <- hue_pal(l = 15, c = 100)(length(fs_levels))
# names(cols_by_fs) <- fs_levels
# 
# # 3) Map each paramset_id to the color of its F-score
# key <- df %>% distinct(paramset_id, f_score_r)
# param_cols <- cols_by_fs[as.character(key$f_score_r)]
# names(param_cols) <- key$paramset_id
# 
# # 4) Plot: color is paramset_id, but colors come from its F-score
# p <- ggplot(df, aes(x = tph, y = f_score_r, color = paramset_id)) +
#   geom_point(alpha = 0.9, size = 2.8) +
#   scale_color_manual(values = param_cols, name = "Param ID") +
#   labs(title = "F-score vs. tph across Parameter Sets",
#        x = "tph", y = "F-score") +
#   theme_minimal() +
#   theme(plot.title = element_text(face = "bold", size = 12))
# 
# p

################################################################################
# BEST PARAMSETS AND ALL F-SCORES
################################################################################

file_ref2 = field_ref |>
  dplyr::select(plot_id = field_plot_id, obs_tree_density = tph, forest_ty = forest_type_dy, min_ht = min_ht_ohvis, dbh = dbh_mean) |>
  mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))

merged_table2 = left_join(merged_table, file_ref2 , by = join_by("plot_id" == "plot_id"))

filtered_df2 <- merged_table2 %>%
  group_by(plot_id) %>%
  slice_max(order_by = f_score, n = 1, with_ties = FALSE) %>%
  ungroup()

best_param_ids <- filtered_df2 %>%
  pull(paramset_id) %>%
  unique()

best_from_all <- merged_table2 %>%
  filter(paramset_id %in% best_param_ids) %>%
  filter(!is.na(obs_tree_density), !is.na(f_score)) %>%
  mutate(
    paramset_id = factor(paramset_id),
    plot_id     = factor(plot_id)
  )

n_plots   <- nlevels(best_from_all$plot_id)
my_shapes <- 0:25

cols <- hue_pal(l = 65, c = 100)(nlevels(best_from_all$paramset_id))
names(cols) <- levels(best_from_all$paramset_id)

p <- ggplot(
  best_from_all,
  aes(
    x     = obs_tree_density,
    y     = f_score,
    shape = paramset_id
  )
) +
  geom_point(size = 1, stroke = 1, alpha = 0.8) +
  scale_shape_manual(values = my_shapes, name = "Parameter Set ID") +
  labs(
    title = "F-score vs Tree Density for Best Paramsets",
    x     = "Tree density (tph)",
    y     = "F-score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.grid   = element_blank(),
    axis.line    = element_line(colour = "black", linewidth = 0.6),
    axis.ticks   = element_line(colour = "black", linewidth = 0.5),
    axis.ticks.length = unit(3, "pt")
  )

p

################################################################################
# FACETED PARAMETER VS F-SCORE FOR BEST-BY-TYPE
################################################################################

library(ggrepel)

param_cols <- c("lmf_a","lmf_b","obs_tree_density","forest_ty","min_ht","Clumping_Index")

param_cols <- best_by_type |>
  dplyr::select(where(is.numeric)) |>
  dplyr::select(-f_score) |>
  names()

param_cols

plot_dat <- best_by_type %>%
  pivot_longer(cols = all_of(param_cols),
               names_to = "param",
               values_to = "value")

plot_dat <- plot_dat %>%
  filter(forest_ty != "Mmc-lowtph-highdbh")

p <- ggplot(plot_dat, aes(x = value, y = f_score)) +
  geom_point(size = 4, alpha = 0.9) +
  geom_text_repel(
    aes(label = plot_id),
    size = 3.5,
    max.overlaps = Inf,
    box.padding = 0.3,
    point.padding = 0.2
  ) +
  facet_wrap(~param, scales = "free_x") +
  labs(
    title = "Best F-score per forest/structure type (across all plots)",
    x = NULL, y = "Best F-score"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )

p

################################################################################
# TOP 8 F-SCORES PER SELECTED PARAMETER VARIABLE
################################################################################

vars_to_plot <- c("lmf_a","lmf_b","obs_tree_density","min_ht","Clumping_Index")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

topn <- 8

top8_each_var <- merged_table2 %>%
  pivot_longer(cols = all_of(vars_to_plot),
               names_to = "param",
               values_to = "value") %>%
  filter(!is.na(value), !is.na(f_score)) %>%
  group_by(param, plot_id) %>%
  slice_max(f_score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(param) %>%
  arrange(desc(f_score), .by_group = TRUE) %>%
  slice_head(n = topn) %>%
  mutate(rank_in_param = row_number()) %>%
  ungroup()

p <- ggplot(top8_each_var, aes(x = value, y = f_score)) +
  geom_point(aes(color = rank_in_param), size = 2.0, alpha = 0.95) +
  geom_text_repel(
    aes(label = plot_id),
    size = 2.2,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  facet_wrap(~param, scales = "free_x", ncol = 3) +
  scale_color_viridis_c(option = "turbo", direction = -1, name = "Rank (1=best)") +
  labs(
    title = "Top 8 F-scores per selected parameter variable",
    x = NULL, y = "F-score"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 10, face = "bold")
  )

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/Top_8_F-scores_per_selected_parameter_variable_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

################################################################################
# GLOBAL PERFORMANCE OF PARAMETER SETS ACROSS PLOTS
################################################################################

library(dplyr)
library(ggplot2)

param_summary <- merged_table2 %>%
  group_by(paramset_id) %>%
  summarise(
    mean_f = mean(f_score, na.rm = TRUE),
    sd_f   = sd(f_score, na.rm = TRUE),
    median_f = median(f_score, na.rm = TRUE),
    n_plots = n()
  ) %>%
  arrange(desc(mean_f))

id_levels <- sort(unique(param_summary$paramset_id))

scale_x_discrete(
  breaks = id_levels[seq(1, length(id_levels), by = 10)]
)

id_levels <- sort(unique(param_summary$paramset_id))

p = ggplot() +
  geom_jitter(
    data = merged_table2,
    aes(x = paramset_id, y = f_score),
    width = 0.2,
    alpha = 0.4,
    size = 1,
    color = "grey40"
  ) +
  geom_errorbar(
    data = param_summary,
    aes(
      x = paramset_id,
      ymin = mean_f - sd_f,
      ymax = mean_f + sd_f
    ),
    width = 0.2,
    linewidth = 0.5
  ) +
  geom_point(
    data = param_summary,
    aes(x = paramset_id, y = mean_f),
    size = 3,
    shape = 21,
    fill = "cyan"
  ) +
  scale_x_discrete(
    breaks = id_levels[seq(1, length(id_levels), by = 10)]
  ) +
  labs(
    title = "Global performance of parameter sets across plots",
    x = "Parameter set",
    y = "F-score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(face = "bold", angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/Global_performance_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

################################################################################
# WHERE GLOBAL BEST FAILS
################################################################################

best_param <- param_summary %>%
  slice(1) %>%
  pull(paramset_id)

failures <- merged_table2 %>%
  group_by(plot_id) %>%
  mutate(best_plot_f = max(f_score, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    paramset_id == best_param,
    best_plot_f - f_score > 0.15
  )

p = ggplot(
  merged_table2 %>% filter(paramset_id == best_param),
  aes(x = plot_id, y = f_score)
) +
  geom_point(size = 3, color = "darkblue") +
  geom_text(
    data = failures,
    aes(label = plot_id),
    vjust = -0.8,
    size = 3,
    color = "red"
  ) +
  labs(
    title = paste("Performance of global best parameter set:", best_param),
    x = "Plot ID",
    y = "F-score"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p

ggsave(
  filename = paste0("paramID_plots_all_fined_new_set/best_param_overall_and plot_performace_04_15", ".png"),
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)