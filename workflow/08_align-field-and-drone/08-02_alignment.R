# Purpose: Perform preliminary tree detection on CHM to create a preliminary drone-detected tree
# dataset for determining how to align the field data to the drone data

library(sf)
library(tidyverse)
library(terra)
library(furrr)

# Load functions from the 'ofo' R package. The copy in "/ofo-share/utils" is intended to contain the
# latest version of the 'main' branch. If you want to make edits and test their effect here as you
# edit ofo-r, you could instead clone the 'ofo-r' repo to your own 'repos' folder and change the
# path below to match where you cloned it to.
# devtools::document("/ofo-share/repos-derek/ofo-r"); devtools::install("/ofo-share/repos-derek/ofo-r"); library(ofo)
# devtools::load_all("/ofo-share/repos-derek/ofo-r")
# devtools::load_all("/ofo-share/repos-derek/ofo-r")
devtools::load_all("/ofo-share/repos-derek/ofo-r")

# Data paths
OBSERVED_UNALIGNED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/unaligned/trees/"
OBSERVED_ALIGNED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/trees/"

OBSERVED_UNALIGNED_PLOTBOUNDS_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/unaligned/plot-bounds/"
OBSERVED_ALIGNED_PLOTBOUNDS_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/plot-bounds/"

PRELIM_DETECTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees_prelim/"

# Determine which plots need trees detected, based on the CHMs that have been generated
plot_ids = list.files(file.path(PRELIM_DETECTED_TREES_DIR), pattern = ".gpkg") |>
  str_remove(".gpkg")

plot_id = plot_ids[1]


align_plot = function(plot_id) {

  # Load the two tree maps
  obs = st_read(file.path(OBSERVED_UNALIGNED_TREES_DIR, paste0(plot_id, ".gpkg")))
  pred = st_read(file.path(PRELIM_DETECTED_TREES_DIR, paste0(plot_id, ".gpkg")))

  # Load the observed plot bounds
  obs_bounds = st_read(file.path(OBSERVED_UNALIGNED_PLOTBOUNDS_DIR, paste0(plot_id, ".gpkg")))
  obs_bounds = obs_bounds |> st_transform(crs = 3310)

  # Prep the two tree maps for alignment (they need a x, y, and z)
  if (sum(is.na(obs$height)) / nrow(obs) > 0.5) {
    stop("More than 50% of the observed tree heights are missing for plot_id ", plot_id)
  }
  obs = st_transform(obs, crs = 3310) # TODO: make this more general with a latlon-to-utm function
  obs_coords = st_coordinates(obs)
  obs = obs |>
    mutate(x = obs_coords[, 1],
          y = obs_coords[, 2],
          z = height)
  pred = st_transform(pred, st_crs(obs))
  pred_coords = st_coordinates(pred)
  pred = pred |>
    mutate(x = pred_coords[, 1],
          y = pred_coords[, 2],
          z = Z)

  obs_sf = obs # Store the original sf object for shifting & export later
  obs = st_drop_geometry(obs)
  pred = st_drop_geometry(pred)

  obs = obs |>
    select(x, y, z)
  pred = pred |>
    select(x, y, z)

  # Focus on the larger trees
  obs = obs |>
    filter(z > 10)
  pred = pred |>
    filter(z > 10)

  # Visualize the two
  vis2(pred, obs, zoom_to_obs = TRUE)

  shift = find_best_shift(pred,
                  obs,
                  obs_bounds = NULL,
                  objective_fn = obj_mean_dist_to_closest,
                  parallel = FALSE)

  # Apply this shift to the observed trees and write
  geom = st_geometry(obs_sf)
  geom_shifted = geom + c(shift$shift_x, shift$shift_y)
  st_crs(geom_shifted) = st_crs(geom)
  obs_shifted = st_set_geometry(obs_sf, geom_shifted)
  st_write(obs_shifted, file.path(OBSERVED_ALIGNED_TREES_DIR, paste0(plot_id, ".gpkg")), delete_dsn = TRUE)

  # Apply this shift to the plot bounds and write
  obs_bounds_geom = st_geometry(obs_bounds)
  obs_bounds_geom_shifted = obs_bounds_geom + c(shift$shift_x, shift$shift_y)
  st_crs(obs_bounds_geom_shifted) = st_crs(obs_bounds_geom)
  obs_bounds_shifted = st_set_geometry(obs_bounds, obs_bounds_geom_shifted)
  st_write(obs_bounds_shifted, file.path(OBSERVED_ALIGNED_PLOTBOUNDS_DIR, paste0(plot_id, ".gpkg")), delete_dsn = TRUE)

  # Visualize
  obs_shifted_coords = st_coordinates(obs_shifted)
  obs_shifted = obs_shifted |>
    mutate(x = obs_shifted_coords[, 1],
           y = obs_shifted_coords[, 2],
           z = height)
  obs_shifted = st_drop_geometry(obs_shifted)
  obs_shifted = obs_shifted |>
    select(x, y, z)

  vis2(pred, obs_shifted, zoom_to_obs = TRUE)

}

purrr::walk(plot_ids, align_plot)


# Poorly aligned are indexes 5, 13, 22, 23
plot_ids[c(5, 13, 22, 23)]
# 0015, 0046, 0105, 0110

# New poorly aligned are 5, 13, 19, 24
plot_ids[c(5, 13, 19, 24)]
# 0015, 0046, 0100, 0110