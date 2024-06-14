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
devtools::load_all("/ofo-share/utils/ofo-r")

# Data paths
OBSERVED_UNALIGNED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/unaligned/trees/"
OBSERVED_ALIGNED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/trees/"

PRELIM_DETECTED_TREES_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/detected-trees_prelim/"

# Determine which plots need trees detected, based on the CHMs that have been generated
plot_ids = list.files(file.path(PRELIM_DETECTED_TREES_DIR), pattern = ".gpkg") |>
  str_remove(".gpkg")

plot_id = plot_ids[1]


for (plot_id in plot_ids) {

  # Load the two tree maps
  obs = st_read(file.path(OBSERVED_UNALIGNED_TREES_DIR, paste0(plot_id, ".gpkg")))
  pred = st_read(file.path(PRELIM_DETECTED_TREES_DIR, paste0(plot_id, ".gpkg")))

  # Prep the two tree maps (they need a x, y, and z)
  if(sum(is.na(obs$height))/ nrow(obs) > 0.5) {
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

  # Apply this shift to the observed trees
  # Get observed tree coords
  coords = st_coordinates(obs_sf)
  obs_nogeom = st_drop_geometry(obs_sf)
  obs_nogeom$x = coords[, 1] + shift$shift_x
  obs_nogeom$y = coords[, 2] + shift$shift_y

  obs_shifted = st_as_sf(obs_nogeom, crs = 3310, coords = c("x", "y"))

  # Write
  st_write(obs_shifted, file.path(OBSERVED_ALIGNED_TREES_DIR, paste0(plot_id, ".gpkg")), delete_dsn = TRUE)

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
