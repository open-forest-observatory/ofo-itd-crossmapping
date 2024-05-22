# Purpose: Find the drone missions that completely cover (with some buffer) a provided set of field
# plots. Create a table of the associations.

devtools::load_all()
library(tidyverse)
library(sf)

# datadir_field = readLines("sandbox/data-dirs/derek-fieldref-laptop.txt")
# datadir_imagery = readLines("sandbox/data-dirs/derek-map-imagery-laptop.txt")

MISSION_POLYGONS_DIR = "/ofo-share/drone-imagery-processed/01/mission-polygons/"
PLOT_BOUNDARIES_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/unaligned/plot-bounds/"
ASSOC_TABLE_OUTPUT_DIR = "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/"

mission_polys = st_read(file.path(MISSION_POLYGONS_DIR, "mission_polygons.gpkg")) |> st_transform(3310)
plot_bounds = read_and_merge_plot_boundaries(PLOT_BOUNDARIES_DIR) |> st_transform(3310)

vis = mission_polys[mission_polys$mission %in% c("000124_20240503T1032", "000126_20240503T1157"),]

# Filter to the preselected field plots
focal_plots = c(5, 88, 7, 6, 16, 18, 14, 15, 26, 94, 95, 90, 46, 35, 102, 32, 98, 112, 44, 91, 100, 104, 105, 110, 19, 68, 101)
focal_plots = focal_plots |> str_pad(4, pad = "0")
length(focal_plots)
plot_bounds = plot_bounds |>
  filter(plot_id %in% focal_plots)

# Filter to the drone missions that meet our criteria (nadir, > 100 m)
mission_polys = mission_polys |>
  filter(agl_median > 100 & agl_cv < 0.1 & processed_pitch < 9)

# Find the drone mision polygons that completely cover a field plot, with at least 50 m buffer
mission_polys_negbuff = mission_polys |> st_buffer(-50)

overlaps = st_within(plot_bounds, mission_polys_negbuff, sparse = FALSE)

# Turn the nested list of overlaps (TRUE/FALSE) into a nested list of mission IDs
overlaps = apply(overlaps, 1, which)
overlaps = lapply(overlaps, function(x) mission_polys$mission[x])

# Get the IDs of the missions that cover each plot
plot_bounds$drone_mission_containing = overlaps

# One field plot is overlapped by two qualifying drone polygons. Take the one that is closer to
# standard altitude, manually
plot_bounds[plot_bounds$plot_id == "0104", "drone_mission_containing"] = "000126_20240503T1157"

# Make sure no other field plots have more than one overlapping drone footprint
lapply(plot_bounds$drone_mission_containing, length) |> unlist()
if (any(lapply(plot_bounds$drone_mission_containing, length) > 1)) {
  stop("Some field plots are overlapped by more than one drone mission. Need to select which one to use manually above.")
}

# Convert overlap list column to a character column
plot_bounds$drone_mission_containing = plot_bounds$drone_mission_containing |> map_chr(~ str_c(.x, collapse = ","))

# Write the correspondences
plot_drone = plot_bounds |>
  st_drop_geometry() |>
  mutate(drone_mission_id = str_sub(drone_mission_containing, 1, 6)) |>
  select(field_plot_id = plot_id,
         drone_mission_id,
         drone_mission_processed_id = drone_mission_containing)

write_csv(plot_drone, file.path(ASSOC_TABLE_OUTPUT_DIR, "field-plot_drone-mission_crosswalk.csv"))
