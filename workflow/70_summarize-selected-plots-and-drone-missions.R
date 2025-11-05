# Purpose: for the selected ground plots and drone missions, summarize relevant attributes for
# reporting in paper

library(tidyverse)
library(sf)

DATADIR = ("/ofo-share/ofo-itd-crossmapping_data/")
MISSION_POLYGONS_DIR = "/ofo-share/ofo-itd-crossmapping_data/from-external_drone-imagery-processed/mission-polygons"
PLOT_BOUNDARIES_DIR = "/ofo-share/ofo-itd-crossmapping_data/field-reference/unaligned/plot-bounds/"
ASSOC_TABLE_OUTPUT_DIR = "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/"

DRONE_MISSION_METADATA_FILEPATH = file.path(MISSION_POLYGONS_DIR, "mission_polygons.gpkg")
OFO_CATALOG_MISSION_METADATA_FILEPATH = "/ofo-share/ofo-itd-crossmapping_data/from-external_ofo-public-catalog/ofo_drone-missions_metadata.gpkg"

SUMMARIZED_METADATA_OUTPUT_FILEPATH = "/ofo-share/ofo-itd-crossmapping_data/site-selection/summaries-for-reporting"


crosswalk = read_csv(file.path(ASSOC_TABLE_OUTPUT_DIR, "field-plot_drone-mission_crosswalk.csv"))
ground_plots = read_csv(file.path(DATADIR, "site-selection", "raw", "field-plots_w-ht_w-imagery_w-forest-type.csv"))
drone_mission_metadata = sf::st_read(DRONE_MISSION_METADATA_FILEPATH)



selected_ground_plot_ids = c("0005", "0006", "0007", "0014", "0016", "0018", "0019", "0026", "0032", "0035", "0044", "0088", "0090", "0091", "0095", "0098", "0101", "0102", "0104", "0105")

selected_ground_plots = ground_plots |>
  filter(field_plot_id %in% as.numeric(selected_ground_plot_ids))

# Using the crosswalk, get the drone mission IDs corresponding to the selected ground plots
selected_crosswalk = crosswalk |>
  filter(field_plot_id %in% selected_ground_plot_ids)

# Is there a ground plot that's not in the crosswalk?
missing_plots = setdiff(selected_ground_plot_ids, selected_crosswalk$field_plot_id)
missing_plots

selected_drone_missions = drone_mission_metadata |>
  filter(mission %in% selected_crosswalk$drone_mission_processed_id)

# Compute the mission ID from the processed ID
selected_drone_missions = selected_drone_missions |>
  mutate(mission_id = str_sub(mission, 1, 6))

# Load the actual OFO drone catalog metadata for these missions (additional attributes needed, like aircraft)
ofo_drone_catalog_metadata = sf::st_read(OFO_CATALOG_MISSION_METADATA_FILEPATH)

selected_ofo_catalog_metadata = ofo_drone_catalog_metadata |>
  filter(mission_id %in% selected_drone_missions$mission_id)

# Select what we need from both data sources and join them
selected_ofo_catalog_metadata_selected = selected_ofo_catalog_metadata |>
  st_drop_geometry() |>
  select(mission_id,
         project_name,
         flight_pattern,
         overlap_front_nominal,
         overlap_side_nominal,
         camera_pitch_nominal,
         terrain_follow,
         altitude_agl_nominal,
         weather,
         contributor_dataset_name,
         aircraft_model_name,
         date = earliest_date_derived,
         year = earliest_year_derived,
         rtk_nominal)

selected_drone_missions_selected = selected_drone_missions |>
  select(mission_id,
    mission_processed = mission,
    photogrammetry_derived_altitude = agl_median
  )

drone_footprints_with_metadata = selected_drone_missions_selected |>
  left_join(selected_ofo_catalog_metadata_selected, by = "mission_id")



# Get the ground plot bounds
plot_bounds = read_and_merge_plot_boundaries(PLOT_BOUNDARIES_DIR)
selected_plot_bounds = plot_bounds |>
  filter(plot_id %in% selected_ground_plot_ids)

# Merge in the relevant ground plot attributes
ground_plots_with_metadata = selected_plot_bounds |>
  mutate(plot_id = as.numeric(plot_id)) |>
  left_join(selected_ground_plots, by = c("plot_id" = "field_plot_id"))


# Save out the geospatial drone mission bounds and ground plot bounds with metadata for
# reporting/figures
st_write(drone_footprints_with_metadata,
         file.path(SUMMARIZED_METADATA_OUTPUT_FILEPATH, "selected-drone-missions_with-metadata.gpkg"),
         delete_dsn = TRUE)

st_write(ground_plots_with_metadata,
         file.path(SUMMARIZED_METADATA_OUTPUT_FILEPATH, "selected-ground-plots_with-metadata.gpkg"),
         delete_dsn = TRUE)


# Across the ground plots, make a figure of TPH vs mean DBH, colored by forest type
p = ground_plots_with_metadata |>
  mutate(forest_type_dy = recode(forest_type_dy,
    "mixed evergreen" = "Mixed evergreen",
    "dry mixed conifer" = "Dry mixed conifer",
    "moist mixed conifer" = "Moist mixed conifer",
    "white fir" = "White fir",
    "subalpine" = "subalpine",
    "red fir" = "Red fir",
    "jeffrey pine" = "Jeffrey pine"
  )) |>
  st_drop_geometry() |>
  ggplot(aes(x = tph, y = dbh_mean, color = forest_type_dy)) +
  scale_color_viridis_d() +
  geom_point(size = 3) +
  theme_bw() +
  labs(x = "Tree density (trees per hectare)",
       y = "Mean tree DBH (cm)",
       color = "Forest type")

png(file.path(SUMMARIZED_METADATA_OUTPUT_FILEPATH, "selected-ground-plots_tph-vs-mean-dbh.png"),
  width = 6, height = 4, units = "in", res = 300
)
print(p)
dev.off()


# Create tables for ms

ground_plots_table = ground_plots_with_metadata |>
  st_drop_geometry() |>
  select(
    plot_id,
    tph,
    dbh_mean,
    forest_type_dy,
    top_species,
    survey_year
  ) |>
  arrange(plot_id) |>
  mutate(
    plot_id = sprintf("%04d", plot_id),
    top_species = str_replace_all(top_species, "-", "% "),
    top_species = str_replace_all(top_species, ",", ", ")
  )
ground_plots_table
  # All had area of 0.29 ha, all measured all overhead-visible trees > 5 m tall

write_csv(ground_plots_table,
  file.path(SUMMARIZED_METADATA_OUTPUT_FILEPATH, "selected-ground-plots_summary-table.csv"),
  na = ""
)


drone_missions_table = drone_footprints_with_metadata |>
  st_drop_geometry() |>
  mutate(overlap_front_side_nominal = paste0(overlap_front_nominal, "/", overlap_side_nominal)) |>
  select(
    mission_id,
    overlap_front_side_nominal,
    date
  ) |>
  arrange(mission_id)

# All had camera pitch of 0, terrain follow, clear weather, RTK, and covered a buffer of at least 60
# m beyond the plot, and a nominal altitude of 110-120 m, collected 2023 (June 22 to Aug 04)

write_csv(drone_missions_table,
  file.path(SUMMARIZED_METADATA_OUTPUT_FILEPATH, "selected-drone-missions_summary-table.csv"),
  na = ""
)


# Use the crosswalk to merge the ground plot and drone mission tables for a combined table
combined_table = crosswalk |>
  filter(field_plot_id %in% selected_ground_plot_ids) |>
  left_join(ground_plots_table,
    by = c("field_plot_id" = "plot_id")
  ) |>
  left_join(drone_missions_table,
    by = c("drone_mission_id" = "mission_id")
  ) |>
  select(field_plot_id, tph, dbh_mean, forest_type = forest_type_dy, top_species, field_survey_year = survey_year, drone_mission_id, drone_imagery_date = date, drone_overlap = overlap_front_side_nominal)

write_csv(combined_table,
  file.path(SUMMARIZED_METADATA_OUTPUT_FILEPATH, "selected-plots-and-drone-missions_combined-summary-table.csv"),
  na = ""
)
