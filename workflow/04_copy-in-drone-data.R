# Purpose: For all the needed drone missions, copy in the processed imagery

library(tidyverse)
library(sf)

# Load functions from the 'ofo' R package. The copy in "/ofo-share/utils" is intended to contain the
# latest version of the 'main' branch. If you want to make edits and test their effect here as you
# edit ofo-r, you could instead clone the 'ofo-r' repo to your own 'repos' folder and change the
# path below to match where you cloned it to.
devtools::load_all("/ofo-share/utils/ofo-r")

# Data paths
ASSOC_TABLE_DIR = "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/"
PROCESSED_IMAGERY_DIR_OFO = "/ofo-share/drone-imagery-processed/01/metashape-outputs/"
PROCESSED_IMAGERY_DIR_CROSSMAPPING = "/ofo-share/ofo-itd-crossmapping_data/drone/photogrammetry-outputs/"

# Read in the association table
assoc = read_csv(file.path(ASSOC_TABLE_DIR, "field-plot_drone-mission_crosswalk.csv"))


# Create columns for the filepaths of the source and destinations of the drone imagery
assoc = assoc |>
  filter(!is.na(drone_mission_processed_id)) |>
  mutate(dsm_ptcloud_src = file.path(PROCESSED_IMAGERY_DIR_OFO, str_c(drone_mission_processed_id, "_dsm-ptcloud.tif")),
         dsm_mesh_src = file.path(PROCESSED_IMAGERY_DIR_OFO, str_c(drone_mission_processed_id, "_dsm-mesh.tif")),
         dtm_ptcloud_src = file.path(PROCESSED_IMAGERY_DIR_OFO, str_c(drone_mission_processed_id, "_dtm-ptcloud.tif")),
         dsm_ptcloud_dest = file.path(PROCESSED_IMAGERY_DIR_CROSSMAPPING, "dsm-ptcloud", str_c(field_plot_id,".tif")),
         dsm_mesh_dest = file.path(PROCESSED_IMAGERY_DIR_CROSSMAPPING, "dsm-mesh", str_c(field_plot_id,".tif")),
         dtm_ptcloud_dest = file.path(PROCESSED_IMAGERY_DIR_CROSSMAPPING, "dtm-ptcloud", str_c(field_plot_id,".tif")))


# Copy (hardlink) them into the crossmapping project directory
file.link(assoc$dsm_ptcloud_src, assoc$dsm_ptcloud_dest)
file.link(assoc$dsm_mesh_src, assoc$dsm_mesh_dest)
file.link(assoc$dtm_ptcloud_src, assoc$dtm_ptcloud_dest)
