# Purpose: For all the needed field plots, copy in the tree locs and plot boundary as .gpkg files

library(tidyverse)
library(sf)
library(googlesheets4)

# Load functions from the 'ofo' R package. The copy in "/ofo-share/utils" is intended to contain the
# latest version of the 'main' branch. If you want to make edits and test their effect here as you
# edit ofo-r, you could instead clone the 'ofo-r' repo to your own 'repos' folder and change the
# path below to match where you cloned it to.
devtools::load_all("/ofo-share/utils/ofo-r")

# Data paths
ASSOC_TABLE_DIR = "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/"
TREE_DATA_DIR_CROSSMAPPING = "/ofo-share/ofo-itd-crossmapping_data/field-reference/unaligned/trees"

GOOGLE_SHEET_ID = "1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4"

# Read in the association table so we know what plots to prepare
assoc = read_csv(file.path(ASSOC_TABLE_DIR, "field-plot_drone-mission_crosswalk.csv"))

# Read in the tree data from the OFO google sheet
# gs4_deauth()
# gs4_auth()
tabular_data = read_and_standardize_tabular_field_ref_data(GOOGLE_SHEET_ID)
trees = prep_trees(tabular_data$trees, tabular_data$species_codes)

# Filter to the trees that are in the field plots we need
trees = trees |>
  filter(plot_id %in% assoc$field_plot_id)

# Make spatial
trees = st_as_sf(trees, coords = c("tree_lon", "tree_lat"), crs = 4326)

# For each needed field plot, save out a .gpkg of the trees
for (field_plot_id in assoc$field_plot_id) {
  trees_foc = trees |>
    filter(plot_id == !!field_plot_id)

  trees_foc |> st_write(file.path(TREE_DATA_DIR_CROSSMAPPING, str_c(field_plot_id, ".gpkg")), delete_dsn = TRUE)
}
