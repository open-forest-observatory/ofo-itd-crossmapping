# Purpose: Prepare config files to run automate-metashape for the drone imagery sets covering the
# selected field reference plots

## SETUP

# Specify the root of this project's data directory
datadir = readLines("datadirs/js2-itd-crossmapping.txt")

# Load functions from the 'ofo' R package. The copy in "/ofo-share/utils" is intended to contain the
# latest version of the 'main' branch. If you want to make edits and test their effect here as you
# edit ofo-r, you could instead clone the 'ofo-r' repo to your own 'repos' folder and change the
# path below to match where you cloned it to.
devtools::load_all("/ofo-share/utils/ofo-r")

# # Alternatively, load it from github:
# devtools::install_github("open-forest-observatory/ofo-r")
# load(ofo)

# Load libraries
library(tidyverse)


## PROCESSING

# Get the names of the selected datasets, assuming all relevant image sets have been placed in
# {datadir}/drone-imagery-raw, each in a separate folder named with the image set ID
dataset_names = list.dirs(file.path(datadir, "drone-imagery-raw"), recursive = FALSE, full.names = FALSE)

# Get the absolute path to the imagery folder for each dataset
all_dataset_paths = file.path(datadir, "drone-imagery-raw", dataset_names)

# Define the 'scenarios' for which we want to run automate-metashape. In this case, a 'scenario' is
# an imagery set.
scenarios = data.frame("photo_path" = all_dataset_paths,
                       "config_filename" = dataset_names)

# Define the path to the base config file for automate-metashape
base_yaml_filepath = file.path(datadir, "photogrammetry", "configs", "base.yml")

# Generate the "derived" configs (one for each "scenario" or imagery dataset) and save them into the
# same folder as the base config file. This also generates a '.sh' file to run all of the resulting
# configs in sequence, using the path to automate-metashape provided here.
make_derived_configs(base_yaml_filepath, scenarios,
                     automate_metashape_path = "/ofo-share/utils/automate-metashape")
