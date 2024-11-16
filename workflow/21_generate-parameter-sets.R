# Purpose: Generate a set of ITD parameter sets (a "parameter set group") to evaluate, and save to csv

#### Setup

# Load necessary packages
library(tidyverse)

## Data paths

# Directory in which to store the parameter definitions (as a CSV)
ITD_PARAMS_DEF_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"

## Processing constants for user to define

# Setting an ID for the set of paramter sets allows us to generate multiple sets of parameters and track them separately, so we can revise
PARAM_GROUP_ID = "01"

# How many parameter sets to sample randomly from the grid?
N_PARAMSETS = 1000

# Define parameter ranges
PARAM_RANGES <- list(
  hmin = seq(2, 15, by = 1),
  hmax = seq(20, 60, by = 10),
  itd_a = seq(0.1, 1.0, by = 0.1),
  itd_b = seq(0, 1.0, by = 0.1),
  itd_c = seq(0, 1.0, by = 0.1),
  rad_min = c(0.5, 1, 2),
  rad_max = c(5, 10, 20)
)



#### Workflow

# Generate full parameter grid
param_grid <- expand.grid(PARAM_RANGES)

# Sample a subset of the parameter sets
set.seed(123)
sampled_params <- param_grid |> sample_n(N_PARAMSETS)

# Add an ID for each parameter set (left-padded with zeros to a width of 6)
ids = sprintf("%06d", 1:N_PARAMSETS)
sampled_params$paramset_id = ids

# Save the parameter sets as CSV
filename = paste0("itd-paramsets_", PARAM_GROUP_ID, ".csv")
write_csv(sampled_params, file.path(ITD_PARAMS_DEF_DIR, filename))
