# Purpose: Generate a set of ITD parameter sets (a "parameter set group") to evaluate, and save to csv

#### Setup

# Load necessary packages
library(tidyverse)

## Data paths

# Directory in which to store the parameter definitions (as a CSV)
ITD_PARAMS_DEF_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"

## Processing constants for user to define

# Setting an ID for the set of paramter sets allows us to generate multiple sets of parameters and track them separately, so we can revise
PARAM_GROUP_ID = "04"

# How many parameter sets to sample randomly from the grid?
N_PARAMSETS = 500

# Define parameter ranges
PARAM_RANGES <- list(
  lmf_a = seq(-10, 10, length.out = 10),
  lmf_b = seq(-2, 2, length.out = 10),
  lmf_c = seq(-0.5, 0.5, length.out = 10),
  lmf_rad_min = seq(0.1, 5, length.out = 10),
  lmf_rad_max = seq(2, 20, length.out = 10)
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
