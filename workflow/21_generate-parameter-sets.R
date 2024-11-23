# Purpose: Generate a set of ITD parameter sets (a "parameter set group") to evaluate, and save to csv

#### Setup

# Load necessary packages
library(tidyverse)
library(yaml)

## Data paths

# Directory in which to store the parameter definitions (as a CSV)
ITD_PARAMS_DEF_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"

## Processing constants for user to define

# Setting an ID for the set of paramter sets allows us to generate multiple sets of parameters and
# track them separately, so we can revise the ranges iteratively
PARAM_GROUP_ID = "01"

# How many parameter sets to sample randomly from the grid?
N_PARAMSETS = 1000

# Define parameter ranges
PARAM_RANGES = data.frame(
  lmf_a = c(min = -2, max = 5),
  lmf_b = c(min = -0.2, max = 0.5),
  lmf_c = c(min = -0.02, max = 0.05),
  lmf_diam_min = c(min = 0.1, max = 5),
  lmf_diam_max = c(min = 2, max = 20)
) |> t()


#### Workflow

# Write the parameter ranges to a file for reference
param_ranges_filename = paste0("itd-param-ranges_", PARAM_GROUP_ID, ".csv")
write.csv(PARAM_RANGES, file.path(ITD_PARAMS_DEF_DIR, param_ranges_filename), row.names = TRUE)

# Create a data frame with the randomly sampled parameter sets, with one columne for each parameter, and
# one row for each parameter set
sampled_params = list()
for(i in 1:nrow(PARAM_RANGES)) {
  param_name = rownames(PARAM_RANGES)[i]
  param_range = PARAM_RANGES[i, ]
  sampled_params[[param_name]] = runif(N_PARAMSETS, min = param_range["min"], max = param_range["max"])
}
sampled_params = bind_rows(sampled_params)

# Add an ID for each parameter set (left-padded with zeros to a width of 6)
ids = sprintf("%06d", 1:N_PARAMSETS)
sampled_params$paramset_id = ids

# Save the parameter sets as CSV
filename = paste0("itd-paramsets_", PARAM_GROUP_ID, ".csv")
write_csv(sampled_params, file.path(ITD_PARAMS_DEF_DIR, filename))
