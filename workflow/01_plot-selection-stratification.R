# Purpose: Aggregate and summarize geospatial data into a format that is convenient for defining plot selection.
# Input data includes past treatments, planned future treatments, topography (elev, SRI, etc), project area bounds

## SETUP

# Load packages

library(terra)
library(sf)
library(tidyverse)

# Constants:

# Specify the root of the data directory
DATADIR = readLines("datadirs/datadir_derek-laptop.txt")

# Number of plots per TPH-DBH category to select randomly
PLOTS_PER_CAT = 2

# Set random seed
set.seed(124)

## LOAD DATA

# Field plot data
plots = read.csv(file.path(DATADIR, "site-selection", "raw", "field-plots_w-ht_w-imagery_w-forest-type.csv"))


## PROCESS DATA

# Exclude plots with skipped trees
plots = plots |>
  filter(is.na(num_ohvis_trees_excluded) | num_ohvis_trees_excluded == 0)

# Exclude FOCAL dispersal kernel project (large drone footprints, post-fire plots, not unique stand characteristics)
plots = plots |>
  filter(project_name != "FOCAL_dispersal_kernel_project")

# Filter to focal forest types
types_foc = c("mixed evergreen",
              "dry mixed conifer",
              "moist mixed conifer",
              "white fir",
              "subalpine",
              "jeffrey pine")

plots = plots |>
  filter(forest_type_dy %in% types_foc)


# Define stem density (TPH) categories

break1 = quantile(plots$tph, 0.4)
break2 = quantile(plots$tph, 0.6)

tph_cats = data.frame(
  category = c("low", "high"),
  min = c(0, break2),
  max = c(break1, Inf)
)


## Select plots
# For each forest type, filter to plots in each TPH category. Then, for each TPH category, define
# DBH categories and select a random subset of plots in each.

plots_selected = data.frame()

for (type in unique(plots$forest_type_dy)) {

  plots_focaltype = plots |>
    filter(forest_type_dy == type)

  for (i in 1:nrow(tph_cats)) {

    plots_focaltype_tph = plots_focaltype |>
      filter(tph >= tph_cats$min[i] & tph < tph_cats$max[i])


    # Define DBH categories

    break1 = quantile(plots_focaltype_tph$dbh_mean, 0.5)
    break2 = quantile(plots_focaltype_tph$dbh_mean, 0.5)

    dbh_cats = data.frame(
      category = c("low", "high"),
      min = c(0, break2),
      max = c(break1, Inf)
    )

    # within each DBH category, select a random subset of plots
    for (j in 1:nrow(dbh_cats)) {

      plots_focaltype_tph_dbh = plots_focaltype_tph |>
        filter(dbh_mean >= dbh_cats$min[j] & dbh_mean < dbh_cats$max[j])

      n = min(nrow(plots_focaltype_tph_dbh), PLOTS_PER_CAT)

      plots_focaltype_tph_dbh_sample = plots_focaltype_tph_dbh |>
        slice_sample(n = n, replace = FALSE)

      # save it to the running list of selected plots
      plots_focaltype_tph_dbh_sample = plots_focaltype_tph_dbh_sample |>
        mutate(tph_cat = tph_cats$category[i],
               dbh_cat = dbh_cats$category[j])

      plots_selected = bind_rows(plots_selected, plots_focaltype_tph_dbh_sample)

    }

  }
}

## VISUALIZE SELECTED PLOTS

ggplot(plots_selected, aes(x = dbh_mean, y = tph, color = forest_type_dy, pch = project_name)) +
  geom_point(size = 3) +
  theme_minimal()

## LIST SELECTED PLOT IDS

plots_selected$field_plot_id |> sort()
length(plots_selected$field_plot_id)
