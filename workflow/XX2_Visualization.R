# Purpose: Visualize tree detection accuracy across plots and parameter sets
#          to understand how parameter values affect performance.

## --------------------------- 0. Packages -------------------------------------

library(tidyverse)
library(plotly)
library(htmlwidgets)
library(mgcv)

## --------------------------- 1. Paths & constants ----------------------------

# File paths
MATCH_STATS_DIR   <- "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-tree-evals/"
ITD_PARAMS_DEF_DIR <- "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"
EVAL_FIGURES_DIR  <- "/ofo-share/ofo-itd-crossmapping_data/itd-paramset-eval-figures/"

# Plots to exclude (bad alignment)
PLOTS_EXCLUDE <- c("0015", "0046", "0100", "0110")

# Parameter group to evaluate
FOC_PARAMGROUP <- "7003"

## --------------------------- 2. Load data ------------------------------------

# Match stats file (per-plot x paramset accuracy)
match_file <- paste0(FOC_PARAMGROUP, "_smooth_10cm_pred_results.csv")

d <- readr::read_csv(
  file.path(MATCH_STATS_DIR, match_file)
)

# Exclude problematic plots
d <- d |>
  dplyr::filter(!plot_ID %in% PLOTS_EXCLUDE)

# Harmonize plot_id name so we can reuse downstream code
if ("plot_ID" %in% names(d)) {
  d <- d |>
    dplyr::rename(plot_id = plot_ID)
}

# Parameter definitions
param_file <- paste0("itd-paramsets_", FOC_PARAMGROUP, ".csv")

param_defs <- readr::read_csv(
  file.path(ITD_PARAMS_DEF_DIR, param_file)
)

# Attach parameter definitions to match stats
d <- d |>
  dplyr::left_join(param_defs, by = dplyr::join_by(paramset_id == paramset_id))

## --------------------------- 3. Long format for parameters -------------------

# Long format: each row = (plot, paramset, param_name, param_value)
d_long <- d |>
  tidyr::pivot_longer(
    cols      = dplyr::starts_with("lmf_"),
    names_to  = "param",
    values_to = "value"
  ) |>
  # Reattach param_defs so all param columns are available for tooltips
  dplyr::left_join(param_defs, by = dplyr::join_by(paramset_id == paramset_id))

## --------------------------- 4. VIS OPTION 1: static ggplot ------------------

# Use a subset of plots for clarity (can expand to all later)
d_plot <- d_long |>
  dplyr::filter(plot_id %in% c("0005", "0006", "0007", "0014"))

p <- ggplot(
  d_plot,
  aes(
    x      = value,
    y      = f_score,
    color  = lmf_diam_max,
    # extra aesthetics so they appear in the Plotly tooltip
    label1 = lmf_a,
    label2 = lmf_b,
    label3 = lmf_c,
    label4 = lmf_diam_min,
    label5 = lmf_diam_max
  )
) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(end = 0.85) +
  facet_grid(plot_id ~ param, scales = "free")

p

## --------------------------- 5. VIS OPTION 2: interactive Plotly ------------- 

p_interactive <- plotly::ggplotly(
  p,
  tooltip = c("label1", "label2", "label3", "label4", "label5")
)

html_file <- paste0("paramset-eval_paramgroup-", FOC_PARAMGROUP, ".html")

htmlwidgets::saveWidget(
  p_interactive,
  file.path(EVAL_FIGURES_DIR, html_file),
  selfcontained = TRUE,
  libdir       = "lib"
)

## --------------------------- 6. VIS OPTION 3: GAM partial effects ------------

# Example: fit GAM for a single plot (try different plot_ids as needed)
d_mod <- d |>
  dplyr::filter(plot_id == "0007")

m <- mgcv::gam(
  f_score ~ s(lmf_a) + s(lmf_b) + s(lmf_c) + s(lmf_diam_min) + s(lmf_diam_max),
  data   = d_mod
)

plot(m, scheme = 1)

## --------------------------- 7. VIS OPTION 4: top paramsets for a plot -------

d_best <- d |>
  dplyr::filter(plot_id == "0007") |>
  dplyr::arrange(dplyr::desc(f_score)) |>
  dplyr::slice(1:10)

d_best
