
#### Setup

## Load packages
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(mgcv)
library(stringr)
library(mgcv)

## Set constants

# File paths
MATCH_STATS_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-tree-evals/"
ITD_PARAMS_DEF_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"
EVAL_FIGURES_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramset-eval-figures/"

FIELD_REF = "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/selected-field-and-drone-plots_v1.csv"

# Processing constants for user to define

# Plots to exclude (those with improperly aligned field reference data)
PLOTS_EXCLUDE = c("0015", "0046", "0105", "0110", "0101") # Temporarily excluding plot 0101 because it was manually added to the list of candidate plots and doesn't have a tree density value, can add back as soon as we address that 

# Which group of parameter sets to evaluate. Set to the same value as in the previous script (30).
FOC_PARAMGROUP = "41"


#### Workflow

## Load match stats
filename = paste0("match-stats_paramgroup-", paste(FOC_PARAMGROUP, collapse = "-"), ".csv")
d = read_csv(file.path(MATCH_STATS_DIR, filename))

## Filter to the focal plots
d = d |>
  filter(!plot_id %in% PLOTS_EXCLUDE)

## Load the parameter set definitions
filename = paste0("itd-paramsets_", paste(FOC_PARAMGROUP, collapse = "-"), ".csv")
param_defs = read_csv(file.path(ITD_PARAMS_DEF_DIR, filename))

d = d |>
  left_join(param_defs, by = join_by("paramset_id" == "paramset_id"))


## Pull in plot data to classify plots as "high" or "low" tree density

# Bind plot-level density to the match stats
field_ref = read_csv(FIELD_REF)
dens = field_ref |>
  select(plot_id = field_plot_id, obs_tree_density = tph) |>
  mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))

d2 = left_join(d, dens, by = join_by("plot_id" == "plot_id"))
d2 = left_join(d, dens, by = join_by("plot_id" == "plot_id"))

mid_dens = median(d2$obs_tree_density)

# Summarize the f-score by parameter set, across all plots and low- and high-density plots separately
d_summ_overall = d2 |>
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) |>
  summarize(allplots = mean(f_score),
            n_plots = n()) |>
  ungroup()

d_summ_lowdens = d2 |>
  filter(obs_tree_density < mid_dens) |>
  group_by(paramset_id) |>
  summarize(lowdens = mean(f_score),
            n_plots_lowdens = n()) |>
  ungroup()

d_summ_highdens = d2 |>
  filter(obs_tree_density >= mid_dens) |>
  group_by(paramset_id) |>
  summarize(highdens = mean(f_score),
            n_plots_highdens = n()) |>
  ungroup()

d_summ_lowhigh = left_join(d_summ_lowdens, d_summ_highdens, by = join_by("paramset_id" == "paramset_id"))
d_summ = left_join(d_summ_overall, d_summ_lowhigh, by = join_by("paramset_id" == "paramset_id"))

d_summ_long = d_summ |>
  pivot_longer(cols = c("allplots", "lowdens", "highdens"),
               names_to = "plot_class",
               values_to = "f_score")

# Only look at F-scores > 0.5
d_summ_long_fig = d_summ_long |>
  mutate(f_score = ifelse(f_score < 0, NA, f_score))

ggplot(d_summ_long_fig, aes(x = lmf_b, y = lmf_a, color = f_score)) +
  geom_point() +
  facet_wrap(~plot_class) +
  scale_color_viridis_c() +
  theme_bw()


## Fit GAMs to model the 2-way interaction

m_overall = gam(allplots ~ te(lmf_b, lmf_a), data = d_summ)
m_lowdens = gam(lowdens ~ te(lmf_b, lmf_a), data = d_summ)
m_highdens = gam(highdens ~ te(lmf_b, lmf_a), data = d_summ)


# Function to make predictions given a model and a data frame of observed values
predict_gam = function(m, d, plot_class) {
  lmf_a_min = min(d$lmf_a)
  lmf_a_max = max(d$lmf_a)
  lmf_b_min = min(d$lmf_b)
  lmf_b_max = max(d$lmf_b)
  param_grid = expand.grid(lmf_a = seq(lmf_a_min, lmf_a_max, length.out = 100),
                          lmf_b = seq(lmf_b_min, lmf_b_max, length.out = 100))
  preds = predict(m, newdata = param_grid, type = "response")
  param_grid$pred_f_score = preds
  param_grid$plot_class = plot_class

  return(param_grid)

}

pred_overall = predict_gam(m_overall, d_summ, plot_class = "allplots")
pred_lowdens = predict_gam(m_lowdens, d_summ, plot_class = "lowdens")
pred_highdens = predict_gam(m_highdens, d_summ, plot_class = "highdens")

params_w_preds = bind_rows(pred_overall, pred_lowdens, pred_highdens)


## Plot this as a heatmap
ggplot(params_w_preds, aes(x = lmf_b, y = lmf_a, fill = pred_f_score)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_bw() +
  facet_wrap(~plot_class)


# Find the optimum
opt = params_w_preds |>
  filter(plot_class == "allplots") |>
  filter(pred_f_score == max(pred_f_score))
opt
