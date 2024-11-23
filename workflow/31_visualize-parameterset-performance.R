# Purpose: Take a set of tree detection accuracy results (one per each plot X parameter set
# combination) and visualize them to understand how parameter values affect performance.

#### Setup

## Load packages
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(mgcv)

## Set constants

# File paths
MATCH_STATS_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-tree-evals/"
ITD_PARAMS_DEF_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"
EVAL_FIGURES_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramset-eval-figures/"

# Processing constants for user to define

# Plots to exclude (those with improperly aligned field reference data)
PLOTS_EXCLUDE = c("0015", "0046", "0105", "0110")

# Which group of parameter sets to evaluate. Set to the same value as in the previous script (30).
FOC_PARAMGROUP = "01"


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

## Bind the parameter definitions to the match stats
d = d |>
  left_join(param_defs, by = join_by("paramset_id" == "paramset_id"))


## Prepare to make a multi-facet plot, where each facet is a different parameter, and the x-axis is
# the parameter value, and the y-axis is the f-score

# Pivot longer so that there is a column with the parameter name and a column with the parameter value
d_long = d |>
  pivot_longer(cols = starts_with("lmf_"), names_to = "param", values_to = "value")

# Bind the param values back onto the long-form data frame, so that we can look up what parameter
# values (besides the focal parameter) were associated with the f-score for the focal parameter
# value

d_long = d_long |>
  left_join(param_defs, by = join_by("paramset_id" == "paramset_id")) 

## VIS OPTION 1: Scatter plot by parameter

# Filter to just a few plots for vis purposes. NOTE: will want to expand this to all plots eventually.
d_plot = d_long |>
  filter(plot_id %in% c("0005", "0006", "0007", "0014"))

# Make the plot
p = ggplot(d_plot, aes(x = value,
                       y = f_score,
                       color = lmf_diam_max,
                       # fake aesthetics so they appear in the plotly tooltip
                       label1 = lmf_a,
                       label2 = lmf_b,
                       label3 = lmf_c,
                       label4 = lmf_diam_min,
                       label5 = lmf_diam_max)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(end = 0.85) +
  facet_grid(plot_id~param, scales = "free")
p


## VIS OPTION 2: Scatter plot by parameter, with plotly for interactive parameter lookup for each point in the plot

plotly = ggplotly(p, tooltip = c("label1", "label2", "label3", "label4", "label5"))
# plotly
# If the interactive plot does not work well in your IDE, save it as an HTML file, which you can
# then open in a browser.
filename = paste0("paramset-eval_paramgroup-", FOC_PARAMGROUP, ".html")
saveWidget(plotly, file.path(EVAL_FIGURES_DIR, filename), selfcontained = TRUE, libdir = "lib")


## VIS OPTION 3: Fit a GAM to the data and visualize the partial effects of the parameters on the f-score

# Filter to just one plot, though may also want to fit across all plots in the future
# NOTE: Try different plots here to see if the fit is different
d_mod = d |>
  filter(plot_id == "0007")

m = gam(f_score ~ s(lmf_a) + s(lmf_b) + s(lmf_c) + s(lmf_diam_min) + s(lmf_diam_max), data = d_mod)
plot(m, scheme = 1)


## VIS OPTION 4: Select the top 10 parameter sets for a given plot and see what their parameter values have in common

# Filter to just one plot for now, though we should also see if the pattern is consistent or variable across all plots

d_best = d |>
  filter(plot_id == "0007") |>
  arrange(-f_score) |>
  slice(1:10)
d_best
