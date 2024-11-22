# Purpose: Take a set of tree detection accuracy results (one per each plot X parameter set
# combination) and visualize them to understand how parameter values affect performance.

#### Setup

## Load packages
library(tidyverse)
library(plotly)
library(htmlwidgets)

## Set constants

# File paths
MATCH_STATS_DIR = "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-tree-evals/"
ITD_PARAMS_DEF_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"
EVAL_FIGURES_DIR = "/ofo-share/ofo-itd-crossmapping_data/itd-paramset-eval-figures/"

# Processing constants for user to define

# Plots to exclude (those with improperly aligned field reference data)
PLOTS_EXCLUDE = c("0015", "0046", "0105", "0110")

# Which group of parameter sets to evaluate
FOC_PARAMGROUP = "04"

#### Functions


#### Workflow

## Load match stats
filename = paste0("match-stats_paramgroup-", paste(FOC_PARAMGROUP, collapse = "-"), ".csv")
d = read_csv(file.path(MATCH_STATS_DIR, filename))

## Filter to the focal plots
d = d |>
  filter(!plot_id %in% PLOTS_EXCLUDE)

## Load the parameter set defs
filename = paste0("itd-paramsets_", paste(FOC_PARAMGROUP, collapse = "-"), ".csv")
param_defs = read_csv(file.path(ITD_PARAMS_DEF_DIR, filename))

## Bind the parameter definitions to the match stats
d = d |>
  left_join(param_defs, by = join_by("paramset_id" == "paramset_id"))

head(d)


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

head(d_long)

# Filter to just a few plots for vis purposes
d_plot = d_long |>
  filter(plot_id %in% c("0005", "0006", "0007", "0014"))

# Make the plot
p = ggplot(d_plot, aes(x = value,
                       y = f_score,
                       color = lmf_rad_max,
                       # fake aesthetics so they appear in the plotly tooltip
                       label1 = lmf_a,
                       label2 = lmf_b,
                       label3 = lmf_c,
                       label4 = lmf_rad_min,
                       label5 = lmf_rad_max)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(end = 0.85) +
  facet_grid(plot_id~param, scales = "free")
p
plotly = ggplotly(p, tooltip = c("label1", "label2", "label3", "label4", "label5"))

filename = paste0("paramset-eval_paramgroup-", FOC_PARAMGROUP, ".html")
saveWidget(plotly, file.path(EVAL_FIGURES_DIR, filename), selfcontained = F, libdir = "lib")


d_mod = d |>
  filter(plot_id == "0005")

library(mgcv)
m = gam(f_score ~ s(lmf_a) + s(lmf_b) + s(lmf_c) + s(lmf_rad_min) + s(lmf_rad_max) + ti(lmf_a, lmf_b), data = d_mod)
plot(m, scheme=1)
summary(m)

d_best = d_mod |>
  arrange(-f_score)

d_best = d_best[1:10, ]
