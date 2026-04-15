## --------------------------- 0. Packages -------------------------------------

# Install if missing
pkgs <- c(
  "terra", "raster", "lidR", "sf", "dplyr", "data.table", "stringr",
  "FNN", "dbscan", "purrr", "tibble", "readr", "tidyverse",
  "plotly", "htmlwidgets", "mgcv", "scales"
)
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

# Load libraries
library(terra)
library(raster)
library(lidR)
library(sf)
library(dplyr)
library(data.table)
library(stringr)
library(FNN)
library(dbscan)
library(purrr)
library(tibble)
library(readr)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(mgcv)
library(scales)

## --------------------------- 1. Paths & constants ---------------------------

MATCH_STATS_DIR <- "/ofo-share/ofo-itd-crossmapping_data/drone/predicted-tree-evals/"
ITD_PARAMS_DEF_DIR <- "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"
EVAL_FIGURES_DIR <- "/ofo-share/ofo-itd-crossmapping_data/itd-paramset-eval-figures/"
GPKG_DIR <- "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/trees/"
FIELD_REF <- "/ofo-share/ofo-itd-crossmapping_data/site-selection/processed/selected-field-and-drone-plots_v1.csv"

# Plots to exclude (bad field alignment)
PLOTS_EXCLUDE <- c("0015", "0046", "0100", "0110")

# Parameter group to evaluate
FOC_PARAMGROUP <- "7010-1"

# Output dirs
dir.create("paramID_plots_all_fined_new_set", showWarnings = FALSE)
dir.create("paramID_plots_second_refined", showWarnings = FALSE)
dir.create("paramID_plots", showWarnings = FALSE)

## --------------------------- 2. Helper functions ----------------------------

# KNN-based CHI on point data
compute_chi_points <- function(df, k = 3) {
  df_clean <- df %>% filter(!is.na(X), !is.na(Y), !is.na(height))
  coords <- as.matrix(df_clean[, c("X", "Y")])
  heights <- df_clean$height
  
  if (nrow(coords) < k) return(NA_real_)
  
  knn_result <- FNN::get.knn(coords, k = k)
  chi_vals <- sapply(seq_len(nrow(knn_result$nn.index)), function(i) {
    sd(heights[knn_result$nn.index[i, ]], na.rm = TRUE)
  })
  
  mean(chi_vals, na.rm = TRUE)
}

# DBSCAN-based clumping index on point data
compute_clumping_index_points <- function(df, threshold = 2, eps = 1, minPts = 3) {
  canopy_points <- df %>% filter(height > threshold)
  total_points <- nrow(df)
  if (nrow(canopy_points) < minPts || total_points == 0) return(0)
  
  coords <- as.matrix(canopy_points[, c("X", "Y")])
  db <- dbscan::dbscan(coords, eps = eps, minPts = minPts)
  
  num_clumps <- length(unique(db$cluster[db$cluster > 0]))
  num_clumps / total_points
}

# Height / DBH metrics on point data
compute_height_metrics_points <- function(df) {
  h <- df$height
  b <- df$dbh
  tibble(
    mean_height = mean(h, na.rm = TRUE),
    max_height  = max(h, na.rm = TRUE),
    media_height = median(h, na.rm = TRUE),
    mean_dbh    = mean(b, na.rm = TRUE),
    max_dbh     = max(b, na.rm = TRUE),
    media_dbh   = median(b, na.rm = TRUE),
    sd_height   = sd(h, na.rm = TRUE),
    q25         = quantile(h, 0.25, na.rm = TRUE),
    q50         = quantile(h, 0.5,  na.rm = TRUE),
    q75         = quantile(h, 0.75, na.rm = TRUE)
  )
}

# Helper for set-cover summary labels
collapse_plots_multiline <- function(x) {
  paste0("(\n", paste(sort(unique(x)), collapse = "\n"), "\n)")
}

# Greedy set cover based on incidence matrix (rows = param sets, cols = plots)
greedy_set_cover <- function(inc) {
  inc <- inc * 1
  plots <- colnames(inc)
  covered <- setNames(rep(FALSE, length(plots)), plots)
  chosen <- character(0)
  
  while (any(!covered)) {
    scores <- rowSums(inc[, !covered, drop = FALSE])
    pick <- names(which.max(scores))[1]
    chosen <- c(chosen, pick)
    covered[colnames(inc)[inc[pick, ] > 0]] <- TRUE
    inc <- inc[setdiff(rownames(inc), pick), , drop = FALSE]
  }
  chosen
}

# Assign each plot to first chosen paramset that covers it
assign_plots_to_chosen <- function(inc, chosen) {
  tibble(plot_id = colnames(inc)) %>%
    rowwise() %>%
    mutate(chosen_paramset = {
      hits <- chosen[inc[chosen, plot_id] > 0]
      if (length(hits) == 0) NA_character_ else hits[1]
    }) %>%
    ungroup()
}

# Build paramset-level summary for plotting
build_cover_plotdf <- function(assignment_df, df_full, param_lookup) {
  df_full %>%
    inner_join(assignment_df, by = "plot_id") %>%
    filter(paramset_id == chosen_paramset) %>%
    group_by(paramset_id) %>%
    summarize(
      plots_str = collapse_plots_multiline(plot_id),
      mean_f    = mean(f_score, na.rm = TRUE),
      n_plots   = n_distinct(plot_id),
      .groups   = "drop"
    ) %>%
    left_join(param_lookup, by = "paramset_id")
}

# Plot paramset locations with dual labels
plot_paramsets_dual_labels <- function(plotdf, title_lbl) {
  ggplot(plotdf, aes(x = lmf_a, y = lmf_b)) +
    geom_point(aes(color = mean_f, size = n_plots), alpha = 0.9) +
    geom_text(aes(label = paramset_id),
              vjust = -0.8, hjust = 0.5, size = 3.2, fontface = "bold") +
    geom_text(aes(label = plots_str),
              hjust = 0, nudge_x = 0.05, size = 3) +
    scale_size_continuous(name = "# plots represented") +
    scale_color_viridis_c(name = "mean F-score", end = 0.97) +
    labs(title = title_lbl, x = "lmf_a", y = "lmf_b") +
    theme_minimal(base_size = 12)
}

## --------------------------- 3. Match stats & param defs ---------------------

# Load match stats and filter plots
d <- read_csv(file.path(MATCH_STATS_DIR, paste0("match-stats_paramgroup-", FOC_PARAMGROUP, ".csv"))) %>%
  filter(!plot_ID %in% PLOTS_EXCLUDE)

# Harmonize plot_id name
if ("plot_ID" %in% colnames(d)) {
  d <- d %>% rename(plot_id = plot_ID)
}

# Load parameter definitions
param_defs <- read_csv(file.path(ITD_PARAMS_DEF_DIR,
                                 paste0("itd-paramsets_", FOC_PARAMGROUP, ".csv")))

# Attach parameter definitions to match stats
d <- d %>%
  left_join(param_defs, by = join_by("paramset_id" == "paramset_id"))

## --------------------------- 4. Field tree metrics (CHI, clumping, height) ---

gpkg_files <- list.files(GPKG_DIR, pattern = "\\.gpkg$", full.names = TRUE)

results_gpkg <- map_df(gpkg_files, function(file) {
  message("Processing: ", file)
  pts <- sf::st_read(file, quiet = TRUE)
  pts <- pts[pts$height >= 5, ]
  
  df <- sf::st_drop_geometry(pts)
  
  # Ensure height column exists
  if (!"height" %in% names(df)) {
    if ("canopy_height" %in% names(df)) {
      df$height <- df$canopy_height
    } else {
      stop(paste("No height column found in", file))
    }
  }
  
  coords <- sf::st_coordinates(pts)
  df$X <- coords[, 1]
  df$Y <- coords[, 2]
  
  chi          <- compute_chi_points(df)
  clumping     <- compute_clumping_index_points(df)
  height_stats <- compute_height_metrics_points(df)
  
  tibble(
    file           = basename(file),
    CHI            = chi,
    Clumping_Index = clumping
  ) %>%
    bind_cols(height_stats)
})

# Derive plot_id from file
results_gpkg <- results_gpkg %>%
  mutate(plot_id = str_pad(parse_number(file), width = 4, pad = "0"))

print(results_gpkg)

## --------------------------- 5. Merge CHI/clumping with match stats ----------

merged_table <- d %>%
  left_join(results_gpkg, by = "plot_id")

## --------------------------- 6. Param-by-f_score plots -----------------------

# Long format for lmf_ parameters
d_long <- merged_table %>%
  pivot_longer(cols = starts_with("lmf_"),
               names_to = "param",
               values_to = "value") %>%
  left_join(param_defs, by = join_by("paramset_id" == "paramset_id"))

# Example: facet by plot and parameter, colored by f_score
p <- ggplot(d_long, aes(x = value, y = f_score, color = f_score,
                        label1 = lmf_a, label2 = lmf_b, label3 = lmf_c,
                        label4 = lmf_diam_min, label5 = lmf_diam_max,
                        label6 = plot_id)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(end = 0.85) +
  facet_grid(plot_id ~ param, scales = "free")

ggsave(
  filename = "summary_7010-1.png",
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

## --------------------------- 7. Bind field-level density & structure ---------

field_ref <- read_csv(FIELD_REF)

dens <- field_ref %>%
  select(plot_id = field_plot_id,
         obs_tree_density = tph,
         min_ht,
         min_ht_ohvis,
         forest_type_dy) %>%
  mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0")) %>%
  na.omit()

d2 <- merged_table %>%
  left_join(dens, by = "plot_id") %>%
  na.omit()

## --------------------------- 8. f_score vs density (low/high TPH) ------------

mid_dens <- median(d2$obs_tree_density, na.rm = TRUE)

d_fig_overall <- d2 %>%
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) %>%
  summarize(allplots = mean(f_score), n_plots = n(), .groups = "drop")

d_fig_lowdens <- d2 %>%
  filter(obs_tree_density < mid_dens) %>%
  group_by(paramset_id) %>%
  summarize(lowdens = mean(f_score), n_plots_lowdens = n(), .groups = "drop")

d_fig_highdens <- d2 %>%
  filter(obs_tree_density >= mid_dens) %>%
  group_by(paramset_id) %>%
  summarize(highdens = mean(f_score), n_plots_highdens = n(), .groups = "drop")

d_fig <- d_fig_overall %>%
  left_join(d_fig_lowdens, by = "paramset_id") %>%
  left_join(d_fig_highdens, by = "paramset_id") %>%
  pivot_longer(cols = c("allplots", "lowdens", "highdens"),
               names_to = "f_score_type",
               values_to = "f_score")

p <- ggplot(d_fig, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point() +
  facet_wrap(~ f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

ggsave(
  filename = "paramID_plots_all_fined_new_set/low_high_tph_7010-1.png",
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

## --------------------------- 9. f_score vs median height (low/high) -----------

mid_mht <- median(merged_table$media_height, na.rm = TRUE)

d_fig_overall_ht <- d2 %>%
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) %>%
  summarize(allplots = mean(f_score), n_plots = n(), .groups = "drop")

d_fig_lowht <- d2 %>%
  filter(media_height < mid_mht) %>%
  group_by(paramset_id) %>%
  summarize(lowht = mean(f_score), n_plots_lowheight = n(), .groups = "drop")

d_fig_highht <- d2 %>%
  filter(media_height >= mid_mht) %>%
  group_by(paramset_id) %>%
  summarize(highht = mean(f_score), n_plots_highheight = n(), .groups = "drop")

d_fig_ht <- d_fig_overall_ht %>%
  left_join(d_fig_lowht, by = "paramset_id") %>%
  left_join(d_fig_highht, by = "paramset_id") %>%
  pivot_longer(cols = c("allplots", "lowht", "highht"),
               names_to = "f_score_type",
               values_to = "f_score")

p <- ggplot(d_fig_ht, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point() +
  facet_wrap(~ f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

ggsave(
  filename = "paramID_plots_all_fined_new_set/low_high_height-7010-1.png",
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

## --------------------------- 10. f_score vs DBH (low/high) -------------------

mid_dbh <- median(d2$media_dbh, na.rm = TRUE)

d_fig_overall_dbh <- d2 %>%
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) %>%
  summarize(allplots = mean(f_score), n_plots = n(), .groups = "drop")

d_fig_lowdbh <- d2 %>%
  filter(media_dbh < mid_dbh) %>%
  group_by(paramset_id) %>%
  summarize(lowdbh = mean(f_score), n_plots_lowdens = n(), .groups = "drop")

d_fig_highdbh <- d2 %>%
  filter(media_dbh >= mid_dbh) %>%
  group_by(paramset_id) %>%
  summarize(higdbh = mean(f_score), n_plots_highdens = n(), .groups = "drop")

d_fig_dbh <- d_fig_overall_dbh %>%
  left_join(d_fig_lowdbh, by = "paramset_id") %>%
  left_join(d_fig_highdbh, by = "paramset_id") %>%
  pivot_longer(cols = c("allplots", "lowdbh", "higdbh"),
               names_to = "f_score_type",
               values_to = "f_score")

p <- ggplot(d_fig_dbh, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point() +
  facet_wrap(~ f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

ggsave(
  filename = "paramID_plots_second_refined/low_high_dbh-7010-1.png",
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

## --------------------------- 11. f_score vs forest type ----------------------

veg_cover <- field_ref %>%
  select(plot_id = field_plot_id, forest_ty = forest_type_dy) %>%
  mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0")) %>%
  na.omit()

d5 <- merged_table %>%
  left_join(veg_cover, by = "plot_id") %>%
  na.omit()

d_fig_overall <- d5 %>%
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) %>%
  summarize(allplots = mean(f_score), n_plots = n(), .groups = "drop")

d_fig_cover <- d5 %>%
  group_by(forest_ty, paramset_id) %>%
  summarize(mean_f_score = mean(f_score, na.rm = TRUE),
            n_plots = n(), .groups = "drop")

d_fig <- d_fig_overall %>%
  left_join(d_fig_cover, by = "paramset_id")

p <- ggplot(d_fig, aes(x = lmf_a, y = lmf_b, color = mean_f_score)) +
  geom_point() +
  facet_wrap(~ forest_ty) +
  scale_color_viridis_c() +
  theme_bw()

ggsave(
  filename = "paramID_plots_second_refined/vege_cover-7010-1.png",
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

## --------------------------- 12. f_score vs clumping & sd_height -------------

mid_clump <- median(d2$Clumping_Index, na.rm = TRUE)
mid_sdh   <- median(d2$sd_height,      na.rm = TRUE)

# Clumping
d_fig_overall_clump <- d2 %>%
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) %>%
  summarize(allplots = mean(f_score), n_plots = n(), .groups = "drop")

d_fig_lowclump <- d2 %>%
  filter(Clumping_Index < mid_clump) %>%
  group_by(paramset_id) %>%
  summarize(lowclump = mean(f_score), n_plots_lowdens = n(), .groups = "drop")

d_fig_highclump <- d2 %>%
  filter(Clumping_Index >= mid_clump) %>%
  group_by(paramset_id) %>%
  summarize(higclump = mean(f_score), n_plots_highdens = n(), .groups = "drop")

d_fig_clump <- d_fig_overall_clump %>%
  left_join(d_fig_lowclump, by = "paramset_id") %>%
  left_join(d_fig_highclump, by = "paramset_id") %>%
  pivot_longer(cols = c("allplots", "lowclump", "higclump"),
               names_to = "f_score_type",
               values_to = "f_score")

p <- ggplot(d_fig_clump, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point() +
  facet_wrap(~ f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

ggsave(
  filename = "paramID_plots_second_refined/clumping-7010-1.png",
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

# SDH
d_fig_overall_sdh <- d2 %>%
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) %>%
  summarize(allplots = mean(f_score), n_plots = n(), .groups = "drop")

d_fig_lowsdh <- d2 %>%
  filter(sd_height < mid_sdh) %>%
  group_by(paramset_id) %>%
  summarize(lowsdh = mean(f_score), n_plots_lowdens = n(), .groups = "drop")

d_fig_highsdh <- d2 %>%
  filter(sd_height >= mid_sdh) %>%
  group_by(paramset_id) %>%
  summarize(higsdh = mean(f_score), n_plots_highdens = n(), .groups = "drop")

d_fig_sdh <- d_fig_overall_sdh %>%
  left_join(d_fig_lowsdh, by = "paramset_id") %>%
  left_join(d_fig_highsdh, by = "paramset_id") %>%
  pivot_longer(cols = c("allplots", "lowsdh", "higsdh"),
               names_to = "f_score_type",
               values_to = "f_score")

p <- ggplot(d_fig_sdh, aes(x = lmf_a, y = lmf_b, color = f_score)) +
  geom_point() +
  facet_wrap(~ f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

ggsave(
  filename = "paramID_plots_second_refined/sdh-7010-1.png",
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

## --------------------------- 13. Top paramsets (best overall, per plot) ------

best_overall <- merged_table %>%
  group_by(paramset_id) %>%
  summarise(avg_f_score = mean(f_score), .groups = "drop") %>%
  arrange(desc(avg_f_score))

# Top 10
top10 <- best_overall %>% slice_head(n = 10)

# Best paramset per plot
best_per_plot <- merged_table %>%
  group_by(plot_id) %>%
  slice_max(f_score, n = 1, with_ties = FALSE) %>%
  ungroup()

# Plot best paramset per plot (bar)
p <- ggplot(best_per_plot, aes(x = plot_id, y = f_score, fill = paramset_id)) +
  geom_col() +
  labs(title = "Best paramset per plot (based on f_score)",
       x = "Plot ID", y = "F-score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  filename = "paramID_plots_second_refined/bets_per_plot-70101-1.png",
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

# Top 10 paramsets overall
p <- ggplot(top10, aes(x = reorder(paramset_id, avg_f_score), y = avg_f_score)) +
  geom_point() +
  labs(title = "Top 10 Paramsets by Average F-score",
       x = "Paramset ID", y = "Avg F-score") +
  coord_flip() +
  theme_minimal()

ggsave(
  filename = "paramID_plots_second_refined/top_10_overall-7010-1.png",
  plot = p,
  width = 6, height = 5, dpi = 300,
  bg = "white"
)

## --------------------------- 14. Plot-level param-space maps -----------------

# Add plot id padded to field_ref for join later
dens2 <- field_ref %>%
  select(plot_id = field_plot_id) %>%
  mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))

field_ref <- cbind(field_ref, dens2)

merged_all <- merged_table %>%
  left_join(field_ref, by = "plot_id")

# Parameter-space plot for each plot_id
plot_ids <- unique(merged_all$plot_id)

for (pid in plot_ids) {
  message("Plotting parameter space for plot ", pid)
  df_plot <- merged_all %>% filter(plot_id == pid)
  
  p <- ggplot(df_plot, aes(x = lmf_a, y = lmf_b, color = f_score)) +
    geom_point(size = 2, alpha = 0.8) +
    scale_color_viridis_c(option = "D", name = "F-score") +
    labs(
      title = paste("Parameter Space for Plot", pid),
      x = "Parameter a",
      y = "Parameter b"
    ) +
    theme_minimal()
  
  ggsave(
    filename = paste0("paramID_plots_second_refined/plot_", pid, ".png"),
    plot = p,
    width = 6, height = 5, dpi = 300,
    bg = "white"
  )
}

## --------------------------- 15. GAM models ----------------------------------

m_full <- gam(
  f_score ~ s(lmf_a) + s(lmf_b) + forest_type_dy +
    s(tph) + s(mean_height) + s(Clumping_Index),
  data = merged_all,
  method = "REML"
)
gam.check(m_full)
summary(m_full)
plot(m_full, scheme = 1)

m_simple <- gam(
  f_score ~ s(lmf_a) + s(lmf_b),
  data = merged_all,
  method = "REML"
)
gam.check(m_simple)
summary(m_simple)

## --------------------------- 16. Best params by env groups -------------------

best_params <- merged_all %>%
  filter(!is.na(f_score)) %>%
  group_by(forest_type_dy, mean_height, tph) %>%
  slice_max(order_by = f_score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(forest_type_dy, mean_height, tph, lmf_a, lmf_b, f_score)

write_csv(best_params, "paramID_plots/best_param_groups.csv")

## --------------------------- 17. Set-cover style selection -------------------

# Best-tied paramsets per plot
eps <- 1e-8
best_per_plot_ties <- merged_all %>%
  group_by(plot_id) %>%
  mutate(max_f = max(f_score, na.rm = TRUE)) %>%
  filter(abs(f_score - max_f) <= eps) %>%
  select(plot_id, paramset_id, f_score) %>%
  ungroup()

inc <- with(best_per_plot_ties, table(paramset_id, plot_id))
inc <- (inc > 0) * 1

stopifnot(all(colSums(inc) > 0))

greedy_chosen_sets <- greedy_set_cover(inc)
greedy_assignment <- assign_plots_to_chosen(inc, greedy_chosen_sets)

param_lookup <- merged_all %>%
  distinct(paramset_id, lmf_a, lmf_b)

greedy_plotdf <- build_cover_plotdf(greedy_assignment, merged_all, param_lookup)

p_greedy <- plot_paramsets_dual_labels(greedy_plotdf,
                                       "Greedy set cover (param sets)")
p_greedy

ggsave(
  filename = "paramID_plots/greedy_set_cover-7010-1.png",
  plot = p_greedy,
  width = 7, height = 5, dpi = 300,
  bg = "white"
)
