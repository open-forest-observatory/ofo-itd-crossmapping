## --------------------------- 1. Long format & join ---------------------------

d_long <- merged_table2 %>%
  pivot_longer(
    cols      = starts_with("lmf_"),
    names_to  = "param",
    values_to = "value"
  ) %>%
  left_join(param_defs, by = join_by("paramset_id == paramset_id"))

## --------------------------- 2. Basic facet plot (all plots) -----------------

p_all <- ggplot(
  d_long,
  aes(
    x      = value,
    y      = f_score,
    color  = lmf_diam_max,
    # extras for Plotly tooltip
    label1 = lmf_a,
    label2 = lmf_b,
    label3 = lmf_c,
    label4 = lmf_diam_min,
    label5 = lmf_diam_max,
    label6 = plot_id
  )
) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(end = 0.85) +
  facet_grid(plot_id ~ param, scales = "free")

p_all

## --------------------------- 3. Example: subset to a few plots ---------------

d_plot <- d_long %>%
  filter(plot_id %in% c("0005"))

p_subset <- ggplot(
  d_plot,
  aes(
    x      = value,
    y      = f_score,
    color  = lmf_diam_max,
    label1 = lmf_a,
    label2 = lmf_b,
    label3 = lmf_c,
    label4 = lmf_diam_min,
    label5 = lmf_diam_max,
    label6 = plot_id
  )
) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(end = 0.85) +
  facet_grid(plot_id ~ param, scales = "free")

p_subset

## --------------------------- 4. Focus on key params & per-plot PNGs ----------

d_long_filtered <- d_long %>%
  filter(param %in% c("lmf_a", "lmf_b", "lmf_diam_max"))

p_filtered <- ggplot(
  d_long_filtered,
  aes(
    x      = value,
    y      = f_score,
    color  = f_score,
    label1 = lmf_a,
    label2 = lmf_b,
    label5 = lmf_diam_max,
    label6 = plot_id
  )
) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(end = 0.85) +
  facet_grid(plot_id ~ param, scales = "free")

p_filtered

# One PNG per plot_id
plot_ids <- unique(d_long_filtered$plot_id)

purrr::walk(plot_ids, function(pid) {
  df_plot <- d_long_filtered %>%
    filter(plot_id == pid)
  
  p_pid <- ggplot(
    df_plot,
    aes(
      x      = value,
      y      = f_score,
      color  = f_score,
      label1 = lmf_a,
      label2 = lmf_b,
      label6 = plot_id
    )
  ) +
    geom_point(alpha = 0.7) +
    scale_color_viridis_c(end = 0.85) +
    facet_grid(. ~ param, scales = "free") +
    ggtitle(paste0("Plot ID: ", pid))
  
  ggsave(
    filename = paste0(pid, "_600.png"),
    plot     = p_pid,
    width    = 8,
    height   = 6,
    dpi      = 300
  )
  
  cat("Saved plot for", pid, "\n")
})

## --------------------------- 5. Plotly interactive view ----------------------

# Use the filtered facet plot for interactivity (change to p_all if you prefer)
plotly_obj <- ggplotly(
  p_filtered,
  tooltip = c("label1", "label2", "label5", "label6")
)

plotly_obj

html_name <- paste0("paramset-eval_paramgroup-", FOC_PARAMGROUP, ".html")

saveWidget(
  plotly_obj,
  file.path(EVAL_FIGURES_DIR, html_name),
  selfcontained = TRUE,
  libdir       = "lib"
)

## --------------------------- 6. GAMs: param effects on F-score ---------------

# Example: single plot (0007)
d_mod <- d %>%
  filter(plot_id == "0007")

m_plot <- gam(
  f_score ~ s(lmf_a) + s(lmf_b),
  data    = d_mod
)
plot(m_plot, scheme = 1)

# Across all plots (a, b)
m_all <- gam(
  f_score ~ s(lmf_a) + s(lmf_b),
  data    = d
)
plot(m_all, scheme = 1)

# Across all plots: diameter max only
m_diam <- gam(
  f_score ~ s(lmf_diam_max),
  data    = d
)
plot(m_diam, scheme = 1)

## --------------------------- 7. Top 10 paramsets for a given plot ------------

d_best <- d %>%
  filter(plot_id == "0007") %>%
  arrange(desc(f_score)) %>%
  slice(1:10)

d_best

## --------------------------- 8. Density-based summaries (Option 5) -----------

FIELD_REF <- "/ofo-share/repos-nayani/ofo-itd-crossmapping/workflow/field_data_all.csv"
field_ref <- read_csv(FIELD_REF)

dens <- field_ref %>%
  select(plot_id = field_plot_id, obs_tree_density = tph) %>%
  mutate(plot_id = str_pad(plot_id, width = 4, side = "left", pad = "0"))

# Clean duplicate .x/.y columns once
colnames(merged_table) <- gsub("\\.x$", "", colnames(merged_table))
colnames(merged_table) <- gsub("\\.y$", "", colnames(merged_table))
merged_table <- merged_table[, !duplicated(colnames(merged_table))]

# Join density to match stats
d2 <- merged_table %>%
  left_join(dens, by = join_by("plot_id == plot_id"))

mid_dens <- median(d2$obs_tree_density, na.rm = TRUE)

# Mean F-score per paramset across all / low / high density
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
  left_join(d_fig_lowdens,  by = "paramset_id") %>%
  left_join(d_fig_highdens, by = "paramset_id")

d_fig_long <- d_fig %>%
  pivot_longer(
    cols      = c("allplots", "lowdens", "highdens"),
    names_to  = "f_score_type",
    values_to = "f_score"
  )

p_density <- ggplot(
  d_fig_long,
  aes(x = lmf_b, y = lmf_a, color = f_score)
) +
  geom_point() +
  facet_wrap(~ f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

p_density

## --------------------------- 9. Same summaries, high f-score subset ----------

d2_high_fs <- d2 %>%
  filter(f_score >= 0.7)

mid_dens_high <- median(d2_high_fs$obs_tree_density, na.rm = TRUE)

d_fig_overall_h <- d2_high_fs %>%
  group_by(paramset_id, lmf_a, lmf_b, lmf_c, lmf_diam_min, lmf_diam_max) %>%
  summarize(allplots = mean(f_score), n_plots = n(), .groups = "drop")

d_fig_lowdens_h <- d2_high_fs %>%
  filter(obs_tree_density < mid_dens_high) %>%
  group_by(paramset_id) %>%
  summarize(lowdens = mean(f_score), n_plots_lowdens = n(), .groups = "drop")

d_fig_highdens_h <- d2_high_fs %>%
  filter(obs_tree_density >= mid_dens_high) %>%
  group_by(paramset_id) %%
  summarize(highdens = mean(f_score), n_plots_highdens = n(), .groups = "drop")

d_fig_h <- d_fig_overall_h %>%
  left_join(d_fig_lowdens_h,  by = "paramset_id") %>%
  left_join(d_fig_highdens_h, by = "paramset_id")

d_fig_long_h <- d_fig_h %>%
  pivot_longer(
    cols      = c("allplots", "lowdens", "highdens"),
    names_to  = "f_score_type",
    values_to = "f_score"
  )

p_density_h <- ggplot(
  d_fig_long_h,
  aes(x = lmf_b, y = lmf_a, color = f_score)
) +
  geom_point() +
  facet_wrap(~ f_score_type) +
  scale_color_viridis_c() +
  theme_bw()

p_density_h

## --------------------------- 10. Extra GAMs w/ density & structure -----------

low_all  <- d2 %>% filter(obs_tree_density <  mid_dens)
high_all <- d2 %>% filter(obs_tree_density >= mid_dens)

m_high <- gam(f_score ~ s(lmf_a) + s(lmf_b), data = high_all)
plot(m_high, scheme = 1)

ggplot(d2_high_fs, aes(x = CHI, y = Clumping_Index, color = f_score)) +
  geom_point() +
  facet_wrap(~ plot_id) +
  scale_color_viridis_c() +
  theme_bw()

# GAM with multiple structure covariates (assuming merged_table4 exists)
d2_struct <- merged_table4 %>%
  left_join(dens, by = join_by("plot_id == plot_id"))

m_struct <- gam(
  f_score ~ s(lmf_a) + s(lmf_b) +
    s(lmf_diam_max) + s(lmf_c) + s(lmf_diam_min) +
    s(CHI) + s(media_height) + s(Clumping_Index) +
    s(obs_tree_density),
  data = d2_struct
)
summary(m_struct)
draw(m_struct)

## --------------------------- 11. Additional GAM diagnostics / interactions ----

library(gratia)

# Derivatives and interaction terms example
derivatives(m_struct, term = "s(CHI)")

m_interact <- gam(
  f_score ~ te(lmf_a, CHI) + te(lmf_b, CHI) +
    s(media_height) + s(Clumping_Index) + s(obs_tree_density),
  data = d2_struct
)

draw(m_interact)
vis.gam(m_interact, view = c("lmf_a", "CHI"), plot.type = "persp")
vis.gam(m_interact, view = c("lmf_b", "CHI"), plot.type = "persp")
