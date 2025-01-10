library(dplyr)
library(plotly)

final_res = read.csv("/ofo-share/ofo-itd-crossmapping_data/drone/predicted-tree-evals/match-stats_paramgroup-01.csv")


params_tested = final_res = read.csv("/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/itd-paramsets_01.csv")

field_all = read.csv("/ofo-share/repos-nayani/ofo-itd-crossmapping/workflow/field_data_all.csv")

match_id = match(final_res$plot_id, field_all$field_plot_id)


final_res = cbind(final_res,field_all[match_id,])




# Count the occurrences of each value in the 'paramset_id' column
paramset_counts <- table(top_5_scores$paramset_id)

paramset_counts


paramset_counts_df <- as.data.frame(paramset_counts)

colnames(paramset_counts_df) <- c("paramset_id", "count")

paramset_counts_df

top_4_params = params_tested[c(132,142,385,608),]



filtered_rows <- final_res[final_res$paramset_id %in% c(132, 142, 385, 608), ]



final_res$composite = (final_res$recall+ final_res$f_score + final_res$precision)/3

# Load necessary library
library(stringr)


fig = plot_ly(final_res, x = ~recall, y = ~f_score, z = ~precision, text = ~paramset_id, type='scatter3d',
              mode = 'markers+text', marker = list(size=5, textposition = 'top middle'))


fig = plot_ly(final_res, x = ~recall, y = ~f_score, z = ~precision, text = ~paramset_id, type='scatter3d', 
              mode = 'markers+text', marker = list(size=5, color = ~composite,
                                                   colorscale = 'Viridis',
                                                   colorbar = list(title = 'Composite Score')), textposition = 'top middle')


fig = fig %>% layout(scene = list(xaxix = list(title = "Recall"),
                                  yaxis = list(title = 'F_Scaore'),
                                  zaxis = list(title = 'Precision')),
                     title = "Accuracy Assesment of individual tree detection parameters")

fig


top_5_scores <- final_res %>%
  group_by(plot_id) %>%
  arrange(desc(f_score), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  select(plot_id, paramset_id, f_score,tph) %>%
  ungroup()


top_5_scores$mathc_id = paste0(top_5_scores$plot_id,"_", top_5_scores$paramset_id)
#top_5_scores= top_5_scores[,-5]
#final_res = final_res[,-29]
final_res$mathc_id = paste0(final_res$plot_id,"_",final_res$paramset_id)


match_Ids = match(top_5_scores$mathc_id,final_res$mathc_id)
final_res_filtered = final_res[match_Ids,]


top_5_scores2 <- filtered_rows %>%
  group_by(plot_id) %>%
  arrange(desc(f_score), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  select(plot_id, paramset_id, f_score,tph) %>%
  ungroup()

# Plots to exclude (those with improperly aligned field reference data)
PLOTS_EXCLUDE = c("0015", "0046", "0105", "0110")


# Remove rows where plot_id is in PLOTS_EXCLUDE
df_filtered <- top_5_scores2[!top_5_scores2$plot_id %in% PLOTS_EXCLUDE, ]

# View the filtered dataframe
df_filtered


# Create the plot
plot(top_5_scores2$paramset_id, top_5_scores2$f_score, 
     xlab = "Paramset ID", ylab = "f_score", 
     main = "Top 5 Composite Scores by Plot ID", 
     pch = 16, col = "blue")

# Add labels for each point (plot_id)
text(top_5_scores$paramset_id, top_5_scores$f_score, 
     labels = top_5_scores$paramset_id, 
     pos = 4, cex = 0.8, col = "red")


# Add labels for each point (plot_id)
text(top_5_scores2$paramset_id, top_5_scores2$f_score, 
     labels = top_5_scores2$tph, 
     pos = 4, cex = 0.8, col = "red")




print(top_5_scores)
