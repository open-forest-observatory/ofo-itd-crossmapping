library(dplyr)
library(plotly)
library(stringr)



final_res = read.csv("/ofo-share/repos-nayani/ofo-itd-crossmapping/ofo-itd-crossmapping_2/01_pred_results.csv")


params_tested = read.csv("/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/itd-paramsets_01.csv")

field_all = read.csv("/ofo-share/repos-nayani/ofo-itd-crossmapping/workflow/field_data_all.csv")

match_id = match(final_res$plot_ID, field_all$field_plot_id)


final_res = cbind(final_res,field_all[match_id,])


top_5_scores <- final_res %>%
  group_by(plot_ID) %>%
  arrange(desc(f_score), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  select(plot_ID, param_ID, f_score,tph) %>%
  ungroup()


# Count the occurrences of each value in the 'paramset_id' column
paramset_counts <- table(top_5_scores$param_ID)

paramset_counts



paramset_counts_df <- as.data.frame(paramset_counts)

colnames(paramset_counts_df) <- c("Param_ID", "count")

paramset_counts_df

# top_4_params = params_tested[c(132,142,385,608),]  # param group 01

# top_5_params = params_tested[c(25,45,112,237,258),]  # param group 02

top_5_params = params_tested[c(1,2,3,4,5),]  # param group 03

filtered_rows <- final_res[final_res$param_ID %in% c(1,2,3,4,5), ]



final_res$composite = (final_res$recall+ final_res$f_score + final_res$precision)/3



fig = plot_ly(final_res, x = ~recall, y = ~f_score, z = ~precision, text = ~param_ID, type='scatter3d',
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




top_5_scores$mathc_id = paste0(top_5_scores$plot_ID,"_", top_5_scores$param_ID)
#top_5_scores= top_5_scores[,-5]
#final_res = final_res[,-29]
final_res$mathc_id = paste0(final_res$plot_ID,"_",final_res$param_ID)


match_Ids = match(top_5_scores$mathc_id,final_res$mathc_id)
final_res_filtered = final_res[match_Ids,]


top_5_scores2 <- filtered_rows %>%
  group_by(plot_ID) %>%
  arrange(desc(f_score), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  select(plot_ID, param_ID, f_score,tph) %>%
  ungroup()

# Plots to exclude (those with improperly aligned field reference data)
PLOTS_EXCLUDE = c("0015", "0046", "0105", "0110")


# Remove rows where plot_id is in PLOTS_EXCLUDE
df_filtered <- top_5_scores2[!top_5_scores2$plot_ID %in% PLOTS_EXCLUDE, ]

# View the filtered dataframe
df_filtered


# id_385 = final_res[final_res$param_ID==385,] # param group 01

#id_258 = final_res[final_res$param_ID==258,] # param group 02

id_1 = final_res[final_res$param_ID==1,] # param group 03


# Base plot: Plot all points in blue
plot(final_res$tph, final_res$f_score, 
     xlab = "tph", ylab = "f_score", 
     main = "f_score based on tph", 
     pch = 16, col = "blue")

# Overlay red points for param_ID == 385
points(id_1$tph, id_1$f_score, 
       pch = 16, col = "red")


legend("topright", 
       legend = c("All Data", "Parameter ID: 1"), 
       col = c("blue", "red"), 
       pch = 16, 
       bty = "n")  # Remove legend box




final_res1 = read.csv("/ofo-share/repos-nayani/ofo-itd-crossmapping/ofo-itd-crossmapping_2/01_pred_results.csv")
final_res2 = read.csv("/ofo-share/repos-nayani/ofo-itd-crossmapping/ofo-itd-crossmapping_2/02_pred_results.csv")
final_res3 = read.csv("/ofo-share/repos-nayani/ofo-itd-crossmapping/ofo-itd-crossmapping_2/03_pred_results.csv")
final_res4 = read.csv("/ofo-share/repos-nayani/ofo-itd-crossmapping/ofo-itd-crossmapping_2/04_pred_results.csv")

params_tested1 = read.csv("/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/itd-paramsets_01.csv")
params_tested2 = read.csv("/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/itd-paramsets_02.csv")
params_tested3 = read.csv("/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/itd-paramsets_03.csv")
params_tested4 = read.csv("/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/itd-paramsets_04.csv")

field_all = read.csv("/ofo-share/repos-nayani/ofo-itd-crossmapping/workflow/field_data_all.csv")

match_id = match(final_res1$plot_ID, field_all$field_plot_id)
final_res1 = cbind(final_res1,field_all[match_id,])

match_id = match(final_res2$plot_ID, field_all$field_plot_id)
final_res2 = cbind(final_res2,field_all[match_id,])


match_id = match(final_res3$plot_ID, field_all$field_plot_id)
final_res3 = cbind(final_res3,field_all[match_id,])


match_id = match(final_res4$plot_ID, field_all$field_plot_id)
final_res4 = cbind(final_res4,field_all[match_id,])




top_5_scores1 <- final_res1 %>%
  group_by(plot_ID) %>%
  arrange(desc(f_score), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  select(plot_ID, param_ID, f_score,tph) %>%
  ungroup()

# Count the occurrences of each value in the 'paramset_id' column
paramset_counts1 <- table(top_5_scores1$param_ID)
paramset_counts1


top_5_scores2 <- final_res2 %>%
  group_by(plot_ID) %>%
  arrange(desc(f_score), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  select(plot_ID, param_ID, f_score,tph) %>%
  ungroup()

# Count the occurrences of each value in the 'paramset_id' column
paramset_counts2 <- table(top_5_scores2$param_ID)
paramset_counts2

top_5_scores3 <- final_res3 %>%
  group_by(plot_ID) %>%
  arrange(desc(f_score), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  select(plot_ID, param_ID, f_score,tph) %>%
  ungroup()
# Count the occurrences of each value in the 'paramset_id' column
paramset_counts3 <- table(top_5_scores3$param_ID)
paramset_counts3

top_5_scores4 <- final_res4 %>%
  group_by(plot_ID) %>%
  arrange(desc(f_score), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  select(plot_ID, param_ID, f_score,tph) %>%
  ungroup()


# Count the occurrences of each value in the 'paramset_id' column
paramset_counts4 <- table(top_5_scores4$param_ID)
paramset_counts4


# Colors for the datasets
colors <- c("blue", "red", "green", "purple")

# Base plot: Use table1 for the initial plot
plot(top_5_scores1$param_ID, top_5_scores1$f_score, 
     xlab = "param_ID", ylab = "f_score", 
     main = "f_score vs param_ID", 
     pch = 16, col = colors[1], xlim = c(0, 1000), ylim = c(0, 1))  # Adjust limits based on your data

# Add points from other tables
points(top_5_scores2$param_ID, top_5_scores2$f_score, pch = 16, col = colors[2])
points(top_5_scores3$param_ID, top_5_scores3$f_score, pch = 16, col = colors[3])
points(top_5_scores4$param_ID, top_5_scores4$f_score, pch = 16, col = colors[4])

# Add legend to differentiate the datasets
legend("topright", 
       legend = c("param_group-01", "param_group-02", "param_group-03", "param_group-04"), 
       col = colors, 
       pch = 16, 
       bty = "n")  # Remove legend box




top_5_scores11 <- final_res1[final_res1$f_score >= 0.8,] 
top_5_scores21 <- final_res2[final_res2$f_score >= 0.8,] 

top_5_scores31 <- final_res3[final_res3$f_score >= 0.8,] 

top_5_scores41 <- final_res4[final_res4$f_score >= 0.8,] 


# Colors for the datasets
colors <- c("blue", "red", "green", "purple")

# Base plot: Use table1 for the initial plot
plot(top_5_scores11$tph, top_5_scores11$f_score, 
     xlab = "tph", ylab = "f_score", 
     main = "f_score vs tph", 
     pch = 16, col = colors[1], xlim = c(0, 1000), ylim = c(0.79, 1))  # Adjust limits based on your data

# Add points from other tables
points(top_5_scores21$tph, top_5_scores21$f_score, pch = 16, col = colors[2])
points(top_5_scores31$tph, top_5_scores31$f_score, pch = 16, col = colors[3])
points(top_5_scores41$tph, top_5_scores41$f_score, pch = 16, col = colors[4])

# Add legend to differentiate the datasets
legend("topright", 
       legend = c("param_group-01", "param_group-02", "param_group-03", "param_group-04"), 
       col = colors, 
       pch = 16, 
       bty = "n")  # Remove legend box



# Colors for the datasets
colors <- c("blue", "red", "green", "purple")

# Base plot: Use table1 for the initial plot
plot(top_5_scores11$tph, top_5_scores11$f_score, 
     xlab = "tph", ylab = "f_score", 
     main = "f_score vs tph", 
     pch = 16, col = colors[1], xlim = c(0, 500), ylim = c(0.79, 1))  # Adjust limits based on your data

# Add points from other tables
points(top_5_scores21$tph, top_5_scores21$f_score, pch = 16, col = colors[2])
points(top_5_scores31$tph, top_5_scores31$f_score, pch = 16, col = colors[3])
points(top_5_scores41$tph, top_5_scores41$f_score, pch = 16, col = colors[4])

# Add legend to differentiate the datasets
legend("topright", 
       legend = c("param_group-01", "param_group-02", "param_group-03", "param_group-04"), 
       col = colors, 
       pch = 16, 
       bty = "n")  # Remove legend box


# Colors for the datasets
colors <- c("blue", "red", "green", "purple")

# Base plot: Use table1 for the initial plot
plot(top_5_scores11$plot_ID, top_5_scores11$f_score, 
     xlab = "plot_ID", ylab = "f_score", 
     main = "f_score vs plot_ID", 
     pch = 16, col = colors[1], xlim = c(0, 120), ylim = c(0.79, 1))  # Adjust limits based on your data

# Add points from other tables
points(top_5_scores21$plot_ID, top_5_scores21$f_score, pch = 16, col = colors[2])
points(top_5_scores31$plot_ID, top_5_scores31$f_score, pch = 16, col = colors[3])
points(top_5_scores41$plot_ID, top_5_scores41$f_score, pch = 16, col = colors[4])

# Add legend to differentiate the datasets
legend("topright", 
       legend = c("param_group-01", "param_group-02", "param_group-03", "param_group-04"), 
       col = colors, 
       pch = 16, 
       bty = "n")  # Remove legend box


# # Create the plot
# plot(top_5_scores2$param_ID, top_5_scores2$f_score, 
#      xlab = "Paramset ID", ylab = "f_score", 
#      main = "Top 5 Composite Scores by Plot ID", 
#      pch = 16, col = "blue")
# 
# # Add labels for each point (plot_id)
# text(top_5_scores2$param_ID, top_5_scores2$f_score, 
#      labels = top_5_scores$param_ID, 
#      pos = 4, cex = 0.8, col = "red")
# 
# 
# # Add labels for each point (plot_id)
# text(top_5_scores2$paramset_id, top_5_scores2$f_score, 
#      labels = top_5_scores2$tph, 
#      pos = 4, cex = 0.8, col = "red")



# 
# print(top_5_scores)



