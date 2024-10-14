

final_res = read.csv("/ofo-share/ofo-itd-crossmapping_data/drone/treetops-output/_pred_results.csv")

library(plotly)

final_res$composite = (final_res$recall+ final_res$f_score + final_res$precision)/3

# fig = plot_ly(final_res, x = ~recall, y = ~f_score, z = ~precision, text = ~param_ID, type='scatter3d', 
#               mode = 'markers+text', marker = list(size=5, textposition = 'top middle'))


fig = plot_ly(final_res, x = ~recall, y = ~f_score, z = ~precision, text = ~param_ID, type='scatter3d', 
              mode = 'markers+text', marker = list(size=5, color = ~composite,
                                                   colorscale = 'Viridis',
                                                   colorbar = list(title = 'Composite Score')), textposition = 'top middle')


fig = fig %>% layout(scene = list(xaxix = list(title = "Recall"),
                                  yaxis = list(title = 'F_Scaore'),
                                  zaxis = list(title = 'Precision')),
                     title = "Accuracy Assesment of individual tree detection parameters")

fig