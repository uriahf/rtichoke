# fake_example <- plot_ly(
#   x = ~x,
#   y = ~y,
#   hoverinfo = "text",
#   text = ~text,
#   color = ~reference_group,
#   colors = c("reference_line" = "black")
# ) |>
#   add_markers(
#     data = data.frame(
#       x = c(1, 2, 3),
#       y = c(1, 2, 3),
#       stratified_by = c(1,2,3),
#       reference_group = "reference_line",
#       text = "bla1"
#     ),
#     frame =~ stratified_by
#   )
# 
# plotly_build(fake_example)$x$data[[1]]
# names(plotly_build(fake_example)$x$layout)
# plotly_build(fake_example)$x$layout$xaxis
# plotly_build(fake_example)$x$layout$updatemenus
# 
# length(plotly_build(fake_example)$x$layout$sliders[[1]]$steps)
# 
# names(plotly_build(fake_example)$x$layout$sliders[[1]]$steps[[1]])
# 
# plotly_build(fake_example)$x$layout$sliders[[1]]$steps[[1]]$value
# plotly_build(fake_example)$x$layout$sliders[[1]]$steps[[2]]
# plotly_build(fake_example)$x$layout$sliders[[1]]$steps[[3]]
# 
# 
# plotly_build(fake_example)$x$frames[[1]]$data[[1]]
# plotly_build(fake_example)$x$frames[[2]]$data[[1]]
# plotly_build(fake_example)$x$frames[[3]]$data[[1]]
# 
# 
# names(plotly_build(fake_example)$x$frames[[1]])
# plotly_build(fake_example)$x$frames[2]
# plotly_build(fake_example)$x$frames[3]
# 
# 
# plotly_build(fake_example)$x$layout$sliders[[1]]$steps[[2]]
# plotly_build(fake_example)$x$layout$sliders[[1]]$steps[[3]]
# 
# plotly_build(fake_example)$x$layout$sliders[[1]]$steps[[1]]$
# 
# names(plotly_build(fake_example)$x$layout$sliders[[1]])
# 
# plotly_build(fake_example)$x$layout$sliders[[1]]$currentvalue
# plotly_build(fake_example)$x$layout$sliders[[1]]$visible
# plotly_build(fake_example)$x$layout$sliders[[1]]$pad
# 
# 
# 
# plotly_build(fake_example)$x$layout$sliders[[1]]$steps
# 
# 
# plotly_build(fake_example)$x$frames
# 
# 
# margin
# plotly_build(fake_example)$x$frames
# 
# 
# 
# plotly_build(fake_example)$x[c("data", "layout")]
# 
# 
# plotly_list <- plotly:::to_JSON(plotly_build(fake_example)$x[c("data", "layout", "config")]) |> 
#   jsonlite::fromJSON() 
# 
# 
# plotly_list$data[[3]]
# plotly_list[[2]]
# plotly_list[[3]]
# 
#  |> 
#   add_trace(
#     data = data.frame(
#       x = c(1, 4, 3),
#       y = c(1, 2, 3),
#       stratified_by = c(1,2,3),
#       reference_group = "model",
#       text = "bla2"
#     ),
#     type = 'scatter',
#     mode = 'lines+markers',
#     line = list(dash = 'solid')
#   ) |> 
#   add_markers(
#     data = data.frame(
#       x = c(2, 4, 5),
#       y = c(2, 4, 5),
#       stratified_by = c(1,2,3),
#       reference_group = "interactive_marker",
#       text = "bla2"
#     ),
#     frame =~ stratified_by,
#     marker =  list(
#       size = 12,
#       line = list(
#         width = 3,
#         color = I("black")
#       )
#     )
#   )
# 
# plotly_build(fake_example)$x$data[[1]]
# plotly_build(fake_example)$x$data[[2]]
# plotly_build(fake_example)$x$data[[3]]
