library(plotly)
library(magrittr)

one_pop_one_model |> 
  plot_roc_curve()

one_pop_one_model   |>
  create_rtichoke_curve_list("roc") |>
  create_plotly_curve()

one_pop_one_model   |>
  create_rtichoke_curve_list("interventions avoided") |>
  create_plotly_curve()


# bla <- multiple_models |>
#   create_rtichoke_curve_list("gains") #%>%
#   
# bla$reference_data


# TODO: Take care of invisible marker:
# copy pyhon the same way I do in R

# TODO: Take care of combined decision curves

multiple_populations |>
  create_rtichoke_curve_list("interventions avoided") |> 
  jsonlite::toJSON(auto_unbox = TRUE) |> 
  write("C:/Users/CRI_user/Documents/rtichoke_curve_json_array.json")

lift_list <- one_pop_one_model_by_ppcr |>
  create_rtichoke_curve_list("lift")

View(lift_list$performance_data_ready_for_curve)


lift_list$performance_data_ready_for_curve$y[is.nan(lift_list$performance_data_ready_for_curve$y)] <- -1 

lift_list$performance_data_ready_for_curve <- lift_list$performance_data_ready_for_curve |> 
  dplyr::filter(!is.nan(y))

lift_list |> 
  jsonlite::toJSON(auto_unbox = TRUE) |> 
  write("C:/Users/CRI_user/Documents/rtichoke_curve_json_array.json")


View(lift_list$reference_data)

plotly::subplot(
  multiple_populations |>
    create_rtichoke_curve_list("interventions avoided", 
                               min_p_threshold = 0.01, max_p_threshold = 0.99) |> 
    create_plotly_curve(),
  multiple_populations |>
    create_rtichoke_curve_list("decision", 
                               min_p_threshold = 0.01, max_p_threshold = 0.99) |> 
    create_plotly_curve(), 
  nrows = 2,
  shareX = TRUE,
  shareY = FALSE, heights = c(0.5, 0.5)
)



#####

json_obj <- plotly::plotly_json(decision_example)

library(dplyr)

stratified_by <- rtichoke:::check_performance_data_stratification(
  performance_data
)

prevalence_from_performance_data <- rtichoke:::get_prevalence_from_performance_data(performance_data)
perf_dat_type <- rtichoke:::check_performance_data_type_for_plotly(performance_data = performance_data)

reference_group_colors_vec <- performance_data |>
  extract_reference_groups_from_performance_data(perf_dat_type) |>
  create_reference_group_color_vector(perf_dat_type, col_values = c(
    "#1b9e77", "#d95f02",
    "#7570b3", "#e7298a",
    "#07004D", "#E6AB02",
    "#FE5F55", "#54494B",
    "#006E90", "#BC96E6",
    "#52050A", "#1F271B",
    "#BE7C4D", "#63768D",
    "#08A045", "#320A28",
    "#82FF9E", "#2176FF",
    "#D1603D", "#585123"
  ))

jsonlite::toJSON(as.list(reference_group_colors_vec), 
                 auto_unbox = TRUE) %>% 
  write("C:/Users/CRI_user/Documents/group_colors_vec.json")

create_reference_lines_data(
  "decision", prevalence_from_performance_data,
  perf_dat_type,
  min_p_threshold = 0,
  max_p_threshold = 0.99) |> 
  write.csv("C:/Users/CRI_user/Documents/reference_data.csv")

x_performance_metric <- "probability_threshold"
y_performance_metric <- "NB"

axes_labels$xaxis = "Probability Threshold"
axes_labels$yaxis = "Probability Threshold"

performance_data_ready_for_curve <- performance_data |>
  prepare_performance_data_for_curve(
    x_performance_metric,
    y_performance_metric,
    stratified_by,
    perf_dat_type, 
  )

axes_ranges <- extract_axes_ranges(performance_data_ready_for_curve, "decision",
                                   min_p_threshold = 0,
                                   max_p_threshold = 0.99)

axes_labels = list(xaxis = "Probability Threshold", yaxis = "Net Benefit")

list(
  reference_data = create_reference_lines_data(
    "decision", prevalence_from_performance_data,
    perf_dat_type,
    min_p_threshold = 0,
    max_p_threshold = 0.99),
  reference_group_colors = reference_group_colors_vec,
  # size = 500,
  performance_data_ready_for_curve = performance_data_ready_for_curve,
  perf_dat_type = perf_dat_type,
  axes_ranges = axes_ranges,
  axes_labels = axes_labels
) |> 
  create_plotly_curve()




jsonlite::toJSON(axis_ranges, keep_vec_names=TRUE, auto_unbox = TRUE) %>%
  write("C:/Users/CRI_user/Documents/axis_ranges.json")


size <- 350
size_height <- switch(is.null(size) +1, size + 50, NULL)





write.csv(performance_data_ready_for_curve, 
          "C:/Users/CRI_user/Documents/performance_data_ready_for_curve.csv")



reference_lines_data <- data.frame(
  reference_group = "reference_line",
  x = seq(0, 1, by = 0.01), 
  y = seq(0, 1, by = 0.01)) |> 
  dplyr::mutate(
    text = 
      glue::glue(
        "<b>Random Guess</b><br>Sensitivity: {y}<br>1 - Specificity: {x}")
  )

col_values = c(
  "#1b9e77", "#d95f02",
  "#7570b3", "#e7298a",
  "#07004D", "#E6AB02",
  "#FE5F55", "#54494B",
  "#006E90", "#BC96E6",
  "#52050A", "#1F271B",
  "#BE7C4D", "#63768D",
  "#08A045", "#320A28",
  "#82FF9E", "#2176FF",
  "#D1603D", "#585123"
)

reference_group_colors_vec <- performance_data |>
  extract_reference_groups_from_performance_data(perf_dat_type) |>
  create_reference_group_color_vector(perf_dat_type, col_values = col_values)

jsonlite::toJSON(as.list(reference_group_colors_vec), 
                 auto_unbox = TRUE) %>% 
  write("C:/Users/CRI_user/Documents/group_colors_vec.json")

# roc_example <- plot_rtichoke_curve(
#   one_pop_one_model, 
#   "roc")

# alternative

# write(plotly:::to_JSON(roc_example), "C:/Users/CRI_user/Documents/rtichoke_json.json")



library(plotly)

plotly_build_obj <- plotly_build(multiple_populations_by_ppcr     |>
                                   create_rtichoke_curve_list("precision recall") |> 
                                   create_plotly_curve())$x
plotly_build_obj$data[1]

plotly_build_obj$data[3]

names()
plotly_build_obj$layout$xaxis$ran


names(plotly_build_obj$layout$updatemenus[[1]]$buttons[[1]])

plotly_build_obj$layout$updatemenus[[1]]$buttons[[1]]$args

attr(plotly_build_obj$layout$updatemenus[[1]]$buttons[[1]]$args[[2]]$mode, "class") <- NULL
attr(plotly_build_obj$layout$updatemenus[[1]]$buttons[[1]]$args[[2]]$transition$duration, "class") <- NULL
attr(plotly_build_obj$layout$updatemenus[[1]]$buttons[[1]]$args[[2]]$transition$easing, "class") <- NULL
attr(plotly_build_obj$layout$updatemenus[[1]]$buttons[[1]]$args[[2]]$frame$duration, "class") <- NULL
attr(plotly_build_obj$layout$updatemenus[[1]]$buttons[[1]]$args[[2]]$frame$redraw, "class") <- NULL
attr(plotly_build_obj$layout$updatemenus[[1]], "class") <- NULL



# remove attributes from sliders

attr(plotly_build_obj$layout$sliders[[1]]$steps[[1]]$args[[2]]$transition$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[1]]$args[[2]]$transition$easing, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[1]]$args[[2]]$frame$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[1]]$args[[2]]$frame$redraw, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[1]]$args[[2]]$mode, "class") <- NULL

attr(plotly_build_obj$layout$sliders[[1]]$steps[[2]]$args[[2]]$transition$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[2]]$args[[2]]$transition$easing, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[2]]$args[[2]]$frame$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[2]]$args[[2]]$frame$redraw, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[2]]$args[[2]]$mode, "class") <- NULL


attr(plotly_build_obj$layout$sliders[[1]]$steps[[3]]$args[[2]]$transition$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[3]]$args[[2]]$transition$easing, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[3]]$args[[2]]$frame$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[3]]$args[[2]]$frame$redraw, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[3]]$args[[2]]$mode, "class") <- NULL


attr(plotly_build_obj$layout$sliders[[1]]$steps[[4]]$args[[2]]$transition$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[4]]$args[[2]]$transition$easing, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[4]]$args[[2]]$frame$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[4]]$args[[2]]$frame$redraw, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[4]]$args[[2]]$mode, "class") <- NULL


attr(plotly_build_obj$layout$sliders[[1]]$steps[[5]]$args[[2]]$transition$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[5]]$args[[2]]$transition$easing, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[5]]$args[[2]]$frame$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[5]]$args[[2]]$frame$redraw, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[5]]$args[[2]]$mode, "class") <- NULL


attr(plotly_build_obj$layout$sliders[[1]]$steps[[6]]$args[[2]]$transition$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[6]]$args[[2]]$transition$easing, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[6]]$args[[2]]$frame$duration, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[6]]$args[[2]]$frame$redraw, "class") <- NULL
attr(plotly_build_obj$layout$sliders[[1]]$steps[[6]]$args[[2]]$mode, "class") <- NULL

attr(plotly_build_obj$layout$sliders[[1]], "class") <- NULL


attr(plotly_build_obj$frames[[1]]$data[[1]]$x, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[1]]$data[[1]]$y, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[1]]$data[[1]]$textfont$color, "class") <- NULL
attr(plotly_build_obj$frames[[1]]$data[[1]]$error_y$color, "class") <- NULL
attr(plotly_build_obj$frames[[1]]$data[[1]]$error_x$line$color, "class") <- NULL
attr(plotly_build_obj$frames[[1]]$data[[1]]$error_x$color, "class") <- NULL
attr(plotly_build_obj$frames[[1]]$data[[1]]$line$color, "class") <- NULL


attr(plotly_build_obj$frames[[2]]$data[[1]]$x, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[2]]$data[[1]]$y, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[2]]$data[[1]]$textfont$color, "class") <- NULL
attr(plotly_build_obj$frames[[2]]$data[[1]]$error_y$color, "class") <- NULL
attr(plotly_build_obj$frames[[2]]$data[[1]]$error_x$line$color, "class") <- NULL
attr(plotly_build_obj$frames[[2]]$data[[1]]$error_x$color, "class") <- NULL
attr(plotly_build_obj$frames[[2]]$data[[1]]$leaine$color, "class") <- NULL


attr(plotly_build_obj$frames[[3]]$data[[1]]$x, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[3]]$data[[1]]$y, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[3]]$data[[1]]$textfont$color, "class") <- NULL
attr(plotly_build_obj$frames[[3]]$data[[1]]$error_y$color, "class") <- NULL
attr(plotly_build_obj$frames[[3]]$data[[1]]$error_x$line$color, "class") <- NULL
attr(plotly_build_obj$frames[[3]]$data[[1]]$error_x$color, "class") <- NULL
attr(plotly_build_obj$frames[[3]]$data[[1]]$line$color, "class") <- NULL


attr(plotly_build_obj$frames[[4]]$data[[1]]$x, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[4]]$data[[1]]$y, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[4]]$data[[1]]$textfont$color, "class") <- NULL
attr(plotly_build_obj$frames[[4]]$data[[1]]$error_y$color, "class") <- NULL
attr(plotly_build_obj$frames[[4]]$data[[1]]$error_x$line$color, "class") <- NULL
attr(plotly_build_obj$frames[[4]]$data[[1]]$error_x$color, "class") <- NULL
attr(plotly_build_obj$frames[[4]]$data[[1]]$line$color, "class") <- NULL


attr(plotly_build_obj$frames[[5]]$data[[1]]$x, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[5]]$data[[1]]$y, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[5]]$data[[1]]$textfont$color, "class") <- NULL
attr(plotly_build_obj$frames[[5]]$data[[1]]$error_y$color, "class") <- NULL
attr(plotly_build_obj$frames[[5]]$data[[1]]$error_x$line$color, "class") <- NULL
attr(plotly_build_obj$frames[[5]]$data[[1]]$error_x$color, "class") <- NULL
attr(plotly_build_obj$frames[[5]]$data[[1]]$line$color, "class") <- NULL


attr(plotly_build_obj$frames[[6]]$data[[1]]$x, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[6]]$data[[1]]$y, "apiSrc") <- NULL
attr(plotly_build_obj$frames[[6]]$data[[1]]$textfont$color, "class") <- NULL
attr(plotly_build_obj$frames[[6]]$data[[1]]$error_y$color, "class") <- NULL
attr(plotly_build_obj$frames[[6]]$data[[1]]$error_x$line$color, "class") <- NULL
attr(plotly_build_obj$frames[[6]]$data[[1]]$error_x$color, "class") <- NULL
attr(plotly_build_obj$frames[[6]]$data[[1]]$line$color, "class") <- NULL


length(plotly_build_obj$layout$sliders[[1]]$steps[[1]]$args)


length(plotly_build_obj$data)

plotly_build_obj$data[[1]]$name
plotly_build_obj$data[[2]]$name
names(plotly_build_obj$data[[3]])

length(plotly_build_obj$layout$sliders[[1]])
length(plotly_build_obj$layout$sliders[[1]])
names(plotly_build_obj$layout$sliders[[1]])

plotly_build_obj$layout$sliders[[1]]$currentvalue
plotly_build_obj$layout$sliders[[1]]$steps
plotly_build_obj$layout$sliders[[1]]$visible
plotly_build_obj$layout$sliders[[1]]$pad



length(plotly_build_obj$layout$sliders[[1]]$steps)
names(plotly_build_obj$layout$sliders[[1]]$steps[[1]])
plotly_build_obj$layout$sliders[[1]]$steps[[1]]$label
plotly_build_obj$layout$sliders[[1]]$steps[[1]]$value
plotly_build_obj$layout$sliders[[1]]$steps[[1]]$method

plotly_build_obj$layout$sliders[[1]]$steps[[2]]$args
plotly_build_obj$layout$sliders[[1]]$steps[[3]]$args
plotly_build_obj$layout$sliders[[1]]$steps[[4]]$args
plotly_build_obj$layout$sliders[[1]]$steps[[5]]$args
plotly_build_obj$layout$sliders[[1]]$steps[[6]]$args


length(plotly_build_obj$frames)

names(plotly_build_obj$data[[1]])
plotly_build_obj$data[[2]]
plotly_build_obj$data[[3]]
       

length(plotly_build_obj$frames)
names(plotly_build_obj$frames[[1]])

plotly_build_obj$frames[[1]]$traces

names(plotly_build_obj$frames[[1]])

plotly_build_obj$frames[[1]]$data
plotly_build_obj$frames[[2]]$traces
plotly_build_obj$frames[[3]]$traces
plotly_build_obj$frames[[4]]$traces
plotly_build_obj$frames[[5]]$traces
plotly_build_obj$frames[[6]]$traces
plotly_build_obj$frames[[6]]$traces

length(plotly_build_obj$frames[[1]]$data)

plotly_build_obj$frames[[1]]$data[[1]]
plotly_build_obj$frames[[1]]$data

names(plotly_build_obj$layout)
plotly_build_obj$layout$margin
plotly_build_obj$layout$xaxis
plotly_build_obj$layout$yaxis
plotly_build_obj$layout$showlegend
plotly_build_obj$layout$hovermode
plotly_build_obj$layout$sliders
plotly_build_obj$layout$updatemenus

length(plotly_build_obj$layout$sliders)

plotly_build_obj$layout$t
plotly_build_obj$frames[[2]]$name
plotly_build_obj$frames[[3]]$name

names(plotly_build_obj$frames[[2]])
names(plotly_build_obj$frames[[3]])
names(plotly_build_obj$frames[[4]])
names(plotly_build_obj$frames[[5]])
names(plotly_build_obj$frames[[6]])


plotly_build_obj$layout$sliders[[1]]$steps$

length(plotly_build_obj$layout$sliders[[1]]$steps)


plotly_build_obj$layout$sliders[[1]]$steps[[1]]

plotly_build_obj$layout$sliders[[1]]$currentvalue

names(plotly_build_obj$layout$sliders[[1]])

plotly_build_obj$layout$sliders[[1]]$currentvalue

plotly_build_obj$frames[[3]]$data

plotly_build_obj$frames[[3]]$frame


plotly_build_obj$layout$sliders[[1]]

length(plotly_build_obj$frames)

length(plotly_build_obj$frames[[2]])

names(plotly_build_obj$data[[1]])

list(
data = plotly_build_obj$data |> 
  purrr::map(magrittr::extract, c("type", "mode", "x", "y", "hoverinfo", "text", "textfont",
                                  "marker", "error_y",   "error_x", "line", "xaxis", "yaxis",
                                  "name", "frame"
                                  )),
layout = magrittr::extract(plotly_build_obj$layout, c("margin", "xaxis", "yaxis", 
                                                      "showlegend", "hovermode",
                                                      "updatemenus", "sliders")),
frames = plotly_build_obj$frames
)|> 
  jsonlite::toJSON(flatten = TRUE, auto_unbox = TRUE, na = 'null')  |>  
  write("C:/Users/CRI_user/Documents/rtichoke_json.json")

dput(names(plotly_build_obj$layout))

dput(names(plotly_build_obj$layout$updatemenus[[1]]))

plotly_build_obj$layout$updatemenus[[1]][c("type", "direction", "showactive", "y", "x", "yanchor", "xanchor", 
                                           "pad", "buttons")]

names(plotly_build_obj$layout$updatemenus[[1]]$buttons[[1]])


attr(
  plotly_build_obj$layout$updatemenus[[1]]$buttons[[1]]$args[[2]]$mode, 
  "class")



labelled::remove_attributes(plotly_build_obj$layout$updatemenus[[1]][c("buttons")])

?labelled::remove_attributes
?attr

attr(MyData$VAR, "ATT") <- NULLnames(plotly_build_obj$layout)


length(plotly_build_obj$layout$sliders[[1]])

plotly_build_obj$layout$updatemenus[[1]]

names(plotly_build_obj$layout$sliders[[1]])

plotly_build_obj$frames[[1]]$data

names(plotly_build_obj$frames[[1]])
length(plotly_build_obj$frames[[1]])



dput(names(plotly_build_obj$layout))

length(plotly_build_obj$layout$sliders)


?magrittr::extract
magrittr::extract(plotly_build_obj$layout, c("margin", "xaxis", "yaxis", "showlegend", "hovermode", 
                                             "updatemenus"))

c(1, 2, 3) |> 
  purrr::map(~function(x)(plotly_build_obj$data.[[x]]))
plotly_build_obj$data |> 
  purrr::map(~function(x){ x})
  [[2]][
  c("type", "mode", "x", "y", "hoverinfo", "text", "textfont", 
    "marker", "error_y",   "error_x", "line", "xaxis", "yaxis")]
