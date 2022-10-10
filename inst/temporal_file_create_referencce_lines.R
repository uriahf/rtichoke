# Check Inputs

check_curve_input <- function(curve) {
  rlang::arg_match(curve, c("roc", "lift", "precision recall", "gains",
                     "decision", "interventions avoided"))
}

check_prevalence_input <- function(prevalence) {
  stopifnot(
    is.na(prevalence) |
      is.numeric(prevalence) &
      all(prevalence >= 0) &
      all(prevalence <= 1) ) &
    is.null(names(prevalence))
}

create_reference_lines_data <- function(curve, prevalence){
  
  check_curve_input(curve)
  
  if (curve == "roc") {
    
    reference_lines_data <- data.frame(
      reference_group = "reference_line",
      x = c(0, 1), 
      y = c(0, 1)) |> 
      dplyr::mutate(
        text = 
          glue::glue(
            "<b>Random Guess</b><br>Sensitivity: {y}<br>1 - Specificity: {x}")
      )
      
  }
  
  if (curve == "lift") {
    
    reference_lines_data <- data.frame(
      reference_group = "reference_line",
      x = c(0, 1), 
      y = c(1, 1)) |> 
      dplyr::mutate(
        text = 
          glue::glue(
            "<b>Random Guess</b><br>Lift: {y}")
      )
    
  }
  
  if (curve == "precision recall") {
    
    reference_lines_data <- data.frame(
      reference_group = rep(names(prevalence), each = 2),
      x = c(0, 1),
      y = rep(prevalence, each = 2)
    ) |> 
      dplyr::mutate(
        text = ifelse(length(prevalence == 1),
          glue::glue(
            "<b>Random Guess</b><br>PPV: \\
            {round(y, digits = 3)}"),
          glue::glue(
            "<b>Random Guess ({reference_group})</b><br>PPV: \\
            {round(y, digits = 3)}")
        )
      )
    
  }
  
  if (curve == "gains") {
    
    reference_lines_data <- data.frame(
      reference_group = names(prevalence),
      x = prevalence,
      y = 1,
      row.names = NULL
    ) |>
      dplyr::bind_rows(
        data.frame(
          reference_group = rep(names(prevalence), each = 2),
          x = c(0, 1),
          y = c(0, 1)
        )
      ) |>
      dplyr::arrange(reference_group, x, y) %>%
      dplyr::bind_rows(
        data.frame(reference_group = "reference_line", 
                   x = c(0, 1), 
                   y = c(0, 1))
      ) |> 
      dplyr::mutate(
        text = dplyr::case_when(
          reference_group != "reference_line" ~ glue::glue(
            "<b>Perfect Prediction</b><br>\\
            Sensitivity: {round(y, digits = 3)}<br>\\
            Predicted Positives: {round(x*100)}%" 
          ),
          TRUE ~ "<b>Random Guess"
        )
      )
    
  }
  
  if (curve == "decision") {

    
    glue_text <- ifelse(
      length(prevalence == 1),
      "<b>Treat All</b><br>\\
      NB: {round(y, digits = 3)}<br>\\
      Probability Threshold: {x}",
      "<b>Treat All ({reference_group})</b><br>\\
      NB: {round(y, digits = 3)}<br>\\
      Probability Threshold: {x}"
    )
    
    
    reference_lines_data <- data.frame(
      prevalence_for_dat = rep(prevalence, each = 99),
      reference_group = rep(names(prevalence), each = 99),
      x = rep(seq(0.01, 0.99, by = 0.01), length(prevalence)),
      row.names = NULL
    ) |>
      dplyr::mutate(
        y = prevalence_for_dat - (1 - prevalence_for_dat) * 
          (x / (1 - x)),
        text = glue::glue(glue_text)) |> 
      dplyr::select(-prevalence_for_dat) |> 
      dplyr::bind_rows(
        data.frame(
          reference_group = "treat_none", 
          x = c(0, 1), 
          y = c(0, 0),
          text = "<b>Treat None</b><br>\\
                  NB: 0")
      )
      
  }
  
  
  reference_lines_data
  
}

create_reference_lines_for_plotly_new <- function(curve,
                                                  prevalence,
                                                  reference_group_colors) {
  
  plot_ly(
    x = ~x,
    y = ~y,
    hoverinfo = "text",
    text = ~text,
    color = ~reference_group,
    colors = reference_group_colors
  )|>
    add_lines(
      data = create_reference_lines_data(
        curve, prevalence),
      line = list(
        dash = "dot"
      )
    )
  
}


plot_ly(
  x =~ x,
  y =~ y,
  hoverinfo = "text",
  text =~ text,
  color =~ reference_group,
  colors = reference_group_colors
) |>
  add_lines(
    data = create_reference_lines_data(
      "roc", prevalence_example),
    line = list(
      dash = "dot"
    )
  )




library(dplyr)

# TODO: Fix ifelse prevalence length statements for text 
# TODO: add insightfull text for referrence lines
# TODO: reference lines should include variety of values, not only the edges (like in decision curve)
# TODO: decision curve and interventions avoided
# TODO: performance_data should be ready with x, y, frame_column, text and reference_group
# TODO: Fix get_prevalence_from_performance_data: remove performance_data_type argument

performance_dat_example <- prepare_performance_data(
  probs = list(
    "train" = example_dat %>%
      dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(estimated_probabilities),
    "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(estimated_probabilities)
  ),
  reals = list(
    "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
      dplyr::pull(outcome),
    "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
      dplyr::pull(outcome)
  )
)


create_reference_group_color_vector <- function(reference_groups, 
                                                 col_values = c("#1b9e77", "#d95f02", 
                                                                "#7570b3", "#e7298a", 
                                                                "#07004D", "#E6AB02", 
                                                                "#FE5F55", "#54494B", 
                                                                "#006E90" , "#BC96E6",
                                                                "#52050A", "#1F271B", 
                                                                "#BE7C4D", "#63768D", 
                                                                "#08A045", "#320A28", 
                                                                "#82FF9E", "#2176FF", 
                                                                "#D1603D", "#585123")){
  
  reference_group_color_vector <- c("grey", col_values[1:length(reference_groups)])
  names(reference_group_color_vector) <- c("reference_line", reference_groups)
  
  reference_group_color_vector
  
}

extract_reference_groups_from_performance_data <- function(performance_data){
  
  if ("model" %in% names(performance_data)) {
    reference_groups <- unique(performance_data$model)
  } else if ("population" %in% names(performance_data)) {
    reference_groups <- unique(performance_data$population)
  } else {
    reference_groups <- "model"
  }
  
  reference_groups
  
}

extract_reference_groups_from_performance_data(performance_dat_example)

prevalence_example <- rtichoke:::get_prevalence_from_performance_data(performance_dat_example)

# TODO: write a function to extract reference groups from prevalence_example



reference_group_colors <- create_reference_group_color_vecctor(
  extract_reference_groups_from_performance_data(performance_dat_example))



library(plotly)
library(rtichoke)


select_and_rename_necessary_variables <- function(performance_data, x, y, main_slider){
  
  performance_data |> 
    dplyr::select(population, x, y, main_slider, text) |> 
    purrr::set_names(c("reference_group", "x", "y", "main_slider", "text"))
  
}


# TODO: tidyeval solution for decision curves



create_hover_text <- function(stratified_by = "probability_threshold", 
                              interventions_avoided = FALSE) {
  
  if (interventions_avoided == FALSE) {
    
    text_for_hover <- paste0(
      "Prob. Threshold: {probability_threshold}
Sensitivity: {sensitivity}
1 - Specificity (FPR): {FPR}
Specificity: {specificity}
Lift: {lift}
PPV: {PPV}
NPV: {NPV}\n",
      ifelse ( stratified_by == "probability_threshold", 
               "NB: {NB}\n", 
               "" ),
      "Predicted Positives: {predicted_positives} ({100 * ppcr}%)
TP: {TP}
TN: {TN}
FP: {FP}
FN: {FN}"
    ) 
    
  } else {
    
    text_for_hover <- "Prob. Threshold: {probability_threshold}
Interventions Avoided (per 100): {NB_treatment_avoided}
NB: {NB}
Predicted Positives: {predicted_positives} ({100 * ppcr}%)
TN: {TN}
FN: {FN}"
    
  }
  
  text_for_hover
  
}


add_hover_text_to_performance_data_new <- function(
    performance_data,
    performance_metric_x,
    performance_metric_y,
    stratified_by = "probability_threshold"){

  hover_text <- create_hover_text(
    stratified_by = stratified_by,
    interventions_avoided = (performance_metric_x == "interventions_avoided")) |> 
    rtichoke:::make_two_performance_metrics_bold(
      performance_metric_x, 
      performance_metric_y) %>% 
    {
      if (names(performance_data)[1] == "model") {
        add_models_for_text_for_hover(.)
      } else {
        .
      }
    } %>% 
    {
      if (names(performance_data)[1] == "population") {
        rtichoke:::add_population_for_text_for_hover(.)
      } else {
        .
      }
    }
  
  performance_data %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), round, 3),
      text = glue::glue(hover_text),
      text = stringr::str_replace_all(text, pattern = "NaN", "")
    )
    
}


prepare_performance_data_for_curve_new <- function(
    performance_data,
    x_performance_metric,
    y_performance_metric,
    main_slider = "probability_threshold"){
  
  performance_data |> 
    add_hover_text_to_performance_data_new(x_performance_metric, y_performance_metric)|> 
    select_and_rename_necessary_variables(
      x_performance_metric, 
      y_performance_metric, 
      main_slider)
  
}

add_markers_and_lines_for_plotly_reference_object <- function(plotly_object, performance_data_ready){
  
  plotly_object |> 
    add_trace(
      data = performance_data_ready,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(dash = 'solid')
    ) |> 
    add_markers(
      data = performance_data_ready,
      frame =~ main_slider,
      marker = list(
        size = 12,
        line = list(
          width = 3,
          color = I("black")
        )
      )
    )
  
}


library(plotly)

performance_data_ready <- performance_dat_example |> 
  prepare_performance_data_for_curve_new("FPR", "sensitivity")

reference_group_colors_vec <- performance_dat_example |> 
  extract_reference_groups_from_performance_data() |> 
  create_reference_group_color_vector()


# ROC


create_reference_lines_for_plotly_new(
  "roc", 
  prevalence_example,
  reference_group_colors = reference_group_colors_vec) |> 
  add_markers_and_lines_for_plotly_reference_object(performance_data_ready)

# Lift

performance_data_ready <- performance_dat_example |> 
  prepare_performance_data_for_curve_new("ppcr", "lift")

create_reference_lines_for_plotly_new(
  "lift", 
  prevalence_example,
  reference_group_colors = reference_group_colors_vec) |> 
  add_markers_and_lines_for_plotly_reference_object(
    performance_data_ready)

# TODO: sort fake marker for lift and precision recall curves

  
# Precision Recall


performance_data_ready <- performance_dat_example |> 
  prepare_performance_data_for_curve_new("sensitivity", "PPV")


create_reference_lines_for_plotly_new(
  "precision recall", 
  prevalence_example,
  reference_group_colors = reference_group_colors_vec) |> 
  add_markers_and_lines_for_plotly_reference_object(
    performance_data_ready)


# Decision


performance_data_ready <- performance_dat_example |> 
  prepare_performance_data_for_curve_new("probability_threshold", "NB")



performance_dat_example |> 
  add_hover_text_to_performance_data_new("probability_threshold", "NB") |> 
  select_and_rename_necessary_variables(
    "probability_threshold", "NB", "probability_threshold")


plot_ly(
  x =~ x,
  y =~ y,
  hoverinfo = "text",
  text =~ text,
  color =~ reference_group,
  colors = reference_group_colors
) |>
  add_lines(
    data = create_reference_lines_data(
      "lift", prevalence_example),
    line = list(
      dash = "dot"
    )
  ) |> 
  add_markers_and_lines_for_plotly_reference_object(performance_data_ready)




  prepare_performance_data_for_curve("sensitivity", 
                                     "FPR", 
                                     "probability_threshold") 

  performance_dat_example |> 
    rtichoke:::add_hover_text_to_performance_data(
      performance_data_type = "several populations",
      curve = "gains")  |> 
    dplyr::select(population, x, y, main_slider)


library(purrr)

c("roc", "lift", "precision recall", "gains") |> 
  map(~plot_curve(.x, prevalence_example, reference_group_colors)) |>
  add_trace(
    data = performance_dat_example %>% 
      mutate(reference_group = population) %>% 
      rtichoke:::add_hover_text_to_performance_data(
        performance_data_type = "several populations",
        curve = "gains"),
    x =~ ppcr,
    y =~ sensitivity,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(dash = 'solid', width = 3)
  )



# TODO: create internal function `create_reference_group_colors_vector()`

library(plotly)

plot_ly(
  hoverinfo = "text",
  text =~ text,
  color =~ reference_group,
  colors = reference_group_colors,
) |>
  add_lines(
    data = create_reference_lines_data("roc", prevalence_example),
    x =~ x,
    y =~ y,
    line = list(
      dash = "dot"
    )
  ) |>
  add_trace(
    data = performance_dat_example %>% 
      mutate(reference_group = population) %>% 
      rtichoke:::add_hover_text_to_performance_data(
        performance_data_type = "several populations",
        curve = "gains"),
    x =~ ppcr,
    y =~ sensitivity,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(dash = 'solid', width = 3)
  ) %>%
  add_markers(
    data = performance_dat_example %>% 
      mutate(reference_group = population) %>% 
      rtichoke:::add_hover_text_to_performance_data(
        performance_data_type = "several populations",
        curve = "gains"),
    x =~ ppcr,
    y =~ sensitivity,
    frame =~ probability_threshold,
    marker = list(size = 14)
  )
    

data.frame(
  reference_group = rep(names(prevalence_example), each = 2),
  x = c(0, 1),
  y = rep(prevalence_example, each = 2),
  text = "Random Guess"
)


create_reference_lines <- function(curve){
  
  check_curve_input(curve)
  
}




# Examples

!is.null(names(prevalence))

check_prevalence_input(c(1.3, 0.4, 0.2))
check_prevalence_input(NA)

prevalence <- c(0.3, 0.4, 0.2)
is.numeric(prevalence) & all(prevalence) >= 0 & 4 5< 
