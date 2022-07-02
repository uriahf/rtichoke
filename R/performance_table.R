# Performance Table ----------------------------------------------------------



#' Performance Table
#'
#' Create a Performance Table
#'
#' @inheritParams create_roc_curve
#' @param output_type the type of the output table
#'
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' 
#' create_performance_table(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#' 
#' create_performance_table(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#' create_performance_table(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#'
#' create_performance_table(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome),
#'   stratified_by = "ppcr"
#' )
#'
#'
#' create_performance_table(
#'   probs = list(
#'     "train" = example_dat %>%
#'       dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   reals = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   )
#' )
#' 
#' create_performance_table(
#'   probs = list(
#'     "train" = example_dat %>%
#'       dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(estimated_probabilities),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(estimated_probabilities)
#'   ),
#'   reals = list(
#'     "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
#'       dplyr::pull(outcome),
#'     "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
#'       dplyr::pull(outcome)
#'   ),
#'   stratified_by = "ppcr"
#' )
#'
#' }
create_performance_table <- function(probs, reals, by = 0.01,
                                     stratified_by = "probability_threshold",
                                     output_type = "reactable") {
  prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  ) %>%
    render_performance_table(output_type = output_type)
}


#' Performance Table
#'
#' Create a Performance Table
#'
#' @inheritParams plot_roc_curve
#' @inheritParams create_performance_table
#'
#' @examples
#' \dontrun{
#'
#' one_pop_one_model %>%
#'   render_performance_table()
#'
#' one_pop_one_model_by_ppcr %>%
#'   render_performance_table()
#'
#' multiple_models %>%
#'   render_performance_table()
#'
#' multiple_models_by_ppcr %>%
#'   render_performance_table()
#'
#' multiple_populations %>%
#'   render_performance_table()
#'
#' multiple_populations_by_ppcr %>%
#'   render_performance_table()
#'
#' }
#'
#' @export
render_performance_table <- function(performance_data,
                                     chosen_threshold = NA,
                                     output_type = "reactable",
                                     col_values = c(
                                       "#5BC0BE",
                                       "#FC8D62",
                                       "#8DA0CB",
                                       "#E78AC3",
                                       "#A4243B"
                                     )) {
  
  
  stratified_by <- check_performance_data_stratification(
    performance_data
  )
  
  perf_dat_type <- check_performance_data_type_for_plotly(
    performance_data = performance_data
  )
  
  prevalence <- get_prevalence_from_performance_data(
    performance_data, perf_dat_type
  )

  if (output_type == "gt") {
    performance_data %>%
      prepare_performance_data_for_gt(main_slider) %>%
      render_and_format_gt(main_slider, perf_dat_type, prevalence, col_values)
  }

  if (output_type == "reactable") {
    performance_data_reactable <- performance_data %>%
      dplyr::select(any_of(
        c(
          "probability_threshold", "model", "population", "sensitivity", "specificity",
          "PPV", "NPV", "lift", "predicted_positives", "NB", "ppcr"
        )
      )) %>%
      dplyr::rename(any_of(c(
        "Model" = "model",
        "Population" = "population",
        "Threshold" = "probability_threshold"
      )))


    if (stratified_by != "probability_threshold") {
      performance_data_reactable <- performance_data_reactable %>%
        dplyr::relocate(predicted_positives,
          ppcr,
          .after = Threshold
        ) %>%
        dplyr::arrange(ppcr) %>%
        dplyr::select(-Threshold) %>%
        mutate(rank = dplyr::dense_rank(ppcr))
    } else {
      performance_data_reactable <- performance_data_reactable %>%
        dplyr::arrange(Threshold) %>%
        mutate(rank = dplyr::dense_rank(Threshold))
    }


    if ("Model" %in% names(performance_data_reactable)) {
      performance_data_reactable <- performance_data_reactable %>%
        dplyr::mutate(
          Model = forcats::fct_inorder(
            factor(Model)
          ),
          key_values =
            forcats::fct_inorder(
              factor(Model)
            ),
          key_values =
            factor(key_values,
              labels =
                c("1", "2", "3", "4", "5")[
                  seq_len(length(unique(performance_data_reactable %>%
                    dplyr::pull(Model))))
                ]
            )
        )
    }

    if ("Population" %in% names(performance_data_reactable)) {
      performance_data_reactable <- performance_data_reactable %>%
        dplyr::mutate(
          Population = forcats::fct_inorder(
            factor(Population)
          ),
          key_values =
            forcats::fct_inorder(
              factor(Population)
            ),
          key_values =
            factor(key_values,
              labels = c("1", "2", "3", "4", "5")[
                seq_len(
                  length(unique(performance_data_reactable %>%
                    dplyr::pull(Population)))
                )
              ]
            )
        )
    }

    confusion_matrix_list <- performance_data %>%
      create_conf_mat_list(stratified_by = stratified_by)

    interactive_data <- SharedData$new(performance_data_reactable)

    status_badge <- function(color = "#aaa", width = "9px", height = width) {
      span(style = list(
        display = "inline-block",
        marginRight = "8px",
        width = width,
        height = height,
        backgroundColor = color,
        borderRadius = "50%"
      ))
    }

    interactive_data_reactable <- interactive_data %>%
      reactable::reactable(
        showSortIcon = FALSE,
        borderless = FALSE,
        defaultColDef = reactable::colDef(
          align = "left"
        ),
        columns = list(
          rank = reactable::colDef(show = FALSE),
          Threshold = reactable::colDef(
            name = "Probability Threshold",
            style = reactable::JS("function(rowInfo, colInfo, state) {
        const firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by school
        if (!firstSorted || firstSorted.id === 'Threshold') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.row['Threshold'] === prevRow['Threshold']) {
            return { visibility: 'hidden' }
          }
        }
      }")
          ),
          sensitivity = reactable::colDef(
            name = "Sens", style = function(value) {
              bar_style_perf(width = value)
            },
            format = reactable::colFormat(digits = 2)
          ),
          specificity = reactable::colDef(
            name = "Spec", style = function(value) {
              bar_style_perf(width = value)
            }, format = reactable::colFormat(digits = 2)
          ),
          PPV = reactable::colDef(
            name = "PPV", style = function(value) {
              bar_style_perf(width = value)
            }, format = reactable::colFormat(digits = 2)
          ),
          NPV = reactable::colDef(
            name = "NPV", style = function(value) {
              bar_style_perf(width = value)
            }, format = reactable::colFormat(digits = 2)
          ),
          lift = reactable::colDef(
            name = "Lift", style = function(value) {
              bar_style_perf(width = value /
                max(performance_data_reactable$lift,
                  na.rm = TRUE
                ))
            }, format = reactable::colFormat(digits = 2)
          ),
          NB = reactable::colDef(
            name = "Net Benefit",
            format = reactable::colFormat(digits = 2),
            style = function(value) {
              bar_style_nb_reactable(width = value /
                max(abs(performance_data_reactable$NB),
                  na.rm = TRUE
                ))
            }
          ),
          ppcr = reactable::colDef(
            name = "Predicted Positives",
            cell = function(value, index) {
              predicted_positives <-
                performance_data_reactable$predicted_positives[index]
              glue::glue("{predicted_positives} \\
                         ({round(value, digits = 2) * 100}%) ")
            },
            style = function(value) {
              bar_style_perf(width = value, color = "lightgrey")
            }
          ),
          predicted_positives = reactable::colDef(
            show = FALSE
          ),
          Population = reactable::colDef(
            show = TRUE,
            cell = function(value, index) {
              n_levels <- length(levels(value))

              key_num <- index %% n_levels
              if (key_num == 0) {
                key_num <- n_levels
              }
              key_num <- as.character(key_num)

              color <- switch(as.character(key_num),
                "1" = "#5BC0BE",
                "2" = "#FC8D62",
                "3" = "#8DA0CB",
                "4" = "#E78AC3",
                "5" = "#A4243B"
              )

              badge <- status_badge(color = color)
              tagList(badge, value)
            }
          ),
          Model = reactable::colDef(
            show = TRUE,
            cell = function(value, index) {
              n_levels <- length(levels(value))

              key_num <- index %% n_levels
              if (key_num == 0) {
                key_num <- n_levels
              }
              key_num <- as.character(key_num)

              color <- switch(as.character(key_num),
                "1" = "#5BC0BE",
                "2" = "#FC8D62",
                "3" = "#8DA0CB",
                "4" = "#E78AC3",
                "5" = "#A4243B"
              )

              badge <- status_badge(color = color)
              tagList(badge, value)
            }
          ),
          key_values = reactable::colDef(
            show = FALSE
          )
        ),
        columnGroups = list(
          reactable::colGroup(
            name = "Performance Metrics",
            columns = ( if(
              stratified_by == "probability_threshold"
            ) c(
              "sensitivity",
              "specificity",
              "PPV", "NPV",
              "lift", "NB"
            ) else c(
              "sensitivity",
              "specificity",
              "PPV", "NPV",
              "lift"
            ) )
          )
        ),
        details = function(index) {
          htmltools::div(
            style = "padding: 16px",
            confusion_matrix_list %>%
              .[[index]]
          )
        }
      )

    if (stratified_by != "probability_threshold") {
      slider_filter_strata <- as.formula(
        paste0("~", "ppcr")
      )

      slider_label <- "Predicted Positives Condition Rate (PPCR)"
    } else {
      slider_filter_strata <- as.formula(
        paste0("~", "Threshold")
      )

      slider_label <- "Probability Threshold"
    }


    if (perf_dat_type %in%
      c("one model", "one model with model column")) {
      crosstalk::bscols(
        widths = c(6, 12),
        crosstalk::filter_slider(
          "Propability Threshold",
          slider_label,
          interactive_data,
          slider_filter_strata
        ),
        interactive_data_reactable
      )
    } else {
      if (perf_dat_type == "several models") {
        main_label <- "Model"
      } else {
        main_label <- "Population"
      }


      crosstalk::bscols(
        widths = c(12, 6, 12),
        filter_checkbox_rtichoke("population",
          main_label,
          interactive_data,
          ~key_values,
          inline = TRUE,
          labels_values = unique(performance_data_reactable %>%
            pull(main_label))
        ),
        crosstalk::filter_slider(
          "Propability Threshold",
          slider_label,
          interactive_data,
          slider_filter_strata
        ),
        interactive_data_reactable
      )
    }
  }
}



#' Preparing Performance Data for gt
#'
#' @keywords internal
#' @inheritParams plot_roc_curve
prepare_performance_data_for_gt <- function(performance_data,
                                            main_slider) {
  performance_data_ready_for_gt <- performance_data %>%
    replace_nan_with_na() %>%
    dplyr::rename(any_of(c(
      "Model" = "model",
      "Population" = "population",
      "Threshold" = "probability_threshold"
    ))) %>%
    add_colors_to_performance_dat()



  if (stratified_by != "probability_threshold") {
    performance_data_ready_for_gt <- performance_data_ready_for_gt %>%
      dplyr::relocate(plot_predicted_positives,
        .after = Threshold
      ) %>%
      dplyr::arrange(ppcr) %>%
      dplyr::select(-Threshold) %>%
      mutate(rank = dplyr::dense_rank(ppcr))
  } else {
    performance_data_ready_for_gt <- performance_data_ready_for_gt %>%
      dplyr::arrange(Threshold) %>%
      mutate(rank = dplyr::dense_rank(Threshold))
  }

  performance_data_ready_for_gt %>%
    dplyr::select(-c(
      ppcr,
      predicted_positives,
      display_predicted_postivies,
      FPR
    ))
}


#' Rendering and Formatting gt
#'
#' @keywords internal
#' @inheritParams plot_roc_curve
#' @param prevalence the prevalence of the populations

render_and_format_gt <- function(performance_data,
                                 main_slider,
                                 perf_dat_type,
                                 prevalence,
                                 col_values) {
  performance_data %>%
    gt::gt() %>%
    gt::cols_hide(rank) %>%
    gt::fmt_missing(
      columns = dplyr::everything(),
      rows = dplyr::everything(),
      missing_text = ""
    ) %>%
    gt::cols_align(
      align = "left",
      columns = dplyr::everything()
    ) %>%
    gt::cols_align(
      align = "center",
      columns = NB
    ) %>%
    gt::cols_width(
      c(
        TP, TN, FP, FN,
        sensitivity, lift, specificity, PPV, NPV, NB,
        plot_predicted_positives
      ) ~ px(100)
    ) %>%
    gt::tab_spanner(
      label = "Confusion Matrix",
      columns = c(
        TP, FP, TN, FN,
        sensitivity, lift, specificity, PPV, NPV, NB
      )
    ) %>%
    gt::tab_spanner(
      label = "Performance Metrics",
      columns = c(
        sensitivity, lift, specificity,
        PPV, NPV, NB
      )
    ) %>%
    gt::cols_label(
      sensitivity = "Sens",
      lift = "Lift",
      specificity = "Spec",
      plot_predicted_positives = "Predicted Positives"
    ) %>%
    # gt::tab_options(
    #   table.background.color = "#FFFBF3"
    # ) %>%
    # gt::tab_header(
    #   title = gt::md(creating_title_for_gt(main_slider)),
    #   subtitle = gt::md(creating_subtitle_for_gt(perf_dat_type,
    #                                       prevalence = prevalence,
    #                                       col_values = col_values))
    # ) %>%
    add_zebra_colors_to_gt_table(perf_dat_type %in% c(
      "several models",
      "several populations"
    ))
}


#' Add Zebra colors to gt table
#'
#' @param performance_table gt performance table
#' @param add_zebra_colors add zebra colors or keep table as it is (FALSE)
#'
#' @keywords internal
add_zebra_colors_to_gt_table <- function(performance_table,
                                         add_zebra_colors) {
  if (add_zebra_colors == TRUE) {
    performance_table %>%
      gt::tab_style(
        style = gt::cell_fill(color = "#f5f1f1"),
        locations = gt::cells_body(
          rows = rank %% 2 == 0
        )
      )
  } else {
    performance_table
  }
}

#' Creating Title for gt performance table
#'
#' @keywords internal
#' @inheritParams plot_roc_curve
creating_title_for_gt <- function(main_slider) {
  if (main_slider == "probability_threshold") {
    gt::md("**Performanc Metrics for Different Thresholds**")
  } else {
    gt::md("**Performanc Metrics by Predicted Positives Rate**")
  }
}



#' Creating Subtitle for gt performance table
#'
#' @keywords internal
#' @inheritParams plot_roc_curve
#' @param models_names the names of the different models
creating_subtitle_for_gt <- function(perf_dat_type,
                                     col_values = NA,
                                     prevalence = NA) {
  if (perf_dat_type == "one model") {
    subtitle_for_gt <- glue::glue("Prevalence: {round(prevalence, digits = 2)}")
  }

  if (perf_dat_type == "one model with model column") {
    subtitle_for_gt <- glue::glue(
      "**{names(prevalence)}** model \\
      (Prevalence: {round(prevalence, digits = 2)})"
    )
  }

  if (perf_dat_type == "several models") {
    col_values <- col_values[seq_len(length(prevalence))]

    subtitle_for_gt <- prevalence %>%
      names() %>%
      purrr::map2(col_values, add_html_color_to_model_for_subtitle) %>%
      glue::glue_collapse(", ", last = " and ") %>%
      glue::glue(" (Prevalnce: {round(prevalence[1], digits = 2)})")
  }

  if (perf_dat_type == "several populations") {
    col_values <- col_values[seq_len(length(prevalence))]

    subtitle_for_gt <- purrr::pmap(
      list(
        names(prevalence),
        col_values,
        prevalence
      ),
      create_subtitle_for_one_population
    ) %>%
      glue::glue_collapse(", ", last = " and ")
  }


  subtitle_for_gt
}





#' Creating Subtitle for One Population
#'
#' @param pop_name the name of the population
#' @param pop_color the color of the population
#' @param pop_prevalence the prevalence of the population
#'
#' @keywords internal
create_subtitle_for_one_population <- function(pop_name,
                                               pop_color,
                                               pop_prevalence) {
  glue::glue("<b><span style=\"color: \\
             {pop_color};\">{pop_name}</span></b> \\
             population (Prevalence: \\
             {round(pop_prevalence, digits = 2)})")
}


#' Add html color to model for subtitle
#'
#' @param model_name the name of the model
#' @param model_color the color of the model
#'
#' @keywords internal
add_html_color_to_model_for_subtitle <- function(model_name,
                                                 model_color) {
  glue::glue("<b><span style=\"color: {model_color};\">{model_name}</span></b>")
}




#' Add background color
#'
#' @param The width of the background color
#'
#' @keywords internal
bar_style_perf <- function(width = 1, color = "lightgreen") {
  if (is.na(width)) {
    width <- 0
  }
  position <- paste0(width * 100, "%")
  list(
    background = sprintf(
      "linear-gradient(90deg, %2$s %1$s, transparent %1$s)",
      position, color
    ),
    backgroundSize = "98% 88%",
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center"
  )
}


#' Add background color
#'
#' @param The width of the background color
#'
#' @keywords internal
bar_style_nb_reactable <- function(width = 1,
                                   pos_fill = "lightgreen",
                                   neg_fill = "pink") {
  if (is.na(width)) {
    width <- 0
  }
  # Split the background into 2 halves for negative and positive bars.
  # For positive bars, draw the bar from 50% to 50% + width
  # For negative bars, draw the bar from 50% + -width to 50%
  position <- paste0((0.5 + width / 2) * 100, "%")
  if (width >= 0) {
    background <- sprintf(
      "linear-gradient(90deg, transparent 50%%, %2$s 50%%, %2$s %1$s, transparent %1$s)",
      position, pos_fill
    )
  } else {
    background <- sprintf(
      "linear-gradient(90deg, transparent %1$s, %2$s %1$s, %2$s 50%%, transparent 50%%)",
      position, neg_fill
    )
  }
  list(
    background = background,
    backgroundSize = "98% 88%",
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center"
  )
}
