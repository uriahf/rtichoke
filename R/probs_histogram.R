#' Prepare events per strata data
#'
#' @inheritParams create_roc_curve
#' 
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' prepare_probs_distribution_data(
#'   probs = list(example_dat$estimated_probabilities),
#'   reals = list(example_dat$outcome)
#' )
#'
#' # Several Models
#'
#' prepare_probs_distribution_data(
#'   probs = list(
#'     "First Model" = example_dat$estimated_probabilities,
#'     "Second Model" = example_dat$random_guess
#'   ),
#'   reals = list(example_dat$outcome)
#' )
#'
#'
#' # Several Populations
#'
#'
#' prepare_probs_distribution_data(
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
#' }

prepare_probs_distribution_data <- function(
    probs, 
    reals = NA, 
    stratified_by = "probability_threshold",
    by = 0.01,
    condition_on = NA) {
  
  if (is.null(names(probs))) {
    names(probs) <- paste("model", seq_len(length(probs)))
  }
  
  # TODO: breaks after return
  
  if (stratified_by != "probability_threshold" &
      length(unique(probs[[1]])) != 1) {
    breaks <-  stats::quantile(probs[[1]],
                    probs = rev(seq(0, 1, by = by))
    )
  } else if (stratified_by != "probability_threshold" &
             length(unique(probs[[1]])) == 1) {
    breaks <- c(0, 1)

  } else {

    breaks <- round(
      seq(0, 1, by = by),
      digits = nchar(format(by, scientific = FALSE))
    )

  }
  
  
   
  
  if ((length(probs) > 1) & (length(reals) == 1) & is.na(condition_on)) {
    
    
    
    return(
      probs %>%
        purrr::map(
          ~ prepare_probs_distribution_data(
            probs = list(.),
            reals = reals,
            by = by,
            stratified_by = stratified_by,
            condition_on = condition_on
          )
        ) %>%
        dplyr::bind_rows(.id = "reference_group")
    )
    
  } else if (
    (length(probs) > 1) & (length(reals) == 1) & !is.na(condition_on)) {
  
    
    nested_hist_data <-  probs %>%
        purrr::map(
          ~ prepare_probs_distribution_data(
            probs = list(.),
            reals = reals,
            by = by,
            stratified_by = stratified_by,
            condition_on = condition_on
          )
        ) 
    
    return(
    list(
      "real_positives" = purrr::map_df(
        nested_hist_data, ~bind_rows(.x$real_positives), 
        .id = "reference_group"),
      "real_negatives" = purrr::map_df(
        nested_hist_data, ~bind_rows(.x$real_negatives), 
        .id = "reference_group")
    ))
    
  } else if (
      (length(probs) > 1) & (length(reals) > 1) & !is.na(condition_on)) {

    names(reals) <- paste("population", seq_len(length(reals)))

    
    return(
    list(
    "real_positives" = purrr::map2_df(
      probs,
      reals,
      function(x, y) {
        
        create_mids_and_counts_data_from_probs(
          x[y==1], breaks = breaks, stratified_by = stratified_by, by = by)
        
      }, .id = "reference_group") |>
      add_bounds_variables(by, stratified_by) |>
          add_text_variables_to_probs_distribution_data(stratified_by, by = by),
    "real_negatives" = purrr::map2_df(
      probs,
      reals,
      function(x, y) {
        
        create_mids_and_counts_data_from_probs(
          x[y==0], breaks = breaks, stratified_by = stratified_by, by = by)
        
      }, .id = "reference_group") |>
      add_bounds_variables(by, stratified_by) |>
      add_text_variables_to_probs_distribution_data(stratified_by, by = by)
      )
    )

   }
  
  
  
  
  
  half_by <- by / 2
  
  reference_groups <- names(probs)
  
  if ( is.na(condition_on) ) {
  
  hist_data <- probs |> 
    purrr::map_df(~ create_mids_and_counts_data_from_probs(
      .x, breaks = breaks, stratified_by = stratified_by, by = by), 
    .id = "reference_group") |>
    add_bounds_variables(by, stratified_by) |> 
    add_text_variables_to_probs_distribution_data(stratified_by, by = by)

  

  } else if ( condition_on %in% c("probs", "reals") ) {
    
    hist_data_events <- probs |> 
      purrr::map_df(~ create_mids_and_counts_data_from_probs(
        .x[reals[[1]] == 1], breaks = breaks, stratified_by = stratified_by, by = by), 
      .id = "reference_group") |>
      add_bounds_variables(by, stratified_by) |> 
      add_text_variables_to_probs_distribution_data(stratified_by, by = by)
    
    hist_data_nonevents <- probs |> 
      purrr::map_df(~ create_mids_and_counts_data_from_probs(
        .x[reals[[1]] == 0], breaks = breaks, stratified_by = stratified_by, by = by), 
        .id = "reference_group") |>
      add_bounds_variables(by, stratified_by) |> 
      add_text_variables_to_probs_distribution_data(stratified_by, by = by)
    
    hist_data = list(
      real_positives = hist_data_events,
      real_negatives = hist_data_nonevents
    )
    
  }
  
  hist_data
  
}

create_mids_and_counts_data_from_probs <- function(
    probs, breaks, stratified_by, by){
  
  hist_data <- hist(
    probs,
    plot = FALSE, 
    breaks = breaks
  )
  
  if (stratified_by == "probability_threshold") {
    
    hist_data %>% 
      .[c("mids", "counts")] |> 
      tibble::as_tibble() |> 
      tibble::add_row(
        mids = 0, counts = as.integer(0), .before = 0)
    
  } else {
    
    tibble::tibble(
      mids = round(
        seq(by, 1 + by, by = by),
        digits = nchar(format(by, scientific = FALSE))
      ),
      counts = as.integer(c(hist_data$counts, 0)) # TODO fix this
    )
    
  }
  
}

add_text_variables_to_probs_distribution_data <- function(
    probs_distribution_data, stratified_by, 
    cumulative = FALSE, by) {
  
  if (stratified_by == "probability_threshold") {
  
  probs_distribution_data |> 
    dplyr::mutate(
      text_obs = glue::glue("{counts} observations in "),
      text_range = glue::glue(
        "{ifelse(include_lower_bound==TRUE,'[','(')}{lower_bound}, \\
        {upper_bound}{ifelse(include_upper_bound==TRUE,']',')')}")
      ,
      text = glue::glue("{text_obs}{text_range}")
    )
    
  } else {
    
    if (cumulative == FALSE) {
    
    probs_distribution_data |> 
      dplyr::mutate(
        text_obs = glue::glue("{counts} observations with probability percentile of "),
        text_range = glue::glue(
          "{mids}"),
        text = glue::glue("{text_obs}{text_range}")
      )
      
    } else {
      
      probs_distribution_data |>
        dplyr::mutate(
          ppcr = round(
            1 - mids + by,
            digits = nchar(format(by, scientific = FALSE))
          ),
          text_obs = glue::glue("{counts} observations with ppcr of "),
          text_range = glue::glue(
            "{ppcr}"),
          text = glue::glue("{text_obs}{text_range}")
        ) 
      
    }
    
  }
  
}


add_bounds_variables <- function(probs_distribution_data, by, stratified_by) {
  
  if (stratified_by == "probability_threshold") {
  
  half_by <- by / 2
  
  probs_distribution_data |> 
    dplyr::mutate(
      lower_bound = dplyr::case_when(
        mids>0 ~ round(
          mids - half_by,
          digits = nchar(format(by, scientific = FALSE))
        ),
        mids==0 ~ 0),
      upper_bound = dplyr::case_when(
        mids>0 ~ round(
          mids + half_by,
          digits = nchar(format(by, scientific = FALSE))
        ),
        mids==0 ~ 0)
    ) |> 
    add_include_bound_variables_to_probs_distribution_data()
  
  } else {
    
    probs_distribution_data
    
  }
  
}

add_include_bound_variables_to_probs_distribution_data <- function(
    probs_distribution_data) {
  
  probs_distribution_data |> 
    dplyr::mutate(
      include_lower_bound = (lower_bound == 0 & upper_bound != 0),
      include_upper_bound = upper_bound != 0
    )
}


prepare_cumulative_probs_distribution_data <- function(
    probs_distribution_data, stratified_by, by){
  
  if (stratified_by == "probability_threshold") {
  
  probs_distribution_data |> 
    purrr::map(
      function(x){
        mutate(
          x,
          lower_bound = 0) |> 
          add_include_bound_variables_to_probs_distribution_data() |> 
          group_by(reference_group) |> 
          mutate(
            n_obs = sum(counts),
            counts =  cumsum(counts)
          ) |>
          ungroup() |> 
          add_text_variables_to_probs_distribution_data(
            stratified_by = stratified_by, by = by)
      }
    )
    
  } else {
    
    probs_distribution_data |> 
      purrr::map(
        function(x){
          # mutate(
          #   x,
          #   lower_bound = 0) |> 
            group_by(x, reference_group) |> 
            dplyr::arrange(desc(mids)) |> 
            mutate(
              n_obs = sum(counts),
              counts =  cumsum(counts)
            ) |> 
            ungroup() |> 
            add_text_variables_to_probs_distribution_data(
              stratified_by, cumulative = TRUE, by = by)
        }
      )
    
  }
  
}



add_performance_metrics_to_performance_data <- function(
    performance_data){
  
  performance_data |> 
    dplyr::mutate(
      N = real_positives + real_negatives,
      sensitivity = TP / (TP + FN),
      FPR = FP / (FP + TN),
      specificity = TN / (TN + FP),
      PPV = TP / (TP + FP),
      NPV = TN / (TN + FN),
      lift = (TP / (TP + FN)) / ((TP + FP) / N),
      predicted_positives = TP + FP,
      # ppcr = (TP + FP) / N,
      # NB = TP / N - (FP / N) * (
        # probability_threshold / (1 - probability_threshold))
    )
  
}

# TODO: fix probability_threshold for PPCR

join_cumulative_probs_distribution_data_to_main_slider <- function(
    main_slider_tibble, 
    cumulative_probs_distribution_data,
    reference_group_column_name,
    stratified_by) {
  
  if (stratified_by == "probability_threshold") {
  
  main_slider_tibble |>
    dplyr::left_join(
      cumulative_probs_distribution_data$real_negatives |> 
        select(reference_group, upper_bound, counts, n_obs) |> 
        rename(
          {{reference_group_column_name}} := "reference_group",
          "probability_threshold" = "upper_bound",
          "TN" = "counts",
          "real_negatives" = "n_obs",
        ), by = "probability_threshold"
    ) |> 
    mutate(
      "FP" = real_negatives - TN
    ) |>
    left_join(
      cumulative_probs_distribution_data$real_positives |>
        select(reference_group, upper_bound, counts, n_obs) |>
        rename(
          {{reference_group_column_name}} := "reference_group",
          "probability_threshold" = "upper_bound",
          "FN" = "counts",
          "real_positives" = "n_obs",
        ), by = c("probability_threshold", reference_group_column_name)
    ) |>
    mutate(
      "TP" = real_positives - FN
    )
    
  } else {

    main_slider_tibble |>
      dplyr::left_join(
        cumulative_probs_distribution_data$real_negatives |>
          dplyr::mutate(ppcr = round(ppcr, digits = 2)) |>  # TODO: fix this
          select(reference_group, ppcr, counts, n_obs) |>
          rename(
            {{reference_group_column_name}} := "reference_group",
            "FP" = "counts",
            "real_negatives" = "n_obs",
          ), by = "ppcr"
      ) |> 
      mutate(
        "TN" = real_negatives - FP
      ) |>
      left_join(
        cumulative_probs_distribution_data$real_positives |>
          select(reference_group, ppcr, counts, n_obs) |>
          rename(
            {{reference_group_column_name}} := "reference_group",
            "TP" = "counts",
            "real_positives" = "n_obs",
          ), by = c("ppcr", reference_group_column_name)
      ) |>
      mutate(
        "FN" = real_positives - TP
      )
    
  }
  
  
}



turn_cumulative_probs_distribution_data_to_performance_data <- function(
    cumulative_probs_distribution_data, 
    reference_group_column_name,
    by = 0.01,
    stratified_by = "probability_threshold"
) {
  
  if (stratified_by == "probability_threshold") {
    
    main_slider_tibble <- tibble(
        probability_threshold = round(
          seq(0, 1, by = by),
          digits = nchar(format(by, scientific = FALSE))
        )
      )
    
  } else {
    
    main_slider_tibble <- tibble(
      
      ppcr = round(
        seq(0, 1, by = by),
        digits = nchar(format(by, scientific = FALSE))
      )
      
    )
    
    
  }
  
  main_slider_tibble |> 
    join_cumulative_probs_distribution_data_to_main_slider(
      cumulative_probs_distribution_data = cumulative_probs_distribution_data,
      reference_group_column_name = {{reference_group_column_name}},
      stratified_by = stratified_by) |> 
    add_performance_metrics_to_performance_data() 
  
  
}



# TODO: add documentation, include inherit params
# TODO: refactor and use map for multiple populations

create_probs_histogram <- function(
    probs, reals, condition_on = NA, stratified_by, by) {
  
  probs_distribution <- prepare_probs_distribution_data(
    probs = probs,
    reals = reals, 
    condition_on = condition_on,
    stratified_by = stratified_by, 
    by = by
  )
  
  real_positives_hist_dat <- probs_distribution$real_positives |> 
    # mutate(name = "real_positives") |>
    rename("real_positives" = counts,
           "cat" = "mids") |> 
    select(cat, real_positives)    
  
  
  real_negatives_hist_dat <- probs_distribution$real_negatives |> 
    # mutate(name = "real_negatives") |>
    rename("real_negatives" = counts,
           "cat" = "mids") |> 
    select(cat, real_negatives)  
  
  full_hist_dat <- left_join(
    real_positives_hist_dat,
    real_negatives_hist_dat,
    by = 'cat',
  )
  
  inputIdtry <- sprintf("filter_%s_%s", "long-confusion-matrix", "probability_threshold")
  labeltry <- "Filter by Minimum Price"
  widthtry <- "200px"
  valueIdtry <- sprintf("filter_%s_%s__value", "long-confusion-matrix", "probability_threshold")
  
  
  oninputtry <-  paste(
    sprintf("document.getElementById('%s').textContent = this.value;", valueIdtry),
    sprintf("Reactable.setFilter('%s', '%s', this.value)", "long-confusion-matrix", "probability_threshold")
  )
  
  
  if ( condition_on == "probs" ) {
  
  hist_predicted_positives <- r2d3::r2d3(
    data = full_hist_dat,
    script = system.file(
      "d3/probs_hist_predicted_positives.js", 
      package = "rtichoke"),
    width = 250,
    height = 250,
    container = 'div', 
    elementId = "hist-predicted-positives"
  )
  
  hist_predicted_negatives <- r2d3::r2d3(
    data = full_hist_dat,
    script = system.file(
      "d3/probs_hist_predicted_negatives.js", 
      package = "rtichoke"),
    width = 250,
    height = 250,
    container = 'div', 
    elementId = "hist-predicted-negatives"
  )
  
  
  

  crosstalk::bscols(
    widths = c(6, 12),
    
      div(
        tags$label(`for` = sprintf("filter_%s_%s", "long-confusion-matrix", 
                                   stratified_by), 
                   ifelse(stratified_by == "probability_threshold",
                          "Prob. Threshold: ",
                          "Predicted Positives (Rate):"
                   )),
        
        browsable(tags$input(
          # id = 'sliderelse',
          # id = sprintf("filter_%s_%s", "long-confusion-matrix", "probability_threshold"),
          id =paste("filter_long-confusion-matrix", stratified_by, sep = "-" ),
          class = "probs-histogram-slider",
          type = "range",
          min = 0,
          max = 1,
          step = by,
          value = 1,
          oninput = oninputtry,
          onchange = oninputtry, # For IE11 support
          style = "width: 100%;"
        ))),
        div(
    hist_predicted_negatives , id = "DivPredictedNegatives", style = css(
      height = "300px",
      border = "3px red solid"
    )),
    div(hist_predicted_positives, id = "DivPredictedPositives", style = css(
height = "300px",
border = "3px red solid"
))
      
  )
  
  } else if (condition_on == "reals") {
   
    
    full_hist_dat <- left_join(
      real_positives_hist_dat,
      real_negatives_hist_dat,
      by = 'cat',
    )
    
    hist_predicted <- r2d3::r2d3(
      data = full_hist_dat,
      script = system.file(
        "d3/probs_hist.js", 
        package = "rtichoke"),
      width = 250,
      height = 250,
      container = 'div', 
      elementId = paste("hist-predicted", stratified_by, sep = "_"),
      options = list(
        listenTO = paste("filter_long-confusion-matrix", stratified_by, sep = "-" ),
        outerDiv = paste("DivPredicted", stratified_by, sep = "_"))
    )
    
    crosstalk::bscols(
      widths = c(6, 6, 12),
        div(
          tags$label(`for` = sprintf("filter_%s_%s", "long-confusion-matrix", 
                                     stratified_by), 
                     ifelse(stratified_by == "probability_threshold",
                            "Prob. Threshold: ",
                            "Predicted Positives (Rate):"
                     )),
          
          browsable(tags$input(
            # id = 'sliderelse',
            # id = sprintf("filter_%s_%s", "long-confusion-matrix", "probability_threshold"),
            id =paste("filter_long-confusion-matrix", stratified_by, sep = "-" ),
            class = paste("probs-histogram-slider", stratified_by, sep = "-" ),
            type = "range",
            min = 0,
            max = 1,
            step = by,
            value = 1,
            oninput = oninputtry,
            onchange = oninputtry, # For IE11 support
            style = "width: 100%;"
          ))),
      create_confusion_matrix_as_input_element(),
      div(hist_predicted, 
          id = paste("DivPredicted", stratified_by, sep = "_"), style = css(
        height = "300px",
        border = "3px red solid"        
      )
      )
    ) 
    
  }
}

create_confusion_matrix_as_input_element <- function() {
  
  tibble::tribble(
    ~"type", ~"predicted_positives", ~"predicted_negatives",
    "Real Positives", "TP", "FN",
    "Real Negatives", "FP", "TN"
  ) |> 
    reactable::reactable(
      sortable = FALSE,
      fullWidth = FALSE,
      borderless = FALSE,
      defaultColDef = reactable::colDef(
        html = TRUE,
        align = "center",
        headerStyle  = list(fontWeight = 100),
        header = reactable::JS(confusion_matrix_header_maker())
      ),
      columns = list(
        predicted_positives = reactable::colDef(
          name = "Predicted Positives", 
          style = reactable::JS(confusion_matrix_style_maker("predicted_positives")), 
          cell = reactable::JS(confusion_matrix_cell_maker())
        ),
        type = reactable::colDef(
          name = "<br>All<br>Observations", 
          # cell = reactable::JS(confusion_matrix_real_cell_maker()),
          cell = reactable::JS(confusion_matrix_cell_maker()),
          align = "left",
          style = list(fontWeight = 100),
          minWidth = 140
        ),
        predicted_negatives = reactable::colDef(
          name = "Predicted Negatives", 
          cell = reactable::JS(confusion_matrix_cell_maker()),
          style = reactable::JS(confusion_matrix_style_maker("predicted_negatives"))
        )
      ),
      meta = list(
        highlightedMetrics = list(
          TP = TRUE, FP = TRUE, TN = TRUE, FN = TRUE, N = TRUE, PP = TRUE, PN = TRUE, RP = TRUE, RN = TRUE
        )),
      elementId = "cars-colors-table"
    )
  
}


