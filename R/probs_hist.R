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

  

  } else if ( condition_on == "reals" ) {
    
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


