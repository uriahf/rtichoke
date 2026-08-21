#' Build evaluation semantic metadata
#'
#' Derive stable internal model, population, and evaluation identities from the
#' existing `probs`/`reals` input shapes. This formalizes semantics without
#' changing public APIs or production plotting behavior.
#'
#' @param probs A list of model predictions.
#' @param reals A list of observed outcomes.
#'
#' @return A data frame with one row per evaluation and `model`, `population`,
#'   and `evaluation` columns.
#' @keywords internal
build_evaluation_metadata <- function(probs, reals) {
  probs_names <- names(probs)
  reals_names <- names(reals)

  if (length(reals) == 1L) {
    population <- if (!is.null(reals_names) && nzchar(reals_names[[1]])) {
      reals_names[[1]]
    } else {
      "population"
    }

    model <- if (!is.null(probs_names)) {
      probs_names
    } else if (length(probs) == 1L) {
      "model"
    } else {
      paste0("model_", seq_along(probs))
    }

    model[is.na(model) | !nzchar(model)] <- paste0(
      "model_",
      which(is.na(model) | !nzchar(model))
    )

    return(data.frame(
      model = model,
      population = rep(population, length(probs)),
      evaluation = model,
      stringsAsFactors = FALSE
    ))
  }

  evaluation <- if (!is.null(probs_names)) {
    probs_names
  } else if (!is.null(reals_names)) {
    reals_names
  } else {
    paste0("evaluation_", seq_along(probs))
  }

  evaluation[is.na(evaluation) | !nzchar(evaluation)] <- paste0(
    "evaluation_",
    which(is.na(evaluation) | !nzchar(evaluation))
  )

  data.frame(
    model = rep(NA_character_, length(evaluation)),
    population = evaluation,
    evaluation = evaluation,
    stringsAsFactors = FALSE
  )
}
