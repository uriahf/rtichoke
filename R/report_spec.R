#' Assemble a canonical rtichoke_viz ReportSpec
#'
#' Compose complete standalone canonical component specifications into the
#' report-level contract. This helper does not calculate statistics, rewrite
#' component semantics, or create report-global evaluation identity.
#'
#' @param ... Complete canonical component specifications.
#' @param title Optional report title.
#' @param component_titles Optional list of presentation titles, one per
#'   component. Use `NULL` for components without a title.
#'
#' @return A nested list representing a canonical ReportSpec.
#' @noRd
rtichoke_viz_report_spec <- function(
  ...,
  title = NULL,
  component_titles = NULL
) {
  specs <- list(...)
  if (length(specs) == 0L) {
    stop("ReportSpec requires at least one component", call. = FALSE)
  }

  supported_types <- c(
    "performance_table",
    "roc",
    "calibration",
    "precision_recall",
    "gains",
    "lift",
    "decision_curve",
    "interventions_avoided"
  )
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    if (!is.list(spec) || !identical(spec$schemaVersion, "2.0")) {
      stop(
        "Report component ",
        i,
        " must be a complete canonical schemaVersion 2.0 specification",
        call. = FALSE
      )
    }
    if (is.null(spec$type) || !spec$type %in% supported_types) {
      stop(
        "Report component ",
        i,
        " has unsupported type: ",
        if (is.null(spec$type)) {
          "<missing>"
        } else {
          spec$type
        },
        call. = FALSE
      )
    }
  }

  if (is.null(component_titles)) {
    component_titles <- rep(list(NULL), length(specs))
  }
  if (!is.list(component_titles) || length(component_titles) != length(specs)) {
    stop(
      "component_titles must be a list with one entry per component",
      call. = FALSE
    )
  }

  base_ids <- vapply(
    specs,
    function(spec) gsub("_", "-", spec$type, fixed = TRUE),
    character(1)
  )
  seen <- integer(0)
  names(seen) <- character(0)
  component_ids <- vapply(
    base_ids,
    function(base_id) {
      count <- if (base_id %in% names(seen)) {
        seen[[base_id]] + 1L
      } else {
        1L
      }
      seen[[base_id]] <<- count
      if (count == 1L) {
        base_id
      } else {
        paste0(base_id, "-", count)
      }
    },
    character(1)
  )

  components <- lapply(
    seq_along(specs),
    function(i) {
      component <- list(
        id = component_ids[[i]],
        spec = specs[[i]]
      )
      component_title <- component_titles[[i]]
      if (!is.null(component_title)) {
        if (!is.character(component_title) || length(component_title) != 1L) {
          stop(
            "Each component title must be NULL or a single string",
            call. = FALSE
          )
        }
        component$title <- component_title
      }
      component
    }
  )

  report <- list(
    schemaVersion = "1.0",
    type = "report",
    components = components
  )
  if (!is.null(title)) {
    if (!is.character(title) || length(title) != 1L) {
      stop("Report title must be NULL or a single string", call. = FALSE)
    }
    report$title <- title
  }
  report
}
