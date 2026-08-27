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

#' Assemble a structured canonical rtichoke_viz ReportSpec v1.1
#'
#' Validate and compose already-wrapped sections, groups, and complete
#' standalone canonical component specifications. This helper is deliberately
#' separate from [rtichoke_viz_report_spec()] so the v1.0 contract remains
#' unchanged.
#'
#' @param ... Structured report sections.
#' @param title Optional report title.
#'
#' @return A nested list representing a canonical ReportSpec v1.1.
#' @noRd
rtichoke_viz_report_spec_v1_1 <- function(..., title = NULL) {
  sections <- list(...)
  if (length(sections) == 0L) {
    stop("ReportSpec v1.1 requires at least one section", call. = FALSE)
  }

  is_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }
  validate_component <- function(component, location) {
    if (!is.list(component) || !identical(component$type, "component")) {
      stop(location, " must have type = \"component\"", call. = FALSE)
    }
    if (!is_string(component$id)) {
      stop(location, " must have a non-empty string id", call. = FALSE)
    }
    if (
      !is.null(component$title) &&
        (!is.character(component$title) || length(component$title) != 1L)
    ) {
      stop(location, " title must be a single string", call. = FALSE)
    }
    spec <- component$spec
    if (!is.list(spec) || !identical(spec$schemaVersion, "2.0")) {
      stop(
        location,
        " must contain a complete canonical schemaVersion 2.0 specification",
        call. = FALSE
      )
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
    if (is.null(spec$type) || !spec$type %in% supported_types) {
      stop(location, " contains an unsupported canonical type", call. = FALSE)
    }
    invisible(NULL)
  }

  section_ids <- character(0)
  group_ids <- character(0)
  component_ids <- character(0)
  for (section_index in seq_along(sections)) {
    section <- sections[[section_index]]
    location <- paste0("Report section ", section_index)
    if (!is.list(section)) {
      stop(location, " must be a list", call. = FALSE)
    }
    if (!is_string(section$id) || !is_string(section$title)) {
      stop(location, " must have non-empty string id and title", call. = FALSE)
    }
    if (section$id %in% section_ids) {
      stop("Report section ids must be unique", call. = FALSE)
    }
    section_ids <- c(section_ids, section$id)
    if (!is.list(section$items) || length(section$items) == 0L) {
      stop(location, " must contain at least one item", call. = FALSE)
    }

    for (item_index in seq_along(section$items)) {
      item <- section$items[[item_index]]
      item_location <- paste0(location, " item ", item_index)
      if (!is.list(item) || is.null(item$type)) {
        stop(
          item_location,
          " must be a component or group wrapper",
          call. = FALSE
        )
      }
      if (identical(item$type, "component")) {
        validate_component(item, item_location)
        if (item$id %in% component_ids) {
          stop("Report component ids must be unique", call. = FALSE)
        }
        component_ids <- c(component_ids, item$id)
      } else if (identical(item$type, "group")) {
        if (!is_string(item$id) || !is_string(item$title)) {
          stop(
            item_location,
            " group must have non-empty string id and title",
            call. = FALSE
          )
        }
        if (item$id %in% group_ids) {
          stop("Report group ids must be unique", call. = FALSE)
        }
        group_ids <- c(group_ids, item$id)
        if (!is.list(item$components) || length(item$components) == 0L) {
          stop(item_location, " group must contain components", call. = FALSE)
        }
        for (component_index in seq_along(item$components)) {
          component <- item$components[[component_index]]
          component_location <- paste0(
            item_location,
            " component ",
            component_index
          )
          validate_component(component, component_location)
          if (component$id %in% component_ids) {
            stop("Report component ids must be unique", call. = FALSE)
          }
          component_ids <- c(component_ids, component$id)
        }
      } else {
        stop(
          item_location,
          " must have type = \"component\" or \"group\"",
          call. = FALSE
        )
      }
    }
  }

  report <- list(
    schemaVersion = "1.1",
    type = "report",
    sections = sections
  )
  if (!is.null(title)) {
    if (!is.character(title) || length(title) != 1L) {
      stop("Report title must be NULL or a single string", call. = FALSE)
    }
    report$title <- title
  }
  report
}
