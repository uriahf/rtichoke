#' Render a canonical ReportSpec with the shared browser renderer
#'
#' Serialize an already-assembled canonical ReportSpec and delegate report
#' composition and component dispatch to the vendored rtichoke_viz
#' `renderReport()` implementation. This helper does not inspect or rebuild
#' embedded component specifications.
#'
#' @param report_spec A complete canonical ReportSpec produced by
#'   [rtichoke_viz_report_spec()].
#' @param section_group_tabs Whether sibling groups within a section should be
#'   presented as tabs. Components within each group remain stacked.
#'
#' @return A browsable htmltools tag list.
#' @noRd
render_rtichoke_viz_report_browser <- function(
  report_spec,
  section_group_tabs = FALSE
) {
  id <- rtichoke_viz_browser_id()
  json <- jsonlite::toJSON(
    report_spec,
    auto_unbox = TRUE,
    digits = NA,
    null = "null"
  )
  json <- gsub("</", "<\\/", json, fixed = TRUE)

  vendor_dir <- system.file("rtichoke-viz", package = "rtichoke")
  bundle <- paste(
    readLines(
      file.path(vendor_dir, "rtichoke-viz.js"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
  bundle <- gsub("</", "<\\/", bundle, fixed = TRUE)

  dependency <- htmltools::htmlDependency(
    name = "rtichoke-viz",
    version = "0.13.0",
    src = c(file = vendor_dir),
    stylesheet = "rtichoke-viz.css"
  )
  render_options <- if (isTRUE(section_group_tabs)) {
    paste0(
      ", {\n",
      "  sectionGroupPresentation: 'tabs',\n",
      "  groupPresentation: 'stacked'\n",
      "}"
    )
  } else {
    ""
  }
  initializer <- paste0(
    "const spec = JSON.parse(document.querySelector('#",
    id,
    "-spec').textContent);\n",
    "document.querySelector('#",
    id,
    "').append(renderReport(spec",
    render_options,
    "));"
  )
  script <- paste(bundle, initializer, sep = "\n")

  htmltools::browsable(htmltools::attachDependencies(
    htmltools::tagList(
      htmltools::tags$div(id = id, class = "rtichoke-viz-report"),
      htmltools::tags$script(
        id = paste0(id, "-spec"),
        type = "application/json",
        htmltools::HTML(json)
      ),
      htmltools::tags$script(type = "module", htmltools::HTML(script))
    ),
    dependency
  ))
}
