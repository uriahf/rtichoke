#' Render a canonical ReportSpec with the shared browser renderer
#'
#' Serialize an already-assembled canonical ReportSpec and delegate report
#' composition and component dispatch to the vendored rtichoke_viz
#' `renderReport()` implementation. This helper does not inspect or rebuild
#' embedded component specifications.
#'
#' @param report_spec A complete canonical ReportSpec produced by
#'   [rtichoke_viz_report_spec()].
#'
#' @return A browsable htmltools tag list.
#' @noRd
render_rtichoke_viz_report_browser <- function(report_spec) {
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
    version = "0.11.0",
    src = c(file = vendor_dir),
    stylesheet = "rtichoke-viz.css"
  )
  initializer <- paste0(
    "const spec = JSON.parse(document.querySelector('#",
    id,
    "-spec').textContent);\n",
    "document.querySelector('#",
    id,
    "').append(renderReport(spec));"
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
