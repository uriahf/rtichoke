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
  json <- jsonlite::toJSON(report_spec, auto_unbox = TRUE, digits = NA)
  json <- gsub("</", "<\\/", json, fixed = TRUE)

  dependency <- htmltools::htmlDependency(
    name = "rtichoke-viz",
    version = "0.5.0",
    src = c(file = system.file("rtichoke-viz", package = "rtichoke")),
    script = list(src = "rtichoke-viz.js", type = "module"),
    stylesheet = "rtichoke-viz.css"
  )
  script <- paste0(
    "import { renderReport } from ",
    "'./lib/rtichoke-viz-0.5.0/rtichoke-viz.js';\n",
    "const spec = JSON.parse(document.querySelector('#",
    id,
    "-spec').textContent);\n",
    "document.querySelector('#",
    id,
    "').append(renderReport(spec));"
  )

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
