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
    version = "0.15.0",
    src = c(file = vendor_dir),
    stylesheet = "rtichoke-viz.css"
  )
  cheat_sheet <- performance_metrics_cheat_sheet_html()
  cheat_sheet_json <- jsonlite::toJSON(
    cheat_sheet,
    auto_unbox = TRUE,
    null = "null"
  )
  cheat_sheet_json <- gsub("</", "<\\/", cheat_sheet_json, fixed = TRUE)

  initializer <- paste0(
    "const spec = JSON.parse(document.querySelector('#",
    id,
    "-spec').textContent);\n",
    "const reportNode = renderReport(spec, { sectionGroupPresentation: 'tabs', groupPresentation: 'tabs', sectionComponentPresentation: 'tabs' });\n",
    "const headerNode = reportNode.querySelector('.rtichoke-report__header');\n",
    "const cheatSheetWrapper = document.createElement('div');\n",
    "cheatSheetWrapper.innerHTML = ",
    cheat_sheet_json,
    ";\n",
    "const cheatSheetNode = cheatSheetWrapper.firstElementChild;\n",
    "if (headerNode && headerNode.nextSibling) {\n",
    "  reportNode.insertBefore(cheatSheetNode, headerNode.nextSibling);\n",
    "} else {\n",
    "  reportNode.appendChild(cheatSheetNode);\n",
    "}\n",
    "document.querySelector('#",
    id,
    "').append(reportNode);"
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

#' Generate Performance Metrics Cheat Sheet HTML for browser summary reports
#'
#' @return A character string containing R-owned browser-native HTML for the
#'   Performance Metrics Cheat Sheet.
#' @noRd
performance_metrics_cheat_sheet_html <- function() {
  paste0(
    "<details class=\"rtichoke-cheat-sheet\">\n",
    "  <summary>Performance Metrics Cheat Sheet</summary>\n",
    "  <div class=\"rtichoke-cheat-sheet__content\">\n",
    "    <section class=\"rtichoke-cheat-sheet__section\">\n",
    "      <h4>Confusion Matrix</h4>\n",
    "      <table class=\"rtichoke-cheat-sheet__table\">\n",
    "        <thead>\n",
    "          <tr>\n",
    "            <th></th>\n",
    "            <th>Predicted +</th>\n",
    "            <th>Predicted -</th>\n",
    "          </tr>\n",
    "        </thead>\n",
    "        <tbody>\n",
    "          <tr>\n",
    "            <th>Real Positive</th>\n",
    "            <td>TP</td>\n",
    "            <td>FN</td>\n",
    "          </tr>\n",
    "          <tr>\n",
    "            <th>Real Negative</th>\n",
    "            <td>FP</td>\n",
    "            <td>TN</td>\n",
    "          </tr>\n",
    "        </tbody>\n",
    "      </table>\n",
    "    </section>\n",
    "    <section class=\"rtichoke-cheat-sheet__section\">\n",
    "      <h4>Metrics &amp; Formulas</h4>\n",
    "      <dl class=\"rtichoke-cheat-sheet__metrics\">\n",
    "        <dt>Prevalence</dt>\n",
    "        <dd><code>(TP + FN) / (TP + FP + TN + FN)</code></dd>\n",
    "        <dt>PPCR</dt>\n",
    "        <dd><code>(TP + FP) / (TP + FP + TN + FN)</code></dd>\n",
    "        <dt>Sensitivity / Recall / TPR</dt>\n",
    "        <dd>\n",
    "          <code>TP / (TP + FN)</code><br />\n",
    "          <code>TP / Real Positives</code><br />\n",
    "          <code>P(Predicted Positive | Real Positive)</code>\n",
    "        </dd>\n",
    "        <dt>Specificity / TNR</dt>\n",
    "        <dd>\n",
    "          <code>TN / (TN + FP)</code><br />\n",
    "          <code>TN / Real Negatives</code><br />\n",
    "          <code>P(Predicted Negative | Real Negative)</code>\n",
    "        </dd>\n",
    "        <dt>PPV / Precision</dt>\n",
    "        <dd>\n",
    "          <code>TP / (TP + FP)</code><br />\n",
    "          <code>TP / Predicted Positives</code><br />\n",
    "          <code>P(Real Positive | Predicted Positive)</code>\n",
    "        </dd>\n",
    "        <dt>NPV</dt>\n",
    "        <dd>\n",
    "          <code>TN / (TN + FN)</code><br />\n",
    "          <code>TN / Predicted Negatives</code><br />\n",
    "          <code>P(Real Negative | Predicted Negative)</code>\n",
    "        </dd>\n",
    "        <dt>Lift</dt>\n",
    "        <dd><code>PPV / Prevalence</code></dd>\n",
    "        <dt>Net Benefit</dt>\n",
    "        <dd>\n",
    "          <code>TP / N - FP / N * p_t / (1 - p_t)</code><br />\n",
    "          <small>where N = TP + FP + TN + FN</small>\n",
    "        </dd>\n",
    "      </dl>\n",
    "    </section>\n",
    "  </div>\n",
    "</details>"
  )
}
