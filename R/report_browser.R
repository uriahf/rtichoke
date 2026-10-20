#' Render a canonical ReportSpec with the shared browser renderer
#'
#' Serialize an already-assembled canonical ReportSpec and delegate report
#' composition and component dispatch to the vendored rtichoke_viz
#' `renderReport()` implementation. This helper does not inspect or rebuild
#' embedded component specifications.
#'
#' @param report_spec A complete canonical ReportSpec produced by
#'   `rtichoke_viz_report_spec()`.
#'
#' @return A browsable htmltools tag list.
#' @noRd
render_rtichoke_viz_report_browser <- function(
  report_spec,
  header_annotation_html = performance_metrics_cheat_sheet_html()
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

  viz_css <- paste(
    readLines(
      file.path(vendor_dir, "rtichoke-viz.css"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )

  fn_binding <- resolve_render_report_identifier(bundle)
  binding_line <- if (fn_binding != "renderReport") {
    paste0("const renderReport = ", fn_binding, ";\n")
  } else {
    ""
  }

  annotation_js <- if (
    !is.null(header_annotation_html) && nzchar(header_annotation_html)
  ) {
    annotation_json <- jsonlite::toJSON(
      header_annotation_html,
      auto_unbox = TRUE,
      null = "null"
    )
    annotation_json <- gsub("</", "<\\/", annotation_json, fixed = TRUE)
    paste0(
      "const cheatSheetWrapper = document.createElement('div');\n",
      "cheatSheetWrapper.innerHTML = ",
      annotation_json,
      ";\n",
      "const cheatSheetNode = cheatSheetWrapper.firstElementChild;\n",
      "if (headerNode && headerNode.nextSibling) {\n",
      "  reportNode.insertBefore(cheatSheetNode, headerNode.nextSibling);\n",
      "} else {\n",
      "  reportNode.appendChild(cheatSheetNode);\n",
      "}\n"
    )
  } else {
    ""
  }

  initializer <- paste0(
    binding_line,
    "const spec = JSON.parse(document.querySelector('#",
    id,
    "-spec').textContent);\n",
    "const reportNode = renderReport(spec, { sectionGroupPresentation: 'tabs', groupPresentation: 'tabs', sectionComponentPresentation: 'tabs' });\n",
    "const headerNode = reportNode.querySelector('.rtichoke-report__header');\n",
    annotation_js,
    "document.querySelector('#",
    id,
    "').append(reportNode);"
  )
  script <- paste(bundle, initializer, sep = "\n")

  htmltools::browsable(
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(viz_css)),
      htmltools::tags$style(htmltools::HTML(summary_report_density_css())),
      htmltools::tags$div(id = id, class = "rtichoke-viz-report"),
      htmltools::tags$script(
        id = paste0(id, "-spec"),
        type = "application/json",
        htmltools::HTML(json)
      ),
      htmltools::tags$script(type = "module", htmltools::HTML(script))
    )
  )
}

#' Resolve the local identifier exported as renderReport in a JS bundle
#'
#' @param bundle Character string containing JS bundle text.
#' @return Character string of the local identifier bound to renderReport.
#' @noRd
resolve_render_report_identifier <- function(bundle) {
  if (!is.character(bundle) || length(bundle) != 1L || is.na(bundle)) {
    stop(
      "Could not resolve renderReport export from rtichoke-viz bundle",
      call. = FALSE
    )
  }

  alias_match <- regmatches(
    bundle,
    regexec(
      "export\\s*\\{[^}]*?\\b([a-zA-Z0-9_$]+)\\s+as\\s+renderReport\\b[^}]*?\\}",
      bundle
    )
  )[[1]]

  if (length(alias_match) >= 2L && nzchar(alias_match[2])) {
    return(alias_match[2])
  }

  shorthand_match <- grepl(
    "export\\s*\\{[^}]*?\\brenderReport\\b[^}]*?\\}",
    bundle
  ) ||
    grepl("export\\s+(function|const|let|var)\\s+renderReport\\b", bundle) ||
    grepl("(function|const|let|var)\\s+renderReport\\b", bundle)

  if (shorthand_match) {
    return("renderReport")
  }

  stop(
    "Could not resolve renderReport export from rtichoke-viz bundle",
    call. = FALSE
  )
}

#' Generate layout density CSS for browser summary reports
#'
#' @return A character string containing summary-report-scoped layout and
#'   density CSS rules.
#' @noRd
summary_report_density_css <- function() {
  paste0(
    ".rtichoke-report {\n",
    "  max-width: 1040px;\n",
    "  gap: 1.5rem;\n",
    "}\n",
    ".rtichoke-report__section {\n",
    "  gap: 1.25rem;\n",
    "}\n",
    ".rtichoke-report__group {\n",
    "  gap: 0.875rem;\n",
    "}\n",
    ".rtichoke-report__component,\n",
    ".rtichoke-report__tabpanel {\n",
    "  gap: 0.5rem;\n",
    "}\n",
    ".rtichoke-report .rtichoke-viz-chart {\n",
    "  min-height: 500px;\n",
    "  height: 500px;\n",
    "}\n",
    ".rtichoke-report .rtichoke-calibration {\n",
    "  min-height: 550px;\n",
    "  height: 550px;\n",
    "}\n",
    ".rtichoke-report__tabpanel .rtichoke-report__component-title {\n",
    "  display: none;\n",
    "}\n",
    ".rtichoke-report__nav {\n",
    "  background-color: transparent;\n",
    "  border: none;\n",
    "  border-bottom: 1px solid #e5e7eb;\n",
    "  border-radius: 0;\n",
    "  padding: 0.5rem 0;\n",
    "}\n",
    ".rtichoke-report .rtichoke-summary-metrics {\n",
    "  border: none;\n",
    "  background: transparent;\n",
    "  box-shadow: none;\n",
    "}\n",
    ".rtichoke-report .rtichoke-summary-metrics__title {\n",
    "  display: none;\n",
    "}\n",
    ".rtichoke-report .rtichoke-summary-metrics__table th,\n",
    ".rtichoke-report .rtichoke-summary-metrics__table td {\n",
    "  padding: 0.35rem 0.65rem;\n",
    "  border-bottom: 1px solid #e5e7eb;\n",
    "}\n"
  )
}

#' Generate Outcome Distribution Note HTML for browser summary reports
#'
#' @return A character string containing R-owned browser-native HTML for the
#'   time-dependent Outcome Distribution explanatory note.
#' @noRd
time_dependent_outcome_distribution_note_html <- function() {
  paste0(
    "<details class=\"rtichoke-cheat-sheet\">\n",
    "  <summary>Outcome Distribution Note</summary>\n",
    "  <div class=\"rtichoke-cheat-sheet__content\">\n",
    "    <p>Outcome Distribution shows raw observed outcome status over time. Each fixed horizon summarizes the observed counts and percentages of observations classified as Event, Competing event, Event-free, or Censored. Censored observations are shown as their own observed status; no Aalen-Johansen, IPCW, or other censoring adjustment is applied.</p>\n",
    "    <p>This report does not yet include predicted probabilities, calibration, discrimination metrics, threshold-based metrics, or decision-curve analysis.</p>\n",
    "  </div>\n",
    "</details>"
  )
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
