from pathlib import Path

# Upgrade all existing browser/report references to the already-vendored v0.6.0 payload.
for path in [
    "R/report_browser.R", "R/rtichoke_viz_v2.R",
    "tests/integration/quarto-browser-summary.qmd",
    "tests/integration/quarto-browser-summary-single.qmd",
    "tests/testthat/test-summary-report-browser.R",
    "tests/testthat/test-report-browser.R",
    ".github/workflows/quarto-browser-summary-acceptance.yaml",
    ".github/workflows/pkgdown-preview.yaml",
    "tools/verify-rtichoke-viz.R", "tests/testthat/test-rtichoke-viz-vendor.R",
]:
    p = Path(path); text = p.read_text(); p.write_text(text.replace("0.5.0", "0.6.0"))
for path in ["tools/verify-rtichoke-viz.R", "tests/testthat/test-rtichoke-viz-vendor.R", ".github/workflows/pkgdown-preview.yaml"]:
    p = Path(path); text = p.read_text()
    text = text.replace("9c5a114ebe968e8cef4d2f14bf82ed552d2c8a17", "3abb3f07a598c3e22d5362a3f88e52bb6b52b083")
    text = text.replace("ab36ae71f9090b4de62da8f552ebe84ac35885ab04958667faaf070db2c98f65", "625613c7f692ff50b7757a27bb6caf84e311971bde92593141393dbd897af3a2")
    p.write_text(text)
p = Path("tools/verify-rtichoke-viz.R"); text = p.read_text()
if 'renderDecisionCurveV2' not in text:
    text = text.replace('  grepl("renderLiftV2", js, fixed = TRUE),\n', '  grepl("renderLiftV2", js, fixed = TRUE),\n  grepl("renderDecisionCurveV2", js, fixed = TRUE),\n')
p.write_text(text)

# Extend the established generic v2 identity builder with Decision Curve data fields.
p = Path("R/rtichoke_viz_v2.R"); text = p.read_text()
text = text.replace('    lift = "renderLiftV2"\n', '    lift = "renderLiftV2",\n    decision_curve = "renderDecisionCurveV2"\n')
text = text.replace('  type = c("roc", "precision_recall", "gains", "lift")\n', '  type = c("roc", "precision_recall", "gains", "lift", "decision_curve")\n')
text = text.replace('    lift = c("probability_threshold", "ppcr", "lift")\n', '    lift = c("probability_threshold", "ppcr", "lift"),\n    decision_curve = c("probability_threshold", "NB")\n')
old = '''    datum <- list(
      seriesId = unname(series_ids[[group]]),
      cutoff = as.numeric(performance_data$probability_threshold[[i]])
    )
    if (type == "roc") {
      datum$sensitivity <- as.numeric(performance_data$sensitivity[[i]])
      datum$specificity <- as.numeric(performance_data$specificity[[i]])
    } else if (type == "precision_recall") {
      datum$sensitivity <- as.numeric(performance_data$sensitivity[[i]])
      datum$ppv <- as.numeric(performance_data$PPV[[i]])
    } else if (type == "gains") {
      datum$sensitivity <- as.numeric(performance_data$sensitivity[[i]])
      datum$ppcr <- as.numeric(performance_data$ppcr[[i]])
    } else if (type == "lift") {
      datum$ppcr <- as.numeric(performance_data$ppcr[[i]])
      datum$lift <- as.numeric(performance_data$lift[[i]])
    }
'''
new = '''    datum <- list(seriesId = unname(series_ids[[group]]))
    if (type == "decision_curve") {
      datum$threshold <- as.numeric(performance_data$probability_threshold[[i]])
      datum$netBenefit <- as.numeric(performance_data$NB[[i]])
    } else {
      datum$cutoff <- as.numeric(performance_data$probability_threshold[[i]])
      if (type == "roc") {
        datum$sensitivity <- as.numeric(performance_data$sensitivity[[i]])
        datum$specificity <- as.numeric(performance_data$specificity[[i]])
      } else if (type == "precision_recall") {
        datum$sensitivity <- as.numeric(performance_data$sensitivity[[i]])
        datum$ppv <- as.numeric(performance_data$PPV[[i]])
      } else if (type == "gains") {
        datum$sensitivity <- as.numeric(performance_data$sensitivity[[i]])
        datum$ppcr <- as.numeric(performance_data$ppcr[[i]])
      } else if (type == "lift") {
        datum$ppcr <- as.numeric(performance_data$ppcr[[i]])
        datum$lift <- as.numeric(performance_data$lift[[i]])
      }
    }
'''
if old not in text: raise SystemExit("v2 data block mismatch")
text = text.replace(old, new, 1)
marker = '  list(\n    schemaVersion = "2.0",\n    type = "lift",\n'
insert = '''  if (type == "decision_curve") {
    return(list(
      schemaVersion = "2.0", type = "decision_curve",
      evaluations = evaluations, series = series, data = data,
      x = "threshold", y = "netBenefit",
      xAxis = list(label = "Probability threshold", domain = c(0, 1)),
      yAxis = list(label = "Net benefit"), references = list()
    ))
  }

'''
if marker not in text: raise SystemExit("lift return marker mismatch")
text = text.replace(marker, insert + marker, 1)
adapter_marker = "v2_population_prevalence <- function(\n"
adapter = '''#' Build a canonical rtichoke_viz v2 conventional Decision Curve specification
#' @inheritParams rtichoke_viz_roc_v2_spec
#' @param min_p_threshold Minimum displayed probability threshold.
#' @param max_p_threshold Maximum displayed probability threshold.
#' @return A canonical Decision Curve v2 specification.
#' @noRd
rtichoke_viz_decision_curve_v2_spec <- function(performance_data, evaluation_metadata, min_p_threshold = 0, max_p_threshold = 1) {
  valid_rows <- is.finite(as.numeric(performance_data$probability_threshold)) & is.finite(as.numeric(performance_data$NB))
  performance_data <- performance_data[valid_rows, , drop = FALSE]
  spec <- rtichoke_viz_curve_v2_spec(performance_data, evaluation_metadata, type = "decision_curve")
  spec$xAxis$domain <- c(min_p_threshold, max_p_threshold)
  populations <- unique(vapply(spec$evaluations, `[[`, character(1), "population"))
  prevalence <- v2_population_prevalence(performance_data, evaluation_metadata, populations)
  compatibility_group <- roc_v2_compatibility_group(performance_data, evaluation_metadata)
  treat_all <- lapply(populations, function(population) {
    groups <- as.character(evaluation_metadata$evaluation[evaluation_metadata$population == population])
    thresholds <- unique(as.numeric(performance_data$probability_threshold[compatibility_group %in% groups]))
    p <- as.numeric(prevalence[[population]])
    list(type = "path", points = lapply(thresholds, function(threshold) list(x = threshold, y = p - (1 - p) * threshold / (1 - threshold))), label = paste0("Treat All — ", population), scope = "population", population = population, benchmark = "treat_all")
  })
  spec$references <- c(list(list(type = "horizontal", value = 0, label = "Treat None", scope = "global", benchmark = "treat_none")), treat_all)
  spec
}

'''
if adapter_marker not in text: raise SystemExit("prevalence marker mismatch")
text = text.replace(adapter_marker, adapter + adapter_marker, 1); p.write_text(text)

# Public API wiring; default semantics remain Plotly/ggplot2 through rtichoke_viz_renderer().
p = Path("R/decision.R"); text = p.read_text()
sig = '  type = "conventional",\n  min_p_threshold = 0,\n  max_p_threshold = 1\n) {'
if text.count(sig) != 2: raise SystemExit(f"decision signatures: {text.count(sig)}")
text = text.replace(sig, '  type = "conventional",\n  min_p_threshold = 0,\n  max_p_threshold = 1,\n  renderer = "default"\n) {', 1)
old_pipe = '''  prepare_performance_data(
    probs = probs,
    reals = reals,
    by = by,
    stratified_by = stratified_by
  ) |>
    plot_decision_curve(
      chosen_threshold = chosen_threshold,
      interactive = interactive,
      color_values = color_values,
      size = size,
      type = type,
      min_p_threshold = min_p_threshold,
      max_p_threshold = max_p_threshold
    )
'''
new_pipe = '''  performance_data <- prepare_performance_data(probs = probs, reals = reals, by = by, stratified_by = stratified_by)
  evaluation_metadata <- if (identical(renderer, "browser")) build_evaluation_metadata(probs, reals)
  plot_decision_curve(performance_data, chosen_threshold = chosen_threshold, interactive = interactive, color_values = color_values, size = size, type = type, min_p_threshold = min_p_threshold, max_p_threshold = max_p_threshold, renderer = renderer, evaluation_metadata = evaluation_metadata)
'''
if old_pipe not in text: raise SystemExit("create pipeline mismatch")
text = text.replace(old_pipe, new_pipe, 1)
if sig not in text: raise SystemExit("plot signature mismatch")
text = text.replace(sig, '  type = "conventional",\n  min_p_threshold = 0,\n  max_p_threshold = 1,\n  renderer = "default",\n  evaluation_metadata = NULL\n) {', 1)
idx = text.index("plot_decision_curve <- function("); before, plot = text[:idx], text[idx:]
guard = '  if (!is.na(chosen_threshold)) {\n    check_chosen_threshold_input(chosen_threshold)\n  }\n\n  if (interactive == FALSE) {'
new_guard = '''  if (!is.na(chosen_threshold)) {
    check_chosen_threshold_input(chosen_threshold)
  }

  renderer <- rtichoke_viz_renderer(renderer, interactive)
  if (renderer == "browser") {
    if (!identical(type, "conventional")) stop("Browser rendering is available only for conventional static Decision Curves", call. = FALSE)
    if (is.null(evaluation_metadata)) stop("Browser rendering requires explicit evaluation_metadata", call. = FALSE)
    return(render_rtichoke_viz_browser(rtichoke_viz_decision_curve_v2_spec(performance_data, evaluation_metadata, min_p_threshold, max_p_threshold)))
  }

  if (renderer == "ggplot2") {'''
if guard not in plot: raise SystemExit("plot guard mismatch")
plot = plot.replace(guard, new_guard, 1).replace('  if (interactive == TRUE) {', '  if (renderer == "plotly") {', 1)
text = before + plot
text = text.replace("#' @param max_p_threshold The maximum Probability Threshold value to be\n#' displayed\n", "#' @param max_p_threshold The maximum Probability Threshold value to be\n#' displayed\n#' @param renderer rendering backend; `\"default\"` preserves `interactive`, with `\"ggplot2\"`, `\"plotly\"`, or `\"browser\"` alternatives.\n#' @param evaluation_metadata semantic metadata required for the precomputed browser path.\n", 1)
p.write_text(text)

# Generated Rd usage/arguments.
p = Path("man/create_decision_curve.Rd"); text = p.read_text().replace('  max_p_threshold = 1\n)', '  max_p_threshold = 1,\n  renderer = "default"\n)', 1)
text = text.replace('\\item{max_p_threshold}{The maximum Probability Threshold value to be\ndisplayed}\n', '\\item{max_p_threshold}{The maximum Probability Threshold value to be\ndisplayed}\n\n\\item{renderer}{rendering backend; \\code{"default"} preserves existing behavior, with \\code{"ggplot2"}, \\code{"plotly"}, and \\code{"browser"} alternatives.}\n'); p.write_text(text)
p = Path("man/plot_decision_curve.Rd"); text = p.read_text().replace('  max_p_threshold = 1\n)', '  max_p_threshold = 1,\n  renderer = "default",\n  evaluation_metadata = NULL\n)', 1)
text = text.replace('\\item{max_p_threshold}{The maximum Probability Threshold value to be\ndisplayed}\n', '\\item{max_p_threshold}{The maximum Probability Threshold value to be\ndisplayed}\n\n\\item{renderer}{rendering backend; \\code{"default"} preserves existing behavior, with \\code{"ggplot2"}, \\code{"plotly"}, and \\code{"browser"} alternatives.}\n\n\\item{evaluation_metadata}{semantic evaluation metadata required when \\code{renderer = "browser"}.}\n'); p.write_text(text)

Path(".github/apply_decision_adoption.py").unlink()
Path(".github/workflows/apply-decision-adoption-temp.yaml").unlink()
