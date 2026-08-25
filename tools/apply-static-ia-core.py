from pathlib import Path


def unique(text, needle, label):
    count = text.count(needle)
    if count != 1:
        raise SystemExit(f"{label}: expected one match, found {count}")


# Keep the established IA formula in one production helper shared by Plotly
# preparation and the new canonical browser path.
p = Path("R/helpers.R")
h = p.read_text()
marker = "prepare_performance_data_for_curve <- function(\n"
unique(h, marker, "helpers insertion marker")
helper = '''add_static_interventions_avoided_metric <- function(performance_data) {
  performance_data |>
    dplyr::mutate(
      N = TP + TN + FP + FN,
      prevalence = (TP + FN) / N,
      NB_intervention_all = prevalence -
        (1 - prevalence) *
          (probability_threshold) /
          (1 - probability_threshold),
      NB_interventions_avoided = 100 *
        (NB - NB_intervention_all) *
        ((1 - probability_threshold) / probability_threshold)
    )
}


'''
h = h.replace(marker, helper + marker, 1)
start = h.index('      if (y_performance_metric == "NB_interventions_avoided") {')
else_pos = h.index('      } else {', start)
h = h[:start] + '''      if (y_performance_metric == "NB_interventions_avoided") {
        add_static_interventions_avoided_metric(data)
''' + h[else_pos:]
p.write_text(h)

# Extend the existing v2 identity/semantic machinery rather than adding a
# separate IA identity path.
p = Path("R/rtichoke_viz_v2.R")
v = p.read_text()
needle = '    decision_curve = "renderDecisionCurveV2"\n  )'
unique(v, needle, "renderer map")
v = v.replace(
    needle,
    '    decision_curve = "renderDecisionCurveV2",\n'
    '    interventions_avoided = "renderInterventionsAvoidedV2"\n  )',
    1,
)
v = v.replace('version = "0.6.0"', 'version = "0.7.0"')
v = v.replace(
    "rtichoke-viz-0.6.0/rtichoke-viz.js",
    "rtichoke-viz-0.7.0/rtichoke-viz.js",
)

marker = "v2_population_prevalence <- function(\n"
unique(v, marker, "population prevalence marker")
ia_builder = '''#' Build a canonical rtichoke_viz v2 Interventions Avoided specification
#' @inheritParams rtichoke_viz_decision_curve_v2_spec
#' @return A canonical Interventions Avoided v2 specification.
#' @noRd
rtichoke_viz_interventions_avoided_v2_spec <- function(
  performance_data,
  evaluation_metadata,
  min_p_threshold = 0,
  max_p_threshold = 1
) {
  required <- c("probability_threshold", "NB_interventions_avoided")
  missing <- setdiff(required, names(performance_data))
  if (length(missing) > 0L) {
    stop(
      "INTERVENTIONS_AVOIDED performance data is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  prevalence_data <- performance_data
  valid_rows <- is.finite(as.numeric(performance_data$probability_threshold)) &
    is.finite(as.numeric(performance_data$NB_interventions_avoided))
  performance_data <- performance_data[valid_rows, , drop = FALSE]
  spec <- rtichoke_viz_curve_v2_spec(
    performance_data,
    evaluation_metadata,
    type = "interventions_avoided"
  )
  spec$xAxis$domain <- c(min_p_threshold, max_p_threshold)

  populations <- unique(vapply(
    spec$evaluations,
    `[[`,
    character(1),
    "population"
  ))
  prevalence <- v2_population_prevalence(
    prevalence_data,
    evaluation_metadata,
    populations
  )
  compatibility_group <- roc_v2_compatibility_group(
    performance_data,
    evaluation_metadata
  )
  treat_none <- lapply(populations, function(population) {
    groups <- as.character(evaluation_metadata$evaluation[
      evaluation_metadata$population == population
    ])
    thresholds <- unique(as.numeric(performance_data$probability_threshold[
      compatibility_group %in% groups
    ]))
    p <- as.numeric(prevalence[[population]])
    list(
      type = "path",
      points = lapply(thresholds, function(threshold) {
        list(
          x = threshold,
          y = 100 * (1 - p - p * (1 - threshold) / threshold)
        )
      }),
      label = paste0("Treat None \\u2014 ", population),
      scope = "population",
      population = population,
      benchmark = "treat_none"
    )
  })
  spec$references <- c(
    list(list(
      type = "horizontal",
      value = 0,
      label = "Treat All",
      scope = "global",
      benchmark = "treat_all"
    )),
    treat_none
  )
  spec
}

'''
v = v.replace(marker, ia_builder + marker, 1)

needle = '  type = c("roc", "precision_recall", "gains", "lift", "decision_curve")'
unique(v, needle, "v2 type list")
v = v.replace(
    needle,
    '  type = c(\n'
    '    "roc", "precision_recall", "gains", "lift", "decision_curve",\n'
    '    "interventions_avoided"\n'
    '  )',
    1,
)
needle = '    decision_curve = c("probability_threshold", "NB")\n  )'
unique(v, needle, "required columns")
v = v.replace(
    needle,
    '    decision_curve = c("probability_threshold", "NB"),\n'
    '    interventions_avoided = c(\n'
    '      "probability_threshold", "NB_interventions_avoided"\n'
    '    )\n'
    '  )',
    1,
)
needle = '''    if (type == "decision_curve") {
      datum$threshold <- as.numeric(performance_data$probability_threshold[[i]])
      datum$netBenefit <- as.numeric(performance_data$NB[[i]])
    } else {
'''
unique(v, needle, "datum mapping")
v = v.replace(
    needle,
    '''    if (type == "decision_curve") {
      datum$threshold <- as.numeric(performance_data$probability_threshold[[i]])
      datum$netBenefit <- as.numeric(performance_data$NB[[i]])
    } else if (type == "interventions_avoided") {
      datum$threshold <- as.numeric(performance_data$probability_threshold[[i]])
      datum$interventionsAvoided <- as.numeric(
        performance_data$NB_interventions_avoided[[i]]
      )
    } else {
''',
    1,
)
needle = '''  list(
    schemaVersion = "2.0",
    type = "lift",
'''
unique(v, needle, "generic lift return")
v = v.replace(
    needle,
    '''  if (type == "interventions_avoided") {
    return(list(
      schemaVersion = "2.0",
      type = "interventions_avoided",
      evaluations = evaluations,
      series = series,
      data = data,
      x = "threshold",
      y = "interventionsAvoided",
      xAxis = list(label = "Probability Threshold", domain = c(0, 1)),
      yAxis = list(label = "Interventions Avoided (per 100)"),
      references = list()
    ))
  }

  list(
    schemaVersion = "2.0",
    type = "lift",
''',
    1,
)
p.write_text(v)

# Public browser dispatch: standalone conventional and standalone IA only.
p = Path("R/decision.R")
d = p.read_text()
start = d.index('  if (renderer == "browser") {')
end = d.index('  if (renderer == "ggplot2") {', start)
new_browser = '''  if (renderer == "browser") {
    if (identical(type, "combined")) {
      stop(
        "Browser rendering is not available for combined static Decision Curves",
        call. = FALSE
      )
    }
    if (is.null(evaluation_metadata)) {
      stop(
        "Browser rendering requires explicit evaluation_metadata",
        call. = FALSE
      )
    }
    spec <- if (identical(type, "conventional")) {
      rtichoke_viz_decision_curve_v2_spec(
        performance_data,
        evaluation_metadata,
        min_p_threshold,
        max_p_threshold
      )
    } else {
      ia_performance_data <- add_static_interventions_avoided_metric(
        performance_data
      )
      rtichoke_viz_interventions_avoided_v2_spec(
        ia_performance_data,
        evaluation_metadata,
        min_p_threshold,
        max_p_threshold
      )
    }
    return(render_rtichoke_viz_browser(spec))
  }

'''
p.write_text(d[:start] + new_browser + d[end:])

# Existing Decision Curve regression expectations now point at the upgraded
# shared bundle; combined remains intentionally unsupported in browser mode.
p = Path("tests/testthat/test-decision-browser-v2.R")
t = p.read_text()
t = t.replace("shared v0.6.0 renderer", "shared v0.7.0 renderer")
t = t.replace("rtichoke-viz-0.6.0/rtichoke-viz.js", "rtichoke-viz-0.7.0/rtichoke-viz.js")
t = t.replace('type = "interventions avoided",', 'type = "combined",', 1)
t = t.replace(
    '"only for conventional static Decision Curves"',
    '"not available for combined static Decision Curves"',
    1,
)
p.write_text(t)

# Provenance verifier follows the exact independently verified release and
# additionally proves the packaged IA public surface remains present.
p = Path("tools/verify-rtichoke-viz.R")
r = p.read_text()
r = r.replace("0.6.0", "0.7.0")
r = r.replace(
    "625613c7f692ff50b7757a27bb6caf84e311971bde92593141393dbd897af3a2",
    "f09c30e231a8be39c2e89ba6ae39c90ed8cab67021213e17681a475066a9806e",
)
r = r.replace(
    "3abb3f07a598c3e22d5362a3f88e52bb6b52b083",
    "b3564d2824ec1791f791fda406c99b3d7865a68f",
)
needle = '  grepl("renderDecisionCurveV2", js, fixed = TRUE),\n'
unique(r, needle, "verifier renderer assertion")
r = r.replace(
    needle,
    needle
    + '  grepl("InterventionsAvoidedV2SpecSchema", js, fixed = TRUE),\n'
    + '  grepl("renderInterventionsAvoidedV2", js, fixed = TRUE),\n',
    1,
)
p.write_text(r)
