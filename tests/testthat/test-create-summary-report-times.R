test_that("create_summary_report_times public API exists and is exported", {
  expect_true(exists("create_summary_report_times", mode = "function"))
})

test_that("create_summary_report_times has expected argument names and defaults", {
  arg_names <- names(formals(create_summary_report_times))
  expect_true("reals" %in% arg_names)
  expect_true("times" %in% arg_names)
  expect_true("fixed_time_horizons" %in% arg_names)
  expect_true("output_file" %in% arg_names)
  expect_true("output_dir" %in% arg_names)
  expect_true("renderer" %in% arg_names)

  defaults <- formals(create_summary_report_times)
  expect_equal(defaults$output_file, "summary_report_times.html")
  expect_equal(defaults$renderer, "browser")
  expect_equal(defaults$heuristics, FALSE)
})

test_that("create_summary_report_times validates heuristics parameter strictly", {
  times <- c(24.1, 9.7, 49.9)
  reals <- c(1, 1, 1)
  fixed_time_horizons <- c(10)

  expect_error(
    create_summary_report_times(
      reals = reals,
      times = times,
      fixed_time_horizons = fixed_time_horizons,
      heuristics = "yes"
    ),
    "`heuristics` must be a single non-missing logical value.",
    fixed = TRUE
  )

  expect_error(
    create_summary_report_times(
      reals = reals,
      times = times,
      fixed_time_horizons = fixed_time_horizons,
      heuristics = NA
    ),
    "`heuristics` must be a single non-missing logical value.",
    fixed = TRUE
  )

  expect_error(
    create_summary_report_times(
      reals = reals,
      times = times,
      fixed_time_horizons = fixed_time_horizons,
      heuristics = c(TRUE, FALSE)
    ),
    "`heuristics` must be a single non-missing logical value.",
    fixed = TRUE
  )
})

test_that("create_summary_report_times strictly validates renderer = 'browser'", {
  times <- c(24.1, 9.7, 49.9)
  reals <- c(1, 1, 1)
  fixed_time_horizons <- c(10)

  expect_error(
    create_summary_report_times(
      reals = reals,
      times = times,
      fixed_time_horizons = fixed_time_horizons,
      renderer = "rmarkdown"
    ),
    'Only `renderer = "browser"` is supported for `create_summary_report_times()`.',
    fixed = TRUE
  )

  expect_error(
    create_summary_report_times(
      reals = reals,
      times = times,
      fixed_time_horizons = fixed_time_horizons,
      renderer = "plotly"
    ),
    'Only `renderer = "browser"` is supported for `create_summary_report_times()`.',
    fixed = TRUE
  )
})

test_that("create_summary_report_times renders self-contained HTML report with single OutcomeDistribution component", {
  times <- c(24.1, 9.7, 49.9, 18.6, 34.8, 14.2, 39.2, 46.0, 31.5, 4.3)
  reals <- c(1, 1, 1, 1, 0, 2, 1, 2, 0, 1)
  fixed_time_horizons <- c(10, 20, 30, 40, 50)

  temp_dir <- tempfile("time_report_test_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  out_filename <- "my_time_report.html"

  expect_output(
    create_summary_report_times(
      reals = reals,
      times = times,
      fixed_time_horizons = fixed_time_horizons,
      output_file = out_filename,
      output_dir = temp_dir,
      renderer = "browser"
    ),
    "my_time_report.html was rendered"
  )

  expected_path <- file.path(temp_dir, out_filename)
  expect_true(file.exists(expected_path))

  html_content <- paste(readLines(expected_path, warn = FALSE), collapse = "\n")

  # Report hierarchy check
  expect_match(html_content, "Time-dependent Summary Report", fixed = TRUE)
  expect_match(html_content, "Time-dependent outcomes", fixed = TRUE)
  expect_match(html_content, "Outcome Distribution", fixed = TRUE)

  # Check embedded spec details
  expect_match(html_content, '"type":"outcome_distribution"', fixed = TRUE)
  expect_match(html_content, '"schemaVersion":"1.1"', fixed = TRUE)
  expect_match(
    html_content,
    '"title":"Time-dependent Summary Report"',
    fixed = TRUE
  )

  # Verify explanatory note is included
  expect_match(html_content, "Outcome Distribution Note", fixed = TRUE)
  expect_match(
    html_content,
    "Outcome Distribution shows raw observed outcome status over time.",
    fixed = TRUE
  )
  expect_match(
    html_content,
    "no Aalen-Johansen, IPCW, or other censoring adjustment is applied.",
    fixed = TRUE
  )

  # Verify binary performance cheat sheet is NOT present
  expect_false(
    grepl("Performance Metrics Cheat Sheet", html_content, fixed = TRUE)
  )
  expect_false(
    grepl("rtichoke-cheat-sheet__metrics", html_content, fixed = TRUE)
  )
})

test_that("create_summary_report_times propagates fixed time horizons correctly", {
  times <- c(10, 20, 30)
  reals <- c(1, 0, 2)
  fixed_time_horizons <- c(15, 25)

  temp_dir <- tempfile("time_report_test_horizons_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  out_filename <- "horizons_report.html"

  create_summary_report_times(
    reals = reals,
    times = times,
    fixed_time_horizons = fixed_time_horizons,
    output_file = out_filename,
    output_dir = temp_dir
  )

  html_content <- paste(
    readLines(file.path(temp_dir, out_filename), warn = FALSE),
    collapse = "\n"
  )

  # Extract embedded JSON spec from <script id="...-spec" type="application/json">
  json_match <- regmatches(
    html_content,
    regexec(
      '<script id="[^"]*-spec" type="application/json">\\s*(\\{.*?\\})\\s*</script>',
      html_content
    )
  )[[1]]

  expect_true(length(json_match) >= 2)
  spec_json <- jsonlite::fromJSON(json_match[2], simplifyVector = FALSE)

  expect_equal(spec_json$schemaVersion, "1.1")
  expect_equal(spec_json$type, "report")
  expect_equal(spec_json$title, "Time-dependent Summary Report")

  # Ensure exactly 1 section
  expect_equal(length(spec_json$sections), 1)
  section <- spec_json$sections[[1]]
  expect_equal(section$id, "time-dependent-outcomes")
  expect_equal(section$title, "Time-dependent outcomes")

  # Ensure section contains exactly 1 item (component)
  expect_equal(length(section$items), 1)
  item <- section$items[[1]]
  expect_equal(item$type, "component")
  expect_equal(item$id, "outcome-distribution")
  expect_equal(item$title, "Outcome Distribution")

  component_spec <- item$spec
  expect_equal(component_spec$type, "outcome_distribution")
  expect_equal(component_spec$schemaVersion, "2.0")

  # Verify horizons 0, 15, 25 are present in fixed_time_horizon stateDistributions
  fixed_sds <- Filter(
    function(x) identical(x$estimateOrigin, "fixed_time_horizon"),
    component_spec$stateDistributions
  )
  horizons <- vapply(fixed_sds, function(x) x$horizon, numeric(1))
  expect_equal(horizons, c(0, 15, 25))
})

test_that("create_summary_report_times with heuristics = TRUE serializes 8 variants and report heuristic controls", {
  times <- c(24.1, 9.7, 49.9, 18.6, 34.8, 14.2, 39.2, 46.0, 31.5, 4.3)
  reals <- c(1, 1, 1, 1, 0, 2, 1, 2, 0, 1)
  fixed_time_horizons <- c(10, 20, 30, 40, 50)

  temp_dir <- tempfile("time_report_heuristics_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  out_filename <- "heuristics_report.html"

  expect_output(
    create_summary_report_times(
      reals = reals,
      times = times,
      fixed_time_horizons = fixed_time_horizons,
      output_file = out_filename,
      output_dir = temp_dir,
      renderer = "browser",
      heuristics = TRUE
    ),
    "heuristics_report.html was rendered"
  )

  expected_path <- file.path(temp_dir, out_filename)
  expect_true(file.exists(expected_path))

  html_content <- paste(readLines(expected_path, warn = FALSE), collapse = "\n")

  # Extract embedded JSON spec
  json_match <- regmatches(
    html_content,
    regexec(
      '<script id="[^"]*-spec" type="application/json">\\s*(\\{.*?\\})\\s*</script>',
      html_content
    )
  )[[1]]

  expect_true(length(json_match) >= 2)
  spec_json <- jsonlite::fromJSON(json_match[2], simplifyVector = FALSE)

  # Check report-level heuristics metadata
  expect_equal(spec_json$schemaVersion, "1.1")
  expect_false(is.null(spec_json$heuristics))
  expect_equal(spec_json$heuristics$active, "adjusted:adjusted_as_negative")
  expect_equal(length(spec_json$heuristics$available), 8)

  expected_pairs <- c(
    "excluded:excluded",
    "excluded:adjusted_as_censored",
    "excluded:adjusted_as_negative",
    "excluded:adjusted_as_composite",
    "adjusted:excluded",
    "adjusted:adjusted_as_censored",
    "adjusted:adjusted_as_negative",
    "adjusted:adjusted_as_composite"
  )
  expect_setequal(unlist(spec_json$heuristics$available), expected_pairs)

  # Check component-level variants
  item <- spec_json$sections[[1]]$items[[1]]
  component_spec <- item$spec
  expect_equal(component_spec$type, "outcome_distribution")
  expect_equal(component_spec$schemaVersion, "2.0")
  expect_equal(length(component_spec$variants), 8)
  expect_setequal(names(component_spec$variants), expected_pairs)

  # Check evaluations
  expect_equal(length(component_spec$evaluations), 1)
  expect_equal(component_spec$evaluations[[1]]$id, "evaluation-1")
  expect_equal(component_spec$evaluations[[1]]$population, "evaluation")

  # Check heuristic explanatory note
  expect_match(html_content, "Outcome Distribution Note", fixed = TRUE)
  expect_match(
    html_content,
    "Outcome estimates reflect the selected treatment of censoring and competing events.",
    fixed = TRUE
  )
  expect_match(
    html_content,
    "Exclusions define a retained population separately at each horizon; exclusion percentages use the original population size.",
    fixed = TRUE
  )
  expect_match(
    html_content,
    "When exclusions are selected, estimates describe that retained population.",
    fixed = TRUE
  )
  expect_match(
    html_content,
    "Empty retained populations have unavailable estimates, not zero probabilities.",
    fixed = TRUE
  )
  expect_match(
    html_content,
    "Beyond observed follow-up support, displayed values retain the estimator\u2019s last plateau and do not establish subsequent outcomes.",
    fixed = TRUE
  )
  expect_match(
    html_content,
    "Censoring-adjusted estimates rely on the relevant independent-censoring assumptions.",
    fixed = TRUE
  )

  # Ensure raw-mode specific phrases are NOT in the heuristic report
  expect_false(
    grepl(
      "Outcome Distribution shows raw observed outcome status over time.",
      html_content,
      fixed = TRUE
    )
  )
  expect_false(
    grepl(
      "no Aalen-Johansen, IPCW, or other censoring adjustment is applied.",
      html_content,
      fixed = TRUE
    )
  )

  # Validate against vendored JS schema and referential integrity
  vendor_js <- resolve_rtichoke_viz_js()
  tf <- tempfile(fileext = ".json")
  writeLines(
    jsonlite::toJSON(spec_json, auto_unbox = TRUE, null = "null", digits = 16),
    tf
  )
  script_file <- tempfile(fileext = ".mjs")
  writeLines(
    sprintf(
      '
import * as fs from "fs";
import * as viz from %s;

const spec = JSON.parse(fs.readFileSync(%s, "utf8"));
viz.assertReportReferentialIntegrity(spec);
viz.assertOutcomeDistributionReferentialIntegrity(spec.sections[0].items[0].spec);
console.log("VALID");
',
      jsonlite::toJSON(vendor_js, auto_unbox = TRUE),
      jsonlite::toJSON(tf, auto_unbox = TRUE)
    ),
    script_file
  )
  on.exit(unlink(c(tf, script_file)), add = TRUE)

  res <- system2("node", args = c(script_file), stdout = TRUE, stderr = TRUE)
  expect_equal(tail(res, 1), "VALID")
})

test_that("create_summary_report_times with multiple populations preserves distinct identities and ordering", {
  times1 <- c(10, 20, 30)
  reals1 <- c(1, 0, 2)
  times2 <- c(15, 25, 35)
  reals2 <- c(0, 1, 1)

  temp_dir <- tempfile("time_report_multi_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  out_filename <- "multi_pop_heuristics.html"

  create_summary_report_times(
    reals = list("Cohort A" = reals1, "Cohort B" = reals2),
    times = list("Cohort A" = times1, "Cohort B" = times2),
    fixed_time_horizons = c(12, 22),
    output_file = out_filename,
    output_dir = temp_dir,
    heuristics = TRUE
  )

  html_content <- paste(
    readLines(file.path(temp_dir, out_filename), warn = FALSE),
    collapse = "\n"
  )

  json_match <- regmatches(
    html_content,
    regexec(
      '<script id="[^"]*-spec" type="application/json">\\s*(\\{.*?\\})\\s*</script>',
      html_content
    )
  )[[1]]

  spec_json <- jsonlite::fromJSON(json_match[2], simplifyVector = FALSE)
  od_spec <- spec_json$sections[[1]]$items[[1]]$spec

  expect_equal(length(od_spec$evaluations), 2)
  expect_equal(od_spec$evaluations[[1]]$id, "evaluation-1")
  expect_equal(od_spec$evaluations[[1]]$population, "Cohort A")
  expect_equal(od_spec$evaluations[[2]]$id, "evaluation-2")
  expect_equal(od_spec$evaluations[[2]]$population, "Cohort B")

  # Validate against vendored JS schema and referential integrity
  vendor_js <- resolve_rtichoke_viz_js()
  tf <- tempfile(fileext = ".json")
  writeLines(
    jsonlite::toJSON(spec_json, auto_unbox = TRUE, null = "null", digits = 16),
    tf
  )
  script_file <- tempfile(fileext = ".mjs")
  writeLines(
    sprintf(
      '
import * as fs from "fs";
import * as viz from %s;

const spec = JSON.parse(fs.readFileSync(%s, "utf8"));
viz.assertReportReferentialIntegrity(spec);
viz.assertOutcomeDistributionReferentialIntegrity(spec.sections[0].items[0].spec);
console.log("MULTI_VALID");
',
      jsonlite::toJSON(vendor_js, auto_unbox = TRUE),
      jsonlite::toJSON(tf, auto_unbox = TRUE)
    ),
    script_file
  )
  on.exit(unlink(c(tf, script_file)), add = TRUE)

  res <- system2("node", args = c(script_file), stdout = TRUE, stderr = TRUE)
  expect_equal(tail(res, 1), "MULTI_VALID")
})

test_that("create_summary_report_times with multiple unnamed populations assigns deterministic names", {
  times1 <- c(10, 20)
  reals1 <- c(1, 0)
  times2 <- c(15, 25)
  reals2 <- c(0, 1)

  temp_dir <- tempfile("time_report_unnamed_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  out_filename <- "unnamed_pop_heuristics.html"

  create_summary_report_times(
    reals = list(reals1, reals2),
    times = list(times1, times2),
    fixed_time_horizons = c(12),
    output_file = out_filename,
    output_dir = temp_dir,
    heuristics = TRUE
  )

  html_content <- paste(
    readLines(file.path(temp_dir, out_filename), warn = FALSE),
    collapse = "\n"
  )

  json_match <- regmatches(
    html_content,
    regexec(
      '<script id="[^"]*-spec" type="application/json">\\s*(\\{.*?\\})\\s*</script>',
      html_content
    )
  )[[1]]

  spec_json <- jsonlite::fromJSON(json_match[2], simplifyVector = FALSE)
  od_spec <- spec_json$sections[[1]]$items[[1]]$spec

  expect_equal(length(od_spec$evaluations), 2)
  expect_equal(od_spec$evaluations[[1]]$id, "evaluation-1")
  expect_equal(od_spec$evaluations[[1]]$population, "evaluation-1")
  expect_equal(od_spec$evaluations[[2]]$id, "evaluation-2")
  expect_equal(od_spec$evaluations[[2]]$population, "evaluation-2")
})
