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
  expect_match(html_content, '"title":"Time-dependent Summary Report"', fixed = TRUE)

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
