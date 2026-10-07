test_that("create_reals_distribution_times exists and is exported", {
  expect_true(exists("create_reals_distribution_times"))
  expect_true(is.function(create_reals_distribution_times))
})

test_that("emitted spec has correct type, schemaVersion, and structure", {
  times <- c(24.1, 9.7, 49.9, 18.6, 34.8, 14.2, 39.2, 46.0, 31.5, 4.3)
  reals <- c(1, 1, 1, 1, 0, 2, 1, 2, 0, 1)
  fixed_time_horizons <- c(10, 20, 30, 40, 50)

  spec <- rtichoke_viz_outcome_distribution_v2_spec(
    reals,
    times,
    fixed_time_horizons
  )

  expect_equal(spec$schemaVersion, "2.0")
  expect_equal(spec$type, "outcome_distribution")
  expect_equal(spec$title, "Outcome Distribution")
  expect_type(spec$evaluations, "list")
  expect_type(spec$stateDistributions, "list")

  # Check that state items do not contain 'estimate' or 'mass' fields
  for (sd in spec$stateDistributions) {
    expect_equal(sd$estimator, "raw")
    expect_equal(sd$estimateOrigin, "fixed_time_horizon")
    expect_type(sd$states, "list")
    for (st in sd$states) {
      expect_null(st$estimate)
      expect_null(st$mass)
      expect_true(
        st$stateId %in%
          c("real_positive", "real_competing", "real_negative", "real_censored")
      )
      expect_type(st$label, "character")
      expect_type(st$count, "integer")
    }
  }
})

test_that("canonical fixed-horizon counts match expected fixture values exactly", {
  times <- c(24.1, 9.7, 49.9, 18.6, 34.8, 14.2, 39.2, 46.0, 31.5, 4.3)
  reals <- c(1, 1, 1, 1, 0, 2, 1, 2, 0, 1)
  fixed_time_horizons <- c(10, 20, 30, 40, 50)

  spec <- rtichoke_viz_outcome_distribution_v2_spec(
    reals,
    times,
    fixed_time_horizons
  )

  expect_equal(length(spec$stateDistributions), 6) # horizons 0, 10, 20, 30, 40, 50

  expected_counts <- list(
    `0` = c(target = 0, competing = 0, no_target = 10, unknown_excluded = 0),
    `10` = c(target = 2, competing = 0, no_target = 8, unknown_excluded = 0),
    `20` = c(target = 3, competing = 1, no_target = 6, unknown_excluded = 0),
    `30` = c(target = 4, competing = 1, no_target = 5, unknown_excluded = 0),
    `40` = c(target = 5, competing = 1, no_target = 2, unknown_excluded = 2),
    `50` = c(target = 6, competing = 2, no_target = 0, unknown_excluded = 2)
  )

  for (sd in spec$stateDistributions) {
    h_str <- as.character(sd$horizon)
    expect_true(h_str %in% names(expected_counts))

    exp <- expected_counts[[h_str]]

    counts_map <- list()
    for (st in sd$states) {
      counts_map[[st$stateId]] <- st$count
    }

    expect_equal(counts_map[["real_positive"]], unname(exp["target"]))
    expect_equal(counts_map[["real_competing"]], unname(exp["competing"]))
    expect_equal(counts_map[["real_negative"]], unname(exp["no_target"]))
    expect_equal(counts_map[["real_censored"]], unname(exp["unknown_excluded"]))
  }
})

test_that("horizon 0 is automatically prepended, sorted, and deduplicated", {
  times <- c(24.1, 9.7, 49.9, 18.6, 34.8, 14.2, 39.2, 46.0, 31.5, 4.3)
  reals <- c(1, 1, 1, 1, 0, 2, 1, 2, 0, 1)

  # Unsorted horizons with duplicate 0 and duplicates
  fixed_time_horizons <- c(30, 0, 10, 50, 20, 10, 40)

  spec <- rtichoke_viz_outcome_distribution_v2_spec(
    reals,
    times,
    fixed_time_horizons
  )

  horizons <- vapply(spec$stateDistributions, function(x) x$horizon, numeric(1))
  expect_equal(horizons, c(0, 10, 20, 30, 40, 50))
})

test_that("censored-before-horizon is counted as real_censored", {
  times <- c(5, 15)
  reals <- c(0, 1) # First subject censored at time 5 (reals=0)
  fixed_time_horizons <- c(10)

  spec <- rtichoke_viz_outcome_distribution_v2_spec(
    reals,
    times,
    fixed_time_horizons
  )

  # Horizon 10 state distribution
  sd_10 <- spec$stateDistributions[[2]] # index 1 is horizon 0
  expect_equal(sd_10$horizon, 10)

  counts <- list()
  for (st in sd_10$states) {
    counts[[st$stateId]] <- st$count
  }

  expect_equal(counts[["real_censored"]], 1)
  expect_equal(counts[["real_positive"]], 0)
  expect_equal(counts[["real_negative"]], 1)
})

test_that("input validation catches errors clearly", {
  times <- c(24.1, 9.7, 49.9)
  reals <- c(1, 1, 1)

  # Length mismatch
  expect_error(
    rtichoke_viz_outcome_distribution_v2_spec(reals, c(1, 2), 10),
    "identical lengths"
  )

  # Invalid reals values
  expect_error(
    rtichoke_viz_outcome_distribution_v2_spec(c(1, 3, 0), times, 10),
    "must contain only 0, 1, or 2"
  )

  # Negative times
  expect_error(
    rtichoke_viz_outcome_distribution_v2_spec(reals, c(10, -2, 5), 10),
    "must contain finite non-negative numbers"
  )

  # Negative horizons
  expect_error(
    rtichoke_viz_outcome_distribution_v2_spec(reals, times, c(-5, 10)),
    "must contain finite non-negative numbers"
  )

  # Unsupported renderer in public function
  expect_error(
    create_reals_distribution_times(reals, times, 10, renderer = "ggplot2"),
    "must be 'browser'"
  )
})

test_that("multi-evaluation named list inputs work correctly", {
  times_list <- list(
    "Pop A" = c(5, 15),
    "Pop B" = c(20, 25)
  )
  reals_list <- list(
    "Pop A" = c(1, 0),
    "Pop B" = c(2, 1)
  )

  spec <- rtichoke_viz_outcome_distribution_v2_spec(
    reals_list,
    times_list,
    c(10, 30)
  )

  expect_equal(length(spec$evaluations), 2)
  expect_equal(spec$evaluations[[1]]$population, "Pop A")
  expect_equal(spec$evaluations[[2]]$population, "Pop B")

  # 2 evaluations x 3 horizons (0, 10, 30) = 6 state distributions
  expect_equal(length(spec$stateDistributions), 6)
})

test_that("create_reals_distribution_times returns browsable HTML tag object for browser renderer", {
  times <- c(24.1, 9.7, 49.9, 18.6, 34.8, 14.2, 39.2, 46.0, 31.5, 4.3)
  reals <- c(1, 1, 1, 1, 0, 2, 1, 2, 0, 1)
  fixed_time_horizons <- c(10, 20, 30, 40, 50)

  res <- create_reals_distribution_times(
    reals,
    times,
    fixed_time_horizons,
    renderer = "browser"
  )

  expect_s3_class(res, "shiny.tag.list")
})

test_that("existing summary report functions remain unchanged", {
  expect_true(exists("create_summary_report"))
})

test_that("package version is unchanged", {
  pkg_ver <- as.character(utils::packageVersion("rtichoke"))
  expect_equal(pkg_ver, "0.0.7")
})
