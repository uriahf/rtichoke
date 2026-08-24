# fmt: skip file

calibration_v2_fixture <- function(groups = "Model A") {
  deciles <- do.call(
    rbind,
    lapply(seq_along(groups), function(i) {
      data.frame(
        reference_group = groups[[i]],
        x = c(0.2, 0.8),
        y = c(0.25, 0.75) + (i - 1) * 0.01,
        sum_reals = c(5, 15) + i,
        total_obs = c(20, 20)
      )
    })
  )
  smooth <- do.call(
    rbind,
    lapply(seq_along(groups), function(i) {
      data.frame(
        reference_group = groups[[i]],
        x = c(0.1, 0.5, 0.9),
        y = c(0.12, 0.52, 0.88) + (i - 1) * 0.01
      )
    })
  )
  histogram <- do.call(
    rbind,
    lapply(seq_along(groups), function(i) {
      data.frame(
        reference_group = groups[[i]],
        mids = c(0.005, 0.015),
        counts = c(7, 11) + i
      )
    })
  )
  list(
    deciles_dat = deciles,
    smooth_dat = smooth,
    histogram_for_calibration = histogram
  )
}

calibration_v2_metadata <- function(
  groups = "Model A",
  model_known = TRUE,
  shared_population = TRUE
) {
  data.frame(
    model = if (model_known) groups else rep(NA_character_, length(groups)),
    population = if (shared_population) {
      rep("Population A", length(groups))
    } else {
      groups
    },
    evaluation = groups,
    stringsAsFactors = FALSE
  )
}

test_that("calibration v2 producer emits a complete canonical discrete spec", {
  calibration <- calibration_v2_fixture()
  metadata <- calibration_v2_metadata()
  spec <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    calibration,
    metadata,
    "discrete"
  )

  expect_identical(spec$schemaVersion, "2.0")
  expect_identical(spec$type, "calibration")
  expect_identical(spec$x, "predicted")
  expect_identical(spec$y, "observed")
  expect_identical(
    spec$references,
    list(list(
      type = "identity",
      scope = "global"
    ))
  )
  expect_true(all(vapply(spec$data, `[[`, "", "method") == "discrete"))
  expect_true(all(vapply(spec$data, function(x) {
    all(c("events", "total") %in% names(x))
  }, logical(1))))
})

test_that("calibration v2 identities are deterministic", {
  calibration <- calibration_v2_fixture(c("Model A", "Model B"))
  metadata <- calibration_v2_metadata(c("Model A", "Model B"))
  first <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    calibration,
    metadata
  )
  second <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    calibration,
    metadata
  )

  expect_identical(first$evaluations, second$evaluations)
  expect_identical(first$series, second$series)
  expect_identical(
    vapply(first$evaluations, `[[`, "", "id"),
    c("evaluation-1", "evaluation-2")
  )
  expect_identical(
    vapply(first$series, `[[`, "", "id"),
    c("series-1", "series-2")
  )
})

test_that("one model and multiple models preserve semantic metadata", {
  one <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    calibration_v2_fixture("Model A"),
    calibration_v2_metadata("Model A")
  )
  expect_identical(one$evaluations[[1]]$model, "Model A")
  expect_identical(one$evaluations[[1]]$population, "Population A")

  groups <- c("Model A", "Model B")
  several <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    calibration_v2_fixture(groups),
    calibration_v2_metadata(groups)
  )
  expect_identical(
    vapply(several$evaluations, `[[`, "", "model"),
    groups
  )
  expect_identical(
    vapply(several$evaluations, `[[`, "", "population"),
    rep("Population A", 2)
  )
})

test_that("multiple populations and model-unknown semantics are preserved", {
  groups <- c("Train", "Test")
  spec <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    calibration_v2_fixture(groups),
    calibration_v2_metadata(
      groups,
      model_known = FALSE,
      shared_population = FALSE
    )
  )

  expect_identical(
    vapply(spec$evaluations, `[[`, "", "population"),
    groups
  )
  expect_true(all(vapply(spec$evaluations, function(x) {
    !"model" %in% names(x)
  }, logical(1))))
  expect_identical(
    vapply(spec$series, function(x) x$display$role, ""),
    rep("population", 2)
  )
})

test_that("distribution rows retain series ownership", {
  groups <- c("Model A", "Model B")
  spec <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    calibration_v2_fixture(groups),
    calibration_v2_metadata(groups)
  )

  expect_identical(
    vapply(spec$distribution, `[[`, "", "seriesId"),
    rep(c("series-1", "series-2"), each = 2)
  )
  expect_true(all(
    vapply(spec$distribution, `[[`, "", "seriesId") %in%
      vapply(spec$series, `[[`, "", "id")
  ))
})

test_that("calibration statistics are passed through without recomputation", {
  calibration <- calibration_v2_fixture()
  calibration$deciles_dat$x <- c(0.123456, 0.876543)
  calibration$deciles_dat$y <- c(0.234567, 0.765432)
  calibration$deciles_dat$sum_reals <- c(3, 17)
  calibration$deciles_dat$total_obs <- c(19, 23)

  spec <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    calibration,
    calibration_v2_metadata()
  )

  expect_identical(
    vapply(spec$data, `[[`, 0, "predicted"),
    calibration$deciles_dat$x
  )
  expect_identical(
    vapply(spec$data, `[[`, 0, "observed"),
    calibration$deciles_dat$y
  )
  expect_identical(
    vapply(spec$data, `[[`, 0, "events"),
    as.numeric(calibration$deciles_dat$sum_reals)
  )
  expect_identical(
    vapply(spec$data, `[[`, 0, "total"),
    as.numeric(calibration$deciles_dat$total_obs)
  )
})

test_that("smooth calibration uses existing smooth output", {
  calibration <- calibration_v2_fixture()
  spec <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    calibration,
    calibration_v2_metadata(),
    "smooth"
  )

  expect_true(all(vapply(spec$data, `[[`, "", "method") == "smooth"))
  expect_identical(
    vapply(spec$data, `[[`, 0, "predicted"),
    calibration$smooth_dat$x
  )
  expect_identical(
    vapply(spec$data, `[[`, 0, "observed"),
    calibration$smooth_dat$y
  )
  expect_true(all(vapply(spec$data, function(x) {
    !any(c("events", "total") %in% names(x))
  }, logical(1))))
})

test_that("ReportSpec embeds produced calibration unchanged", {
  spec <- rtichoke:::rtichoke_viz_calibration_v2_spec(
    calibration_v2_fixture(),
    calibration_v2_metadata()
  )
  report <- rtichoke:::rtichoke_viz_report_spec(spec)

  expect_identical(report$components[[1]]$id, "calibration")
  expect_identical(report$components[[1]]$spec, spec)
})

test_that("existing v1 calibration producer remains unchanged", {
  calibration <- calibration_v2_fixture()
  v1 <- rtichoke:::rtichoke_viz_calibration_spec(calibration)

  expect_identical(v1$schemaVersion, "1.0")
  expect_identical(v1$type, "calibration")
  expect_true(all(vapply(v1$data, `[[`, "", "method") == "discrete"))
  expect_identical(v1$references, list(list(type = "identity")))
})
