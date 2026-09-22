test_that("interactive discrete calibration y-axis has equal unit scale constraint", {
  cal_list <- create_calibration_curve_list(
    probs = list("Model A" = example_dat$estimated_probabilities),
    reals = list(example_dat$outcome)
  )

  p <- create_plotly_curve_from_calibration_curve_list(
    cal_list,
    type = "discrete"
  )
  built <- plotly::plotly_build(p)

  layout <- built$x$layout

  expect_equal(layout$yaxis$scaleanchor, "x")
  expect_equal(layout$yaxis$scaleratio, 1)
  expect_equal(layout$yaxis$constrain, "domain")

  expect_equal(layout$xaxis$range, cal_list$axes_ranges$xaxis)
  expect_equal(layout$yaxis$range, cal_list$axes_ranges$yaxis)
  expect_equal(layout$xaxis$range, layout$yaxis$range)

  expect_null(layout$yaxis2$scaleanchor)
  expect_null(layout$yaxis2$scaleratio)
  expect_null(layout$yaxis2$constrain)
})

test_that("interactive smooth calibration y-axis has equal unit scale constraint", {
  cal_list <- create_calibration_curve_list(
    probs = list("Model A" = example_dat$estimated_probabilities),
    reals = list(example_dat$outcome)
  )

  p <- create_plotly_curve_from_calibration_curve_list(
    cal_list,
    type = "smooth"
  )
  built <- plotly::plotly_build(p)

  layout <- built$x$layout

  expect_equal(layout$yaxis$scaleanchor, "x")
  expect_equal(layout$yaxis$scaleratio, 1)
  expect_equal(layout$yaxis$constrain, "domain")

  expect_equal(layout$xaxis$range, cal_list$axes_ranges$xaxis)
  expect_equal(layout$yaxis$range, cal_list$axes_ranges$yaxis)
  expect_equal(layout$xaxis$range, layout$yaxis$range)

  expect_null(layout$yaxis2$scaleanchor)
  expect_null(layout$yaxis2$scaleratio)
  expect_null(layout$yaxis2$constrain)
})
