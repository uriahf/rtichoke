test_that("interactive calibration main panel is square", {
  probs <- list(model = seq(0.05, 0.95, length.out = 20))
  reals <- list(rep(c(0, 1), 10))

  for (type in c("discrete", "smooth")) {
    fig <- create_calibration_curve(
      probs = probs,
      reals = reals,
      type = type,
      interactive = TRUE
    )

    built <- plotly::plotly_build(fig)

    expect_equal(built$x$layout$xaxis$range, built$x$layout$yaxis$range)
    expect_equal(built$x$layout$yaxis$scaleanchor, "x")
    expect_equal(built$x$layout$yaxis$scaleratio, 1)
    expect_equal(built$x$layout$yaxis$constrain, "domain")

    # The histogram uses yaxis2 and is intentionally not square-constrained.
    expect_null(built$x$layout$yaxis2$scaleanchor)
  }
})
