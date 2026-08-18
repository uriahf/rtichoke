# Keep the upper interactive calibration panel on a 1:1 predicted/observed scale.
#
# The histogram is a separate lower subplot and intentionally remains
# unconstrained.  This wrapper is loaded after calibration.R so the plotting
# implementation itself stays unchanged apart from the final Plotly layout.

.create_plotly_curve_from_calibration_curve_list_unconstrained <-
  create_plotly_curve_from_calibration_curve_list

create_plotly_curve_from_calibration_curve_list <- function(calibration_curve_list,
                                                             type = "discrete") {
  calibration_curve <-
    .create_plotly_curve_from_calibration_curve_list_unconstrained(
      calibration_curve_list,
      type = type
    )

  calibration_curve |>
    plotly::layout(
      yaxis = list(
        title = "Observed",
        range = calibration_curve_list$axes_ranges$yaxis,
        showgrid = FALSE,
        scaleanchor = "x",
        scaleratio = 1,
        constrain = "domain"
      )
    )
}
