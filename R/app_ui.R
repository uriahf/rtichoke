#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  rtichoke_theme <- bslib::bs_theme(
    bg = "#fdf9f1", fg = "#141516", primary = "#7f687c",
    base_font = bslib::font_google("Roboto"),
    heading_font = bslib::font_google("Alegreya"),
    "font-size-base" = "1.0rem"
  )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # fluidPage(
    #   h1("rtichoke")
    # )
    navbarPage(
      theme = rtichoke_theme,
      "rtichoke",
      tabPanel(
        "Performance by Cutoff",
        fillPage(
          fluidRow(
            # setBackgroundColor("#fdf9f1"),
            # conf_ui_1("hist1"),
            mod_inputs_ui("inputs_ui_1")
            # conf_ui_2("roc"),
            # conf_ui_3("probs")
          )
        )
      ),
      tabPanel("Calibration")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "rtichoke"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
