#' inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_inputs_ui <- function(id){
  ns <- NS(id)
  # tagList(
  column(
    shinyWidgets::awesomeRadio(
      inputId = NS(id, "main_slider"),
      label = "Main Slider",
      choices = c(
        "Threshold" = "threshold",
        "Percent of Predicted Positives" = "percentpositives"
      ),
      inline = T
    ),
    shinyWidgets::checkboxGroupButtons(
      inputId = NS(id, "model_picker"),
      label = "Choose a Model",
      choices = c("First Model", "Second Model") #unique(perf_dat_percent_positives$model)
    ),
    shinyWidgets::sliderTextInput(
      NS(id, "cutoff"),
      label = "Choose a Cutoff",
      choices = seq(0, 1, by = 0.01)
    ),
    # div(reactableOutput(NS(id, "performance_metrics_and_conf")), style = "font-size:80%"),
    width = 4
  )
  # )
}
    
#' inputs Server Functions
#'
#' @noRd 
mod_inputs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_inputs_ui("inputs_ui_1")
    
## To be copied in the server
# mod_inputs_server("inputs_ui_1")
