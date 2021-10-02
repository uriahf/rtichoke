library(magrittr)
library(dplyr)




col_values = c("#5E7F9A", 
               "#931B53", 
               "#F7DC2E", 
               "#C6C174", 
               "#75DBCD")
  

print(performance_data_type)
print(col_values_vec)

prevalence <- get_prevalence_from_performance_data(rtichoke::train_and_test_sets)
reference_lines_roc <- create_reference_lines_data_frame("roc", plotly = T)
reference_lines_lift <- create_reference_lines_data_frame("lift", plotly = T)
reference_lines_precision_recall <- create_reference_lines_data_frame("precision recall", prevalence, plotly = T)
reference_lines_gains <- create_reference_lines_data_frame("gains", prevalence, plotly = T)


library(plotly)

# roc

reference_lines_roc %>%
  plotly::plot_ly(x =~ x ,y =~y) %>%
  plotly::add_lines(color = I("grey"), line = list(dash = 'dash', width = 1.75)) %>%
  create_plotly_for_performance_metrics(FPR,
                                        sensitivity,
                                        reference_lines = reference_lines
  )

# lift

reference_lines_lift %>%
  plotly::plot_ly(x =~ x ,y =~y) %>%
  plotly::add_lines(color = I("grey"), line = list(dash = 'dash', width = 1.75))


# precision recall not multiple pop

create_color_populations_vector <- function(prevalence, col_values = c("#5E7F9A", 
                                                                    "#931B53", 
                                                                    "#F7DC2E", 
                                                                    "#C6C174", 
                                                                    "#75DBCD")){
  col_populations_vec <- col_values[1:length(prevalence)]
  names(col_populations_vec) <- names(prevalence)
  col_populations_vec
}


create_color_models_vector <- function(models, col_values = c("#5E7F9A", 
                                                              "#931B53", 
                                                              "#F7DC2E", 
                                                              "#C6C174", 
                                                              "#75DBCD")){
  col_models_vec <- col_values[1:length(unique(performance_data$model))]
  names(col_models_vec) <- names(unique(performance_data$model))
  col_models_vec
}


create_color_reference_lines_vector <- function(color_populations_vector, curve){
  if (curve == "gains") {
    color_populations_vector <- c(color_populations_vector, random = "grey")
  }
  if (curve == "precision recall") {
    color_populations_vector <- color_populations_vector
  }
  if (curve == "decision") {
    color_populations_vector <- c(color_populations_vector, treat_none = "grey")
  }
  color_populations_vector
}



create_linetype_reference_vector <- function(color_populations_vector, curve){
  col_populations_vec <- rep("dash", length(color_populations_vector))
  names(col_populations_vec) <- names(color_populations_vector)
  if (curve == "gains") {
    col_populations_vec <- c(col_populations_vec, random = "solid")
  }
  if (curve == "decision") {
    col_populations_vec <- c(col_populations_vec, treat_none = "solid")
  }
   col_populations_vec
}


create_color_populations_vector(prevalence) %>%
  create_color_reference_lines_vector("precision recall")

create_color_populations_vector(prevalence) %>%
  create_color_linetype_vector("gains")




create_reference_lines_data_frame("precision recall", prevalence[1], plotly = T) %>%
  plot_ly(x =~ x ,y =~y) %>%
  add_lines(color = I("grey"), line = list(dash = 'dash', width = 1.75))

# precision recall  multiple pop


reference_lines_precision_recall %>%
  plot_ly(x =~ x ,y =~y, color =~ population, 
          colors =  create_color_populations_vector(prevalence) %>%
            create_color_reference_lines_vector("precision recall")) %>%
  add_lines(line = list(dash = 'dash',  width = 1.75))


# gains not multiple pop

create_reference_lines_data_frame("gains", prevalence[1], plotly = T) %>%
  plot_ly(x =~ x ,y =~y) %>%
  add_lines(color = I("grey"), line = list(dash = 'dash', width = 1.75))

# gains multiple pop

reference_lines_gains %>%
  plot_ly(x =~ x ,y =~y, color =~ population, 
          colors =  create_color_populations_vector(prevalence) %>%
            create_color_reference_lines_vector("gains"),
          linetype =~ population,
          linetypes = create_color_populations_vector(prevalence) %>%
            create_linetype_reference_vector("gains")) %>%
  add_lines(line = list( width = 1.75))



# decision not multiple pop


create_reference_lines_data_frame("decision", prevalence, plotly = T) 



# reference_lines  %>%
#   split(1:nrow(.)) %>%
# purrr::reduce(add_reference_lines_to_plotly,
#               .init = fake_base_plotly) %>%
#   add_markers() %>%
#   add_lines()



make_reference_lines_proper_to_multiple_populations <- function(reference_lines){
  reference_lines %>%
    tibble::rownames_to_column("population") %>%
    tidyr::pivot_longer(cols = c(x, xend), values_to = "x") %>%
    select(-name) %>%
    tidyr::pivot_longer(cols = c(y, yend), values_to = "y") %>%
    select(-name) %>%
    distinct() 
}



reference_lines_multiple_pop <- reference_lines %>%
  make_reference_lines_proper_to_multiple_populations()

library(plotly)

reference_lines_multiple_pop %>%
  plot_ly(x =~ x ,y =~y, color =~ population, colors =~ col) %>%
  add_lines() %>%
  plotly::add_trace(
    data = train_and_test_sets,
    x =~ sensitivity, 
    y =~ PPV,
    hoverinfo = "text",
    text = ~ paste(
      "Population:", population, "<br>",
      "TPR (Sensitivity):", round(sensitivity, digits = 3), "<br>",
      "FPR:", round(FPR, digits = 3), "<br>",
      "Specificity", round(specificity, digits = 3), "<br>",
      "LIFT", round(lift, digits = 3), "<br>",
      "PPV", round(PPV, digits = 3), "<br>",
      "NPV", round(NPV, digits = 3), "<br>",
      "TP:", TP, "<br>",
      "TN:", TN, "<br>",
      "FP:", FP, "<br>",
      "FN:", FN
    ), 
    type = "scatter", 
    mode = "markers+lines"
  ) %>%
  add_markers(
    x =~ sensitivity,
    y =~ PPV,
    frame =~ threshold,
    marker =     list(
      size = 12,
      line = list(
        width = 3,
        color = I("black")
      )
    ),
    hoverinfo = "text",
    text = ~ paste(
      # "Model:", model,
      "TPR (Sensitivity):", round(sensitivity, digits = 3), "<br>",
      "FPR:", round(FPR, digits = 3), "<br>",
      "Specificity", round(specificity, digits = 3), "<br>",
      "LIFT", round(lift, digits = 3), "<br>",
      "PPV", round(PPV, digits = 3), "<br>",
      "NPV", round(NPV, digits = 3), "<br>",
      "TP:", TP, "<br>",
      "TN:", TN, "<br>",
      "FP:", FP, "<br>",
      "FN:", FN
    )
  )
  
  



