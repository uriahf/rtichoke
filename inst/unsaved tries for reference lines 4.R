


performance_table_type <- check_performance_table_type_for_plotly(perf_table)
prevalence <- get_prevalence_from_performance_table(perf_table)
population_color_vector <- create_color_populations_vector(prevalence) 


create_reference_lines_for_plotly <- function(performance_table_type, curve, prevalence = NA, population_color_vector = NA){
  if (curve %in% c("roc", "lift") || performance_table_type != "several populations" ) {
    
    reference_lines_for_plotly <- create_reference_lines_data_frame(curve, plotly = T, prevalence) %>%
      plot_ly(x =~ x ,y =~y)  %>%
      add_lines(color = I("grey"), line = list(width = 1.75))
    
  } else {
    
  if (curve == "precision recall") {

    reference_lines_for_plotly <- create_reference_lines_data_frame("precision recall", plotly = T, prevalence) %>%
      plot_ly(x =~ x ,y =~y, color =~ population,
              colors =  population_color_vector) %>%
      add_lines(line = list(dash = 'dash',  width = 1.75))

  }
    
  if (curve == "gains") {
    print("ok")
      
      population_color_reference_vector <- population_color_vector %>%
        create_color_reference_lines_vector("gains")
      print(population_color_reference_vector)
      
      
      population_linetype_reference_vector <- population_color_vector %>%
        create_linetype_reference_vector("gains")
      print(population_linetype_reference_vector)
      
      
      reference_lines_for_plotly <- create_reference_lines_data_frame("gains", plotly = T, prevalence) %>%
        plot_ly(x =~ x,
                y =~y, 
                color =~ population,
                colors =  population_color_reference_vector) %>%
        add_lines(line = list(width = 1.75),
                  linetype =~ population,
                  linetypes = population_linetype_reference_vector)

  }
    
    if (curve == "decision") {

      population_color_reference_vector <- population_color_vector %>%
        create_color_reference_lines_vector("decision")

      population_linetype_reference_vector <- population_color_vector %>%
        create_linetype_reference_vector("decision")

      reference_lines_for_plotly <- create_reference_lines_data_frame("decision", plotly = T, prevalence) %>%
        plot_ly(x =~ x,
                y =~y,
                color =~ population,
                colors =  population_color_reference_vector) %>%
        add_lines(line = list(width = 1.75),
                  linetype =~ population,
                  linetypes = population_linetype_reference_vector)

    }
  
  }
  
  reference_lines_for_plotly
  
}


create_reference_lines_data_frame("decision", plotly = T, prevalence)

create_reference_lines_for_plotly(performance_table_type, "roc")
create_reference_lines_for_plotly(performance_table_type, "lift")
create_reference_lines_for_plotly("several populations", "gains", prevalence, population_color_vector = population_color_vector)
create_reference_lines_for_plotly("several populations", "precision recall", prevalence, population_color_vector = population_color_vector)
create_reference_lines_for_plotly("several populations", "decision", prevalence, population_color_vector = population_color_vector)



create_reference_lines_for_plotly("several populations", 
                                  "decision", 
                                  prevalence, 
                                  population_color_vector = population_color_vector) %>%
  plotly::add_trace(
    data = perf_table, 
    x =~ threshold,
    y =~ NB,
    type = "scatter",
    mode = "markers+lines" 
  )  %>%
  plotly::add_markers(
    data = perf_table, 
    x =~ threshold,
    y =~ NB,
    frame = ~ threshold,
    marker =     list(
      size = 12,
      line = list(
        width = 3,
        color = I("black")
      )
    )
  )



  
linetype_solid <- population_linetype_reference_vector
linetype_solid[1:6] <- "solid"



performance_table_for_train_and_test_sets %>%
  rtichoke::create_plotly_for_performance_metrics(FPR, sensitivity) %>%
  plotly::layout(paper_bgcolor='#fdf9f1',
                 plot_bgcolor='#fdf9f1')
