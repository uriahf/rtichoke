


performance_table_type <- check_performance_data_type_for_plotly(rtichoke::train_and_test_sets)
prevalence <- get_prevalence_from_performance_data(rtichoke::train_and_test_sets)
population_color_vector <- create_color_populations_vector(prevalence) 

performance_table_type <- check_performance_data_type_for_plotly(rtichoke::one_pop_three_models)
prevalence <- get_prevalence_from_performance_data(rtichoke::one_pop_three_models)
population_color_vector <- create_color_populations_vector(prevalence) 





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
    data = train_and_test_sets, 
    x =~ threshold,
    y =~ NB,
    type = "scatter",
    mode = "markers+lines"
  )  %>%
  plotly::add_markers(
    data = train_and_test_sets, 
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


create_reference_lines_for_plotly("several populations", 
                                  "decision", 
                                  prevalence, 
                                  population_color_vector = population_color_vector) %>%
  plotly::add_trace(
    data = train_and_test_sets, 
    x =~ threshold,
    y =~ NB,
    type = "scatter",
    mode = "markers+lines" 
  )  %>%
  plotly::add_markers(
    data = train_and_test_sets, 
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




create_reference_lines_for_plotly("several populations", 
                                  "roc", 
                                  population_color_vector = population_color_vector) %>%
  plotly::add_trace(
    data = train_and_test_sets, 
    x =~ FPR,
    y =~ sensitivity,
    type = "scatter",
    mode = "markers+lines" ,
    color =~ population
  )  %>%
  plotly::add_markers(
    data = train_and_test_sets, 
    x =~ FPR,
    y =~ sensitivity,
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



train_and_test_sets %>%
  rtichoke::create_plotly_for_performance_metrics(FPR, sensitivity) %>%
  plotly::layout(paper_bgcolor='#fdf9f1',
                 plot_bgcolor='#fdf9f1')
