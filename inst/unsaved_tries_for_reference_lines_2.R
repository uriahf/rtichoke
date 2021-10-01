



prevalence_one_model <- rtichoke:::get_prevalence_from_performance_table(rtichoke::one_pop_one_model_as_a_list)

data.frame(population = rep(names(prevalence_one_model), each = 2), 
           x = c(0, 1), 
           y = rep(prevalence_one_model, each = 2), 
           col = rep("grey", each = 2))

# precision recall multiple populations

data.frame(population = rep(names(prevalence), each = 2), 
           x = c(0, 1), 
           y = rep(prevalence, each = 2), 
           col = rep(col_values[1:2], each = 2)) %>%
  arrange(population, x, y)

# gains multiple populations


data.frame(population = names(prevalence), 
           x = prevalence, 
           y = 1, 
           col_values = col_values[1:2], 
           row.names = NULL) %>%
  bind_rows(
    data.frame(population = rep(names(prevalence), each = 2),
               x = c(0, 1),
               y = c(0, 1),
               col_values = rep(col_values[1:2], each = 2)
               )
  ) %>%
  arrange(population, x, y)

# decision multiple populations




data.frame(population = names(prevalence), x = prevalence, y = 0, col_values[1:2], row.names = NULL) 




reference_lines_data_frame <- data.frame(
  x = 0, xend = prevalence, y = prevalence, yend = 0, col = col_values[1:2], 
  linetype = "dotted"
)







