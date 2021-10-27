test_that("multiplication works", {
  
  one_pop_one_model_as_a_vector %>%
    render_performance_table(interactive = T)
  expect_equal(2 * 2, 4)
})
