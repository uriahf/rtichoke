library(httr)
library(rtichoke)

?create_roc_curve

r <- POST("http://127.0.0.1:7644/create_summary_report", 
          body = jsonlite::toJSON(
            list(
              probs = list("First Model" = example_dat$estimated_probabilities),
              reals = list("First Model" = example_dat$outcome)), 
            auto_unbox = TRUE), 
          encode = "json")




r <- POST("http://127.0.0.1:7644/create_summary_report", 
          body = jsonlite::toJSON(
            list(
              probs = list(
                "First Model" = example_dat$estimated_probabilities,
                "Second Model" = example_dat$random_guess
              ),
              reals = list(example_dat$outcome)), 
            auto_unbox = TRUE), 
          encode = "json")


r <- POST("http://127.0.0.1:7644/create_summary_report", 
          body = jsonlite::toJSON(
            list(
              probs = list(
                "train" = example_dat %>%
                  dplyr::filter(type_of_set == "train") %>%
                  dplyr::pull(estimated_probabilities),
                "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
                  dplyr::pull(estimated_probabilities)
              ),
              reals = list(
                "train" = example_dat %>% dplyr::filter(type_of_set == "train") %>%
                  dplyr::pull(outcome),
                "test" = example_dat %>% dplyr::filter(type_of_set == "test") %>%
                  dplyr::pull(outcome)
              )), 
            auto_unbox = TRUE), 
          encode = "json")




r <- POST("http://127.0.0.1:7644/roc_curve_list", 
          body = jsonlite::toJSON(
            list(
              probs = list("First Model" = example_dat$estimated_probabilities),
              reals = list("First Model" = example_dat$outcome)), 
            auto_unbox = TRUE), 
          encode = "json") |> 
  content()


r$reference_data |> 
  rtichoke:::create_plotly_curve()

r$cookies
  
names(r)


r$



json_rtichoke_list <- jsonlite::toJSON(
  list(
    probs = list("First Model" = example_dat$estimated_probabilities),
    reals = list("First Model" = example_dat$outcome)), 
  auto_unbox = TRUE) |> 
  jsonlite::fromJSON() 


prepare_performance_data(
  probs = as.list(json_rtichoke_list$probs),
  reals = as.list(json_rtichoke_list$reals)
) |> 
  rtichoke:::create_rtichoke_curve_list("roc") |> 
  jsonlite::toJSON()



r$request$output


r$request$output
names(r)
