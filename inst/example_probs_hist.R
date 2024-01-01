library(readr)

data_three_myths <- read.csv("C:/Users/Admin/Downloads/12916_2019_1425_MOESM1_ESM.csv")


create_summary_report(
  probs = list(data_three_myths$pred),
  reals = list(data_three_myths$outcome)
)

# 1. probs, probability_threshold

create_probs_histogram(
  probs = list(data_three_myths$pred),
  reals = list(data_three_myths$outcome), 
  condition_on = "probs",
  stratified_by = "probability_threshold",
  by = 0.01
)

# 2. probs, ppcr

create_probs_histogram(
  probs = list(data_three_myths$pred),
  reals = list(data_three_myths$outcome), 
  condition_on = "probs",
  stratified_by = "ppcr",
  by = 0.01
)

# 3. reals, probability_threshold

create_probs_histogram(
  probs = list(data_three_myths$pred),
  reals = list(data_three_myths$outcome), 
  condition_on = "reals",
  stratified_by = "probability_threshold",
  by = 0.01
)

# 4. reals, ppcr

create_probs_histogram(
  probs = list(data_three_myths$pred),
  reals = list(data_three_myths$outcome), 
  condition_on = "reals",
  stratified_by = "ppcr",
  by = 0.01
)






# 1. probabilit_threshold condition on none
# TODO: Should be condition_on = NA

create_probs_histogram(
  probs = list(data_three_myths$pred),
  reals = list(data_three_myths$outcome), 
  condition_on = "probs",
  stratified_by = "ppcr",
  by = 0.01
)

# 2. probabilit_threshold condition on reals
# TODO: create js files

create_probs_histogram(
  probs = list(data_three_myths$pred),
  reals = list(data_three_myths$outcome), 
  condition_on = "reals",
  stratified_by = "probability_threshold",
  by = 0.01
)

# TODO: Should be condition_on = NA

create_probs_histogram(
  probs = list(data_three_myths$pred),
  reals = list(data_three_myths$outcome), 
  condition_on = "reals",
  stratified_by = "ppcr",
  by = 0.01
)

# TODO: Should be condition_on = NA

create_probs_histogram(
  probs = list(data_three_myths$pred),
  reals = list(data_three_myths$outcome), 
  condition_on = "probs",
  stratified_by = "ppcr",
  by = 0.01
)











probs_distribution_p_threshold <- prepare_probs_distribution_data(
  
  probs = list(data_three_myths$pred),
  reals = list(data_three_myths$outcome), 
  condition_on = "reals",
  by = 0.01
)


real_positives_hist_dat <- probs_distribution_p_threshold$real_positives |> 
  # mutate(name = "real_positives") |>
  rename("real_positives" = counts,
         "cat" = "mids") |> 
  select(cat, real_positives)    


real_negatives_hist_dat <- probs_distribution_p_threshold$real_negatives |> 
  # mutate(name = "real_negatives") |>
  rename("real_negatives" = counts,
         "cat" = "mids") |> 
  select(cat, real_negatives)  



full_hist_dat <- left_join(
  real_positives_hist_dat,
  real_negatives_hist_dat,
  by = 'cat',
)

hist_predicted <- r2d3::r2d3(
  data = full_hist_dat,
  script = system.file(
    "d3/probs_hist.js", 
    package = "rtichoke"),
  width = 300,
  height = 300,
  container = 'div'
)


crosstalk::bscols(widths = c(1),
                  hist_predicted ,
                  htmltools::HTML("<input type='range' id='sliderelse' min='0' max='1' step='0.01' value='0.16'>
")
)



probs_distribution <- prepare_probs_distribution_data(
  
  probs = list(data_three_myths$pred),
  reals = list(data_three_myths$outcome), 
  condition_on = "probs",
  stratified_by = "ppcr", by = 0.01
)

real_positives_hist_dat <- probs_distribution$real_positives |> 
  # mutate(name = "real_positives") |>
  rename("real_positives" = counts,
         "cat" = "mids") |> 
  select(cat, real_positives)    


real_negatives_hist_dat <- probs_distribution$real_negatives |> 
  # mutate(name = "real_negatives") |>
  rename("real_negatives" = counts,
         "cat" = "mids") |> 
  select(cat, real_negatives)  


full_hist_dat <- left_join(
  real_positives_hist_dat,
  real_negatives_hist_dat,
  by = 'cat',
)


hist_predicted_positives <- r2d3::r2d3(
  data = full_hist_dat,
  script = system.file(
    "d3/probs_hist_predicted_positives.js", 
    package = "rtichoke"),
  width = 300,
  height = 300,
  container = 'div'
)

hist_predicted_negatives <- r2d3::r2d3(
  data = full_hist_dat,
  script = system.file(
    "d3/probs_hist_predicted_negatives.js", 
    package = "rtichoke"),
  width = 300,
  height = 300,
  container = 'div'
)

crosstalk::bscols(
  div(hist_predicted_negatives) ,
  div(hist_predicted_positives) ,
  htmltools::HTML("<input type='range' id='sliderelse' min='0' max='1' step='0.01' value='0.16'>
")
)




hist_predicted <- r2d3::r2d3(
  data = full_hist_dat,
  script = system.file(
    "d3/probs_hist.js", 
    package = "rtichoke"),
  width = 300,
  height = 300,
  container = 'div'
)

crosstalk::bscols(widths = c(1),
                  hist_predicted ,
                  htmltools::HTML("<input type='range' id='sliderelse' min='0' max='1' step='0.01' value='0.16'>
")
)

