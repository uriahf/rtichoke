library(plumber)
library(rtichoke)
library(htmlwidgets)
library(xml2)
library(dplyr)


#* @serializer html
#* @post /create_summary_report
function(req, res) {
  
  print(req$body)
  
  print("probs")
  print(typeof(req$body$probs))
  print(str(req$body$probs))
  print(is.list(req$body$probs))
  
  print("reals")
  print(typeof(req$body$reals))
  print(str(req$body$reals))
  print(is.list(req$body$reals))
  
  create_summary_report(
    probs = req$body$probs,
    reals = req$body$reals
  )
  as.character(xml2::read_html("summary_report.html"))
  
}

#* @post /roc_curve_list
#* @serializer json
function(req, res){

  prepare_performance_data(
    probs = list(example_dat$estimated_probabilities),
    reals = list(example_dat$outcome)
   ) |>
    rtichoke:::create_rtichoke_curve_list("roc")
  
}