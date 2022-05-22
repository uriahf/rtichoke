library(plumber)
library(rtichoke)
library(htmlwidgets)
library(xml2)
library(dplyr)

#* @serializer html
#* @post /rtichoke/flex_tichoke

function(column, req, res) {
  dat <- tryCatch(jsonlite::fromJSON(req$postBody),
                  error = function(e) NULL)
  
  print(dat)
  
  # print("probs")
  # print(dat$probs)
  # 
  # print("reals")
  # print(dat$probs)
  # 
  # # rtichoke::rtichoke(list(dat$probs), dat$real)
  # rtichoke::create_summary_report(
  #   probs = dat$probs, 
  #   reals = dat$reals
  #   ) 
  # 
  # as.character(xml2::read_html("summary_report.html"))
  
  # file <- tempfile(fileext=".html")
  # htmlwidgets::saveWidget(result, file, selfcontained = T)
  # paste(readLines(file), collapse="")
  
}