library(plumber)
library(rtichoke)
library(htmlwidgets)
library(xml2)
library(dplyr)


#* @serializer html
#* @post /rtichoke/create_summary_report
function(column, req, res) {
  dat <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyMatrix=FALSE),
                  error = function(e) NULL)
  
  print(dat$probs)
  print(dat$real)
  
  print("creating summary report")
  
  rtichoke::create_summary_report(dat$probs,
                                  dat$real)
  
  as.character(xml2::read_html("summary_report.html"))
}