library(plumber)
library(rtichoke)
library(htmlwidgets)
library(xml2)
library(dplyr)



#* @serializer htmlwidget
#* @post /rtichoke/roc_tichoke
function(req, res) {
  
  dat <- tryCatch(jsonlite::fromJSON(req$postBody),
                  error = function(e) NULL)
  
  # rtichoke::rtichoke(list(dat$probs), dat$real)
  result <- rtichoke::create_roc_curve(
    dat$probs,
    dat$reals,
    interactive = T)
  

  # result <- plotly::plot_ly(x =~ dat$probs,
  #                           y =~ dat$real)
  #result
  print(result)
}