library(plumber)
x <- plumb("C:/Users/CRI_user/Documents/rtichoke/inst/rest_api/plumber.R")
# x$filter("robj", function(req) {
#   req$rook.input$rewind()
#   req$robj <- unserialize(req$rook.input$read())
#   plumber::forward()
# })

x$run(debug = TRUE, host = "0.0.0.0", port = 4242)

