library(plumber)

x <- plumb("C:/Users/CRI_user/Documents/rtichoke/inst/rest_api/plumber.R")
x$run(debug = TRUE, host = "0.0.0.0", port = 4242)

