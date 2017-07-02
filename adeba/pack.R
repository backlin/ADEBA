library(roxygen2)
roxygenize()

library(devtools)
check(".", args="--as-cran")
install()
build()

