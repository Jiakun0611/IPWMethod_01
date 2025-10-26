library(devtools)
library(usethis)
library(roxygen2)
unlink("NAMESPACE")  # delete NAMESPACE as roxygen2 will generate new one

usethis::use_r("IPWM")
devtools::document()

build()      # Builds .tar.gz
install()    # Installs locally
