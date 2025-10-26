library(devtools)
library(usethis)
library(roxygen2)
unlink("NAMESPACE")  # delete NAMESPACE as roxygen2 will generate new one

usethis::use_r("IPWM")
devtools::document()

build()      # Builds .tar.gz
install()    # Installs locally
load_all()   # Loads your package into current R session

library(IPWMethod)
?IPWM

# for others to use
install.packages("devtools")
devtools::install_github("Jiakun0611/IPWMethod")
