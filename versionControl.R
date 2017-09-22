# Repos to update
options(repos = c(CRAN = "https://mran.revolutionanalytics.com/snapshot/2016-09-01"))

## Specific libraries
# library(checkpoint)
# checkpoint("2016-12-01")


require(devtools)
install_version("shiny", version = "0.14.2", repos = "http://cran.us.r-project.org")


## Package Abrem
ifelse(!require(devtools),
       install.packages("devtools", type = "source"),
       library(devtools) ) 

install.packages("RCurl")

install_github(repo = "thomas4912/pkgs", subdir = "pkgs/debias")
install_github(repo = "thomas4912/pkgs", subdir = "pkgs/pivotals")
install_github(repo = "thomas4912/pkgs", subdir = "pkgs/abremDebias")
install_github(repo = "thomas4912/pkgs", subdir = "pkgs/abremPivotals")
install_github(repo = "thomas4912/pkgs", subdir = "pkgs/abrem")
