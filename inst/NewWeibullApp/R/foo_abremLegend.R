# Format Weibull plot legend into a table
WeibullLegend <- function(abrem.object){
  
  x <- purrr::flatten(abrem.object)
  variables <- names(x)
  
  df <- tibble(stat=x, names=variables)
  t(df)
  df
  
  # Make new columns with pretty description for plots
  
  
  ####
  ####
  
  description <- c(
    "Number of points", 
    "Failures", 
    "Censored")
  
  values <- c(
    x$n, 
    x$fail, 
    x$cens ) 
  
  # results <- data.frame(measure, values)
  
  results <- data.frame( variables , description, values)
  names(results) <- c("Measure", "Value")
  
  return(results)
  
  
}

flat_abrem <- function(x){
  purrr::flatten(x)
}

investigate_notRun <- function(){
# Extract parameter of the plot?? 
load("./data/abrem_structure.RDat")
dplyr::bind_rows(x)
data.table::rbindlist(x)

t <- purrr::flatten(x)

# 0 level list 
x$data
x$n
x$fail
x$cens
x$options # list
x$fit # list

# 1st level
options <- x$options
names(options)

t <- x$options

fit <- x$fit[[1]]
fit$options
fit$n
fit$fail
fit$cens
fit$data
fit$lm  
fit$beta
fit$eta
fit$gof
names(fit)

fit$conf
# Si hay confident intervals
conf <- fit$conf$blives[[1]]
conf$type
conf$cl
conf$sides
conf$unrel
conf$MLEXContour
conf$bounds
conf$options


}