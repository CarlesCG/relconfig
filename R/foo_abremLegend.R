# Format Weibull plot legend into a table
WeibullLegend <- function(r){
  x <- r
  
  measure <- c(
    "Number of points", 
    "Failures", 
    "Censored")
  
  values <- c(
    x$n, 
    x$fail, 
    x$cens ) 
  
  # results <- data.frame(measure, values)
  
  results <- data.frame( measure, values)
  names(results) <- c("Measure", "Value")
  
  return(results)
  
  
}


investigate <- function(){
# Extract parameter of the plot?? 
dplyr::bind_rows(x)
data.table::rbindlist(x)


# 0 level list 
x$data
x$n
x$fail
x$cens

# 1st level
options <- x$options
names(options)

fit <- x$fit[[1]]
fit$options

fit$beta
fit$eta
fit$gof
names(fit)

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

