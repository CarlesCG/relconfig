##################################### --
#### Plotting the Data and Interpreting the Plot 
#### Chapter 2 at NWH 
##################################### --


#' Convert MTTF to Characteristic life
#' 
#' From the equation (2-4)
#' @param beta shape of the failure mechanism
#' @param MTTF Mean Time To Failure to convert
#' @examples
#' 
#' convert_MTTF_to_CharacLife(MTTF = 54, beta = 1)
#' convert_MTTF_to_CharacLife(MTTF = 54, beta = .5)
#' convert_MTTF_to_CharacLife(MTTF = 54, beta = .3)
#' convert_MTTF_to_CharacLife(MTTF = 54, beta = 2)
#' 
#' # Figure 2-3. MTTF/eta assymtotic relation 
#' beta <- seq(.5, 5, by = .05) 
#' ratio.MTTF.eta <- gamma(1+1/beta)
#' plot(beta, ratio.MTTF.eta, main = "MTTF / eta = Gamma[1+(1/beta)]")
#' abline(h = 0.9, col = 2, lty = 3, lw=3)
#' text(3,1.2, "MTTF / Eta = 0.9", col = "red")
#' text(3,1.25, "Asymptotic relation", col = "red")

convert_MTTF_to_CharacLife <- function(beta, MTTF){
  round(
    MTTF / gamma(1 + 1/beta),
    digits = 2
  )
}

#' Convert Characteristic life to MTTF
#' From the equation (2-4)
#' 
#' @param beta shape of the failure mechanism
#' @param MTTF Mean Time To Failure to convert
#' @examples
#' convert_CharacLife_to_MTTF(eta = 54, beta = .5)
#' convert_CharacLife_to_MTTF(eta = 54, beta = 1)
#' convert_CharacLife_to_MTTF(eta = 54, beta = 1.5)
#' 
#' # Figure 2-3. MTTF/eta assymtotic relation 
#' beta <- seq(.5, 5, by = .05) 
#' ratio.MTTF.eta <- gamma(1+1/beta)
#' plot(beta, ratio.MTTF.eta, main = "MTTF / eta = Gamma[1+(1/beta)]")
#' abline(h = 0.9, col = 2, lty = 3, lw=3)
#' text(3,1.2, "MTTF / Eta = 0.9", col = "red")
#' text(3,1.25, "Asymptotic relation", col = "red")


convert_CharacLife_to_MTTF <- function(eta, beta){
  round(
    eta * gamma(1 + 1/beta),
    digits = 2
  )
}
