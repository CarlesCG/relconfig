##################################### --
#### Zero test failure functions 
#### Chapter 6 at NWH 
##################################### --



##### Section 6.10 Zero test failure test pland for reliability testing

#' Equivalent characteristic life requirement
#' Re-expresion of reliablity goal to determine HTML("&eta;:", "Characteristic life to demonstrate")
#' 
#' @param dem_eta Eta/characteristic life to demonstrate
#' @param beta beta of the failure mode
#' @param Bx Reliability requiremnt at time/cycles t. Typically is B1 or B10. Integer from ]0, 100[
#' @param t Time/cycles to meet the Rt reliability requirement
#' @examples 
#' # Lets calculate the the equivalen eta for a B1 of 1800 cycles
#' # with a beta of 3
#' t <- 1800
#' Bx <- 1
#' beta <- 3
#' calculate_equivalent_eta(t=1800, Bx= 1, beta=3)
#' #
#' # which results in a equivalent life of 8340.9

calculate_equivalent_eta <- function(t, Bx, beta) {
  # Checks
  if( missing(t) || missing(Bx) ||missing(beta) ) stop("Specify all parameters")
  
  
  # Get the reliability percentage from the requirement B-life
  Rt <- (100-Bx)/100
  
  round( 
     t / ( (-log(Rt))^(1/beta) ), 
     digits = 1
     )
 }



#' Probability pass zero test
#' 
#' NEED TO UNDERSTAND THIS!
#' @export
#' @inheritParams k_At_Nconf
#' @param r ratio: true eta / demonstrated eta
#' @examples 
#' Probability_Passing_Zero_Test(n=10, beta=1.5, r=.5, conf=.9)
Probability_Passing_Zero_Test <- function(n, beta, r, conf=.9){
  # Checks
  if( missing(n) || missing(beta) || missing(r)) stop("Specify parameter") 
  if(conf ==.9) message("Confidence level defafult is 0.9")
  
  # foo
  probability <- 
    round( 
      (exp( ( -(k_At_Nconf(n, beta, conf)/r)^beta) ) )  ^ n, 
      digits = 3)
  
  return(probability)
  
}



### Section 6.9 Zero test failure for substaintion testing

#' Required sample size for partial test plan
#' 
#' @param dem_eta Requirement to demonstrate new eta (scale)
#' @param test_eta Maximum number of cycles that the will eb able to run
#' @param beta shape of the Weibull distribution
#' @param conf Level of confidence
#' @return Number of components/samples needed in the test to pass, given the parameters
#' @examples 
#' 
#' samples_needed(dem_eta= 500, test_eta= 300, beta=2, conf=.9  )

samples_needed <- function(dem_eta, test_eta, beta, conf=.9){
  ceiling(
  (-((dem_eta/test_eta)^beta))*log(1-conf)
  )
}


#' Zero test failure time
#' 
#' The zero test failure calculates how much time a test with zero failures 
#' have to run in order to demonstrate a certain eta with an specific level of confidence.
#' If the one or more of the components fail, the test has fail. Making several Zero tests
#' is consider cheating. 
#' 
#' @export
#' @inheritParams k_At_Nconf
#' @param dem_eta Requirement to demonstrate new eta (scale)
#' @examples 
#' time_Pass_Zero_failure(n=10, beta=1.5, dema_eta=500)
#' 

time_Pass_Zero_failure  <- function(n, beta, dem_eta, conf=.9){
  # Checks
  if( missing(dem_eta) ) stop("Specify dem_eta parameter")
  if( missing(conf)) message("Confidence level defafult is 0.9")
  
  # Foo
  time <-  round(
    ( k_At_Nconf(n, beta, conf) * dem_eta ),
    digits = 2)
  
  return( time ) 
}


#' Characteristic Life Multiplier - K
#' 
#' [ Section 6.1 New Weibull Handbook ] 
#' The characteristic life multiplier gives a constant that will indicate
#' for how much or less the test will have to be carry out, depending on the inputs. 
#' The results are based on the null hypothesis, that the new design is no better than the old.
#' 
#' @export
#' @param n number of components tested simultaneously
#' @param beta shape of the Weibull distribution
#' @param conf confidence to demonstrate
#' @examples 
#' n <- 1:10
#' k_At_Nconf(n, beta= 1.5, conf=.9)

k_At_Nconf <- function(n, beta, conf=.9){
  # Check inputs
  if(missing(n) || missing(beta)){
    stop("Please specify aprameters:
         n Number of components and/or
         beta Shape of the failure mode") 
  }
  if( missing(conf)) message("Default parameter `conf` set to 0.9")
  
  # foo
  (-(1/n) * log(1-conf) ) ^ (1/beta) 
}
