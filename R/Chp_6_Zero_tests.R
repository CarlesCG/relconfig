#####################################
#### Zero test failure functions 
#### Chapter 6 at NWH 
#####################################

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
  if(conf ==.9) message("Confidence level defafult is 0.9")
  
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
  if(conf ==.9) message("Default parameter `conf` set to 0.9")
  
  # foo
  (-(1/n) * log(1-conf) ) ^ (1/beta) 
}
