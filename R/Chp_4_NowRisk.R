##################################### --
#### Plotting the Data and Interpreting the Plot 
#### Chapter 4 Failure forecasting and risk analysis  
##################################### --


#' Now risk 
#' 
#' Now risk formula from the chapter 4-2. This formula is usefull to check 
#' if the weibull fit is apropiatte and check for batch defects. 
#' @param t
#' @param eta
#' @param beta 
#' @param N number of components with the descrive weibull characteristics
#' @examples 
#' 

NowRisk   <- function(t=t, eta=eta, beta=beta, N=N) {
  # Funciton Expected failure or Now Risk
  ft_fun <- function(t, eta = eta, beta = beta) {
    round(1 - exp(-( (t / eta) ^ beta) ), digits = 3)
  }
  
  ft <-   sapply(t, FUN = ft_fun, eta = eta, beta = beta)
  temp <- N * ft
  #  browser()
  NowRisk <- sum(temp)
  return(NowRisk)
}

#' Now Risk wirth Reduced Bias Adjustment
#' 
#' No risk with RBA correction
#' @export
#' @import dplyr
#' @param df
#' @param eta
#' @param beta
#' @examples

NowRiskRBA   <- function(df, eta=eta, beta=beta) {
  # Funciton Expected failure or Now Risk
  ft_fun <- function(t, eta = eta, beta = beta) {
    ft <- 1 - exp(-( (t / eta) ^ beta))
    ft <- round(ft, digits = 3)              
    return(ft)
    
  }
  
  t_failures <- df %>% filter(event==1) %>% select(time)  
  t_failures <- as.numeric(t_failures$time)
  
  t_suspensions <- df %>% filter(event==0) %>% select(time) 
  t_suspensions <- as.numeric(t_suspensions$time)
  
  ft_failures <-   sapply(t_failures, FUN = ft_fun, eta = eta, beta = beta)
  ft_suspensions <-  sapply(t_suspensions, FUN = ft_fun, eta = eta, beta = beta)
  
  NowRiskRBA <- 2 * sum(ft_failures) + sum(ft_suspensions)
  NowRiskRBA <- round(NowRiskRBA, digits = 2)
  return(NowRiskRBA)
}


FailuresFuture_legacy1 <- function(u=u, t=t, eta=eta, beta=beta, N=N){
  ft_fun <- function(t=t, eta = eta, beta = beta){
    round(1 - exp(-( (t / eta) ^ beta) ), digits = 5)}
  
  ft  <- ft_fun(t=t, eta = eta, beta = beta);ft
  ftu <- ft_fun(t=t+u, eta = eta, beta = beta);ftu
  FailuresFuture <- sum(N* (ftu-ft)/(1-ft));FailuresFuture
  
  return(round(FailuresFuture, digits = 1))
}


#' Failures future
#' 
#' Forecast the number of failures that will occour, after a specific amount 
#' of cicles/time. This forecast assumes no renewal for the fail parts
#' 
#' @inheritParams NowRiskRBA
#' @param u Amount of time/cicle to predict the number of failures

FailuresForecast <- function(u, df, eta, beta){
  ft_fun <- function(t=t, eta = eta, beta = beta){
    round(1 - exp(-( (t / eta) ^ beta) ), digits = 5)}
  
  # SUM( F(ti+u) - f(ti) / 1-f(ti) )
  t_suspensions <- df %>% filter(event==0) %>% select(time) 
  t_suspensions <- as.numeric(t_suspensions$time)
  
  f_ti_u <- ft_fun(t=t_suspensions + u, eta, beta)
  f_ti <- ft_fun(t=t_suspensions, eta, beta)
  
  futureRisk <- sum(   ( (f_ti_u - f_ti) / (1-f_ti) ))
  return( round(futureRisk, digits = 4))
}

#' Help function to test the code
#' @importFrom tibble, as_tibble
extrac_param <- function(df){
  dfa <- Abrem(df , col="black", pch=2)
  dfa <- abrem.fit(dfa, col="blue")
  
  # Extract the parameters
  beta <- round( dfa$fit[[1]]$beta, 2)
  eta  <- round( dfa$fit[[1]]$eta,  2)
  return( tibble::tibble(beta=beta, eta= eta, abrem=list(dfa) ) )
}
