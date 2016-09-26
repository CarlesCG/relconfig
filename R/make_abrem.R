######################################## #
# Make abrem pot function
# with multiple failure modes capability!
######################################### #
library(abrem)
library(tidyverse)

#' Make one filure mode object
make_abrem <- function(df, 
                       fit.selector= input$fit.selector,
                       conf.intervals = input$confinter, 
                       conf.method = input$method.conf, 
                       conf.level = input$conflevel.generator){
  # Detect if data set is loaded. Otherwise, dummy
  # df <- detect_df(df, input) 
  
  message("Calculating individual Abrems...")
  
  # Create the data frame (this is redundant)
  # df <- data.frame(serial=df$part_id,
  #               time=df$time, event=df$event)
  
  ### 2. Make abrem object with the df  
  # Fit abrem without fit.selection option! 
  dfa <- Abrem(df)
  
  # Work around due to the impossibility to render concatenated strings i.e. c('rr', 'xony')
  # to HTMLwithout getting escaped!
  if(fit.selector %in% "RRxony"){
    dfa <- abrem.fit(dfa, col="blue", method.fit=c('rr', 'xony'))
  }else{
    # This get ok the mle and mle-rba
    dfa <- abrem.fit(dfa, col="blue", method.fit=fit.selector)
  }
  
  ### 3. Add confidence intervals
  if(conf.intervals == T){
    dfa <- abrem.conf(dfa, 
                      method.conf.blives= conf.method, 
                      cl=                 conf.level, 
                      unrel.n=25, 
                      lty=2, lwd=2, col="orange")
  }
  
  return(dfa)
}

#' Make several abrems
#' @export
make_several_abrems <- function(df, 
                                fit.selector = input$fit.selector,
                                conf.intervals = input$confinter, 
                                conf.method = input$method.conf, 
                                conf.level = input$conflevel.generator){
  
  # # 1. Get the number of failure modes in the data
  modes <- levels(df$failure_mode)
  message("Detectec failure mode(s): ", modes )
  
  # 2. Funciton that will combine the modes with the suspensions
  # and split them in a list
  split_datasets <- function(modes, data){
    if (modes != 'None') {
      df2    <- data %>% dplyr::filter(failure_mode %in% modes)
      noMode <- data %>% dplyr::filter(failure_mode %in% 'None')
      df3    <- full_join(df2, noMode)
      return(df3)
    }
  }
  
  result <- lapply(X = modes, FUN = split_datasets, df)
  
  # For some weird reason there is and extra element.
  result[ length(result) ] <- NULL # Delete the last list
  
  # 3. Apply to each list the function make_abrem and add them to a list
  # The foo plot.abrem takes lists af abrem objects as inputs.
  list.ofAbrems <- lapply(X= result , FUN = make_abrem, 
                          # Input variables
                          fit.selector,
                          conf.intervals, 
                          conf.method, 
                          conf.level )
  
  return(list.ofAbrems)
}

#' Plot abrem objects with options
#' @export
#' @import abrem
plot_abrem <- function(dfa, 
                       conf.intervals = input$confinter, 
                       conf.method = input$method.conf, 
                       conf.level = input$conflevel.generator,
                       is.plot.legend= T, 
                       title= "Title"){
  
  # Subtitle
  if( conf.intervals == T){
    if( conf.method == 'lrb'){
      subtitle2 <- paste0("Likelihood Ratio confidence level ", conf.level*100," %", "(the red line)")
    }else{ subtitle2 <- paste0("Beta Binomial confidence level ", conf.level*100," %", "(the red line)")     
    }
  }else{
    subtitle2 <- " "
  }
  
  message('Plotting abrem object...')
  # Plot
  plot.abrem(dfa, 
             main= title, 
             sub= subtitle2, 
             is.plot.legend=is.plot.legend,
             xlab= "Time to Failure", 
             ylab= "Occurrence CDF %")
}

#' Detec if there is a df() loaded. If not pass dummy dataset.
detect_df <- function(df, 
                      load.file= input$file){
  # If there is not any data.frame for the function the pick up one example data set
  if( is.null(load.file) == T){ df <- get(data('twofm', package = 'abernethy') )
  }else{ df <- df() }
}


#' Detec if there is one or more failure modes.
detect_fm <- function(df){
  # df <- get ( data('onefm', package = 'abernethy') )
  Nfm <- nlevels(df$failure_mode) - 1 
  
  message( sprintf("Number of fm present in the data set: %s", Nfm) )
  return( Nfm)
}
