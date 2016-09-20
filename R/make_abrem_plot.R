# Make abrem pot function
# with multiple failure modes capability!

# NECESITAS REESCRIBIR TODA ESTA MIERDA! NO HAY DIOS QUE LO ENTIENDA!!
# SALEN UN HUEVO DE ERRORES CUANDO INTENTAS INICIALIZAR CON EL PLOT
# DE LOS 2 FAILURE MODES! 

## MODULE GENERATOR - improve
# For small Betas (like 0.1) and condinden intervals plotting get an error
# > Warning in dweibull(x, Beta, Eta, log = TRUE) : NaNs produced
# > Warning in pweibull(s, Beta, Eta, lower.tail = FALSE, log.p = TRUE) :
# >  NaNs produced
# > Error in RadialSecant(RadEst, theta, Beta_hat, Eta_hat, RadLimit) : 
# >  object 'Eta1' not found
# > Error in if (X2 < 0) { : missing value where TRUE/FALSE needed


#' Function to make and abrem plot
#' 
#' Description 
#' @export
#' @import abrem
#' @param df a data frame shiuted with the column to make and abrem plot
#' @param input this are the inputs from the shinyapp to make the plot interactive
#' @param title title of the plot to make 

make_abrem_plot <- function(df, input, title){
  # Notes:
  # df is reactive expresion! 
  
  # If there is not any data.frame for the function the pick up one example data set
  # if( is.null(input$file) == T){
  #   print("input$file is 0")
  #   df <- twofm
  #   head ( print(df) )
  #   }else{
  #     print("input$file is 1")
  #     df <- df()
  #   }

  
  # data.initial <- reactive({
  #   validate(need(input$file, message = FALSE))
  #   df()
  # })
  # 
  if(input$method.conf %in% 'lrb'){
    subtitle2 <- paste0("Likelihood Ratio confidence level ", input$confcalc*100," %", "(the red line)")
  } else { 
    subtitle2 <- paste0("Beta Binomial confidence level ", input$confcalc*100," %", "(the red line)")
  }
  
  if (df$failure_mode %>% unique %>% length <= 2) {
    print("Business as usual")
    
    # Create the data frame (this is redundant)
    df <- data.frame(serial=df$part_id, 
                     time=df$time, event=df$event)
    print('1')
    # Fit abrem without fit.selection option! 
    dfa <- Abrem(df)
    # Work around due to the impossibility to render concatenated strings to HTML
    # without getting escaped!
    if(input$fit.selector %in% "RRxony"){
      dfa <- abrem.fit(dfa, col="blue", method.fit=c('rr', 'xony'))
    }else{
      dfa <- abrem.fit(dfa, col="blue", method.fit=input$fit.selector)
    }
    print('2')
    
    dfa <- abrem.conf(dfa, method.conf.blives=input$method.conf, cl=input$confcalc, 
                      unrel.n=25, 
                      lty=1, lwd=3, col="red")
    print('2.2')
    # Plot
    plot(dfa, main=title, sub=subtitle2, is.plot.legend=T,
         xlab="Time to Failure", 
         ylab="Occurrence CDF %")
    print('Plot 1 FM done!')
  }else{
    print("There are 2 or more failure modes!")
    # Lets get down with it!
    # Get a list with the named modes!
    modes <- as.list(levels(df$failure_mode)) 
    print('3')
    # Split into 1 list per factor and combine with the 'None' factor! then
    # combine in a new list.
    result <- lapply(
      X = modes, FUN = function(X) {
        if (X != 'None') {
          df2    <- df %>% filter(failure_mode %in% X)
          noMode <- df %>% filter(failure_mode %in% 'None')
          df3 <- full_join(df2, noMode)
        }
      }
    )
    print('4')
    # Assign individuals data frames by name
    lapply(seq_along(result), function(x) {
      assign(
        x = gsub(pattern = " ", replacement = "_", x = modes[x]),
        value = result[[x]], envir = .GlobalEnv)
    })
    
    # rm("Mode_A");rm("Mode_B");rm("Mode_C")
    print('5')
    names.df <- gsub(pattern = " ", replacement = "_", x = modes)
    names.df <- names.df %>% subset(names.df != 'None')
    
    # YEEEE Got the df by name!
    # Now we will need to create and object with all objects (lapply like brefore??)
    # Sort of like the 
    # The concatenate into the plot!
    
    print('6')
    # Make function! x es un character string!!
    #, input_abrem=input
    make_abrem <- function(x, 
                           type.fit= input$fit.selector, 
                           confidence= input$confcalc,
                           conf.blives= input$method.conf 
    ){
      df.fit <- eval(parse(text = names.df[x]))
      abrem.df <- Abrem(df.fit)
      
      if(type.fit %in% "RRxony"){
        fit <- abrem.fit(abrem.df , dist="weibull2p", 
                         method.fit=c("rr","xony"),  
                         col="blue")
      }else{
        fit <- abrem.fit(abrem.df , dist="weibull2p", 
                         method.fit=type.fit,  
                         col="blue")
      }
      
      conf <- abrem.conf(fit, method.conf.blives=conf.blives, 
                         unrel.n=28, cl=confidence, 
                         lty=1, lwd=3, col="dark green")
      return(conf)
    }
    # make_abrem(1)
    print('7')
    # browser()
    lapply(seq_along(names.df), function(x){
      assign(
        paste0("abrem_", names.df[x]), # Name of the object to be assign 
        make_abrem(x) ,                # Values to be assign 
        # envir = .GlobalEnv)          # Which enviroment
        envir = parent.env(environment())      # Which enviroment
      )
    })
    print('8')
    # browser() 
    # rm("abrem_Mode_A");rm("abrem_Mode_B")
    
    position <- grep(pattern = "abrem_", x = ls())
    names.abrems <- ls()[position]
    print('9')
    #browser() 
    
    # Generate abrems objects!
    abrems <-   lapply(seq_along(names.abrems), function(x) {
      eval(parse(text = names.abrems[x]) )
    })
    print('10')
    # browser() 
    
    # Habra que decirle return???
    plot.abrem(abrems, main=title, sub=subtitle2, is.plot.legend=T,
               xlab="Time to Failure", 
               ylab="Occurrence CDF %")
  }
}

#' Testing funciton
#' 
#' @export
#' @import abrem
test <- function(){
  Abrem(twofm)
}
## IT WORKS! THEREFORE THE PACKAGE IS LOADED CORRECTLY!
