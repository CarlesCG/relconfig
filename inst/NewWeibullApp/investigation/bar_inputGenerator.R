# library(abrem)
input <- c()
input$npoints <- 10
input$shape <- 1
input$scale <- 750
input$nFailures <- 26
input$npoints <- 60
input$obs <- 10
input$beta <- 2
input$dem_eta <- 500
input$dem_eta <- .85

input$conflevel_generator <- .8


## df
df <- data.frame(time = rweibull(n = input$npoints, shape = input$shape, scale = input$scale) )
df$event <- c( rep(1, input$nFailures), rep(0, input$npoints - input$nFailures) )

df <- data.frame(time=df$time, event=df$event)
dfa <- Abrem(df, col="black", pch=2)
dfa <- abrem.fit(dfa, col="blue")
plot( dfa )


# df CONF_INTERVALS
dfi <- abrem.conf(dfa, method.conf.blives="lrb", cl=input$conflevel_generator,
                  unrel.n=25,
                  lty=1, lwd=3, col="red")
