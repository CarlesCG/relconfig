# library(abrem)
# Module generate

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
input$fit.selector <- "mle-rba"
input$method.conf <- 'lrb'
input$conflevel_generator <- .9
input$confinter == F
input$confi <- .9
input$test_eta <- 300



# Dummy data
df <- data.frame(time = rweibull(n = input$npoints, shape = input$shape, scale = input$scale) )
df$event <- c( rep(1, input$nFailures), rep(0, input$npoints - input$nFailures) )

df <- data.frame(time=df$time, event=df$event)

# Fit abrem 
library(abrem)
dfa <- Abrem(df , col="black", pch=2)
dfa <- abrem.fit(dfa, col="blue")

dfa <- abrem.conf(dfa, method.conf.blives="lrb", cl=input$conflevel_generator,
                    unrel.n=25, S=1000,
                    lty=1, lwd=3, col="red") 
plot(dfa)

# foo build legends
getAnywhere("buildSingleFitLegend")
function (fit, opadata, ...) 
{
  arg <- list(...)
  if (!is.null(fit$options)) {
    opafit <- modifyList(opadata, fit$options)
  }
  else {
    opafit <- opadata
  }
  opafit <- modifyList(opafit, list(...))
  t0 <- NULL
  le <- NULL
  if (opafit$is.plot.legend) {
    if (is.logical(opafit$threshold)) 
      if (opafit$threshold) {
        if (is.logical(opadata$threshold)) {
          if (opadata$threshold) 
            warning("opafit$threshold and opadata$threshold are logical values but numeric values were expected. Proceeding...")
        }
        else {
          t0 <- opadata$threshold
        }
      }
    if (is.numeric(opafit$threshold)) 
      t0 <- opafit$threshold
    si <- function(number) signif(number, opafit$signif)
    li <- list()
    if (opadata$in.legend) {
      li[[10]] <- bsll(legend = paste0("ranks = ", opafit$pp[1], 
                                       ifelse(is.null(t0), "",
                                              paste0(" (t0 = ", si(t0), 
                                                                      ")"))), col = opadata$col, pch = opadata$pch, 
                       lwd = opadata$lwd.points)
      li[[15]] <- bsll(legend = paste0("n (fail | cens.) = ", 
                                       fit$n, " (", fit$fail, " | ", fit$cens, ")"))
    }
    if (opafit$in.legend) {
      li[[20]] <- bsll(legend = paste0(fit$options$dist, 
                                       " (", paste0(fit$options$method.fit, collapse = ", "), 
                                       ")"), col = opafit$col, lwd = opafit$lwd, lty = opafit$lty)
      li[[30]] <- bsll(legend = ifelse(is.null(fit$rate), 
                                       NA, paste0("rate = ", si(fit$rate))))
      li[[40]] <- bsll(legend = ifelse(is.null(fit$meanlog), 
                                       NA, paste0("mean(log) = ", si(exp(fit$meanlog)), 
                                                  " (", si(fit$meanlog), ")")))
      li[[50]] <- bsll(legend = ifelse(is.null(fit$sdlog), 
                                       NA, paste0("sd(log) = ", si(exp(fit$sdlog)), 
                                                  " (", si(fit$sdlog), ")")))
      li[[60]] <- bsll(legend = ifelse(is.null(fit$beta), 
                                       NA, paste0("beta = ", si(fit$beta))))
      li[[70]] <- bsll(legend = ifelse(is.null(fit$eta), 
                                       NA, paste0("eta = ", si(fit$eta))))
      li[[80]] <- bsll(legend = ifelse(is.null(fit$t0), 
                                       NA, paste0("t0 = ", si(fit$t0))))
      if (!is.null(fit$gof) && opafit$in.legend.gof) {
        if (!is.null(fit$gof$r2)) {
          if (!is.null(fit$gof$ccc2)) {
            li[[100]] <- bsll(legend = paste0("r^2 | CCC^2 = ", 
                                              si(fit$gof$r2), " | ", si(fit$gof$ccc2), 
                                              ifelse(fit$gof$r2 >= fit$gof$ccc2, " (good)", 
                                                     " (BAD)")))
          }
          else {
            li[[100]] <- bsll(legend = paste0("r^2 = ", 
                                              si(fit$gof$r2)))
          }
        }
        if (!is.null(fit$gof$loglik)) {
          li[[110]] <- bsll(legend = paste0("loglik = ", 
                                            si(fit$gof$loglik)))
        }
        li[[120]] <- bsll(legend = ifelse(is.null(fit$gof$prr), 
                                          NA, paste0("prr = ", si(fit$gof$prr), " (S=", 
                                                     ifelse(is.null(fit$gof$S), "NA", fit$gof$S), 
                                                     ")")))
      }
    }
    leconf <- legendConf(fit, "blives", opadata = opadata, 
                         ...)
    if (!is.null(leconf)) 
      li[[130]] <- bsll(legend = "")
    li <- c(li, leconf)
    removeBadLegendEntries <- function(e) {
      if (!is.null(e)) 
        !is.na(e$legend)
      else FALSE
    }
    if (length(li) > 0) 
      li <- li[sapply(li, removeBadLegendEntries)]
    else li <- ""
    fu <- function(x, i) {
      if (i %in% names(x)) 
        x[[i]]
    }
    fu2 <- function(i, x) {
      lapply(x, fu, i = i)
    }
    items <- c("legend", "lty", "lwd", "pch", "col")
    le <- lapply(items, fu2, li)
    names(le) <- items
    if (identical(label <- opafit$label, "")) 
      label <- NULL
    le$rect <- legend("bottomright", legend = le$legend, 
                      title = label, cex = opafit$legend.text.size, plot = FALSE)$rect
    le$label <- opafit$label
    le$legend.text.size <- opafit$legend.text.size
  }
  le
}

