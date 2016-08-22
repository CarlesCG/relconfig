## plot.abrem
# function (x, ...) 
{
# x is dfa!
  if (identical(class(x), "abrem")) 
    x <- list(x)
  if (!all(sapply(x, function(x) identical(class(x), "abrem")))) {
    stop("Argument \"x\" is not of class \"abrem\" or ", 
         "a list of \"abrem\" objects.")
  }
  opa <- x[[1]]$options
  opa <- modifyList(opa, list(...))
  if (!is.null(list(...)$threshold)) 
    message("Currently, passing the 'threshold' argument to plot.abrem is not supported. Proceeding...")
  ra <- findMaxDataRange(x, opa$verbosity, opa$log)
  xlimits <- range(ra$xrange, na.rm = TRUE)
  ylimits <- range(ra$yrange, na.rm = TRUE)
  if (is.null(opa$xlim)) {
    opa$xlim <- c(10^(log10(xlimits[1]) - 0.5), 10^(log10(xlimits[2]) + 
                                                      1))
  }
  if (is.null(opa$ylim)) {
    if (ylimits[1] < 0.01) 
      opa$ylim <- c(signif(ylimits[1], 1), 0.99)
    else opa$ylim <- c(0.01, 0.99)
  }
  opanames <- names(opa)
  plotargs <- c(list(x = NA, axes = FALSE), opa[opanames %in% 
                                                  plot_default_args()])
  if (!is.null(plotargs$ylim)) {
    plotargs$ylim <- F0inv(plotargs$ylim, opa$log)
  }
  plotargs$main <- NULL
  if (!is.null(opa$mar)) 
    par(mar = opa$mar)
  if (!is.null(opa$mai)) 
    par(mai = opa$mai)
  do.call(plot.default, plotargs)
  if (opa$is.plot.grid) {
    abline(h = F0inv(seq.wb(opa$ylim[1]/10, 1 - (1 - opa$ylim[2])/10), 
                     opa$log), v = seq.log(opa$xlim[1]/10, opa$xlim[2] * 
                                             10, seq(0, 10, 1)), col = opa$col.grid)
  }
  r <- seq.log(opa$xlim[1]/10, opa$xlim[2] * 10, c(1, 5))
  for (t in c(1, 3)) {
    axis(t, at = seq.log(opa$xlim[1]/10, opa$xlim[2] * 10, 
                         seq(0, 10, 0.2)), labels = NA, tcl = -0.25)
    axis(t, at = r, labels = r, tcl = -0.75)
  }
  r <- c(seq.wb(opa$ylim[1]/10, 1 - (1 - opa$ylim[2])/10, c(1, 
                                                            2, 5)), 0.9)
  for (t in c(2, 4)) {
    axis(t, at = F0inv(seq.wb(opa$ylim[1]/10, 1 - (1 - opa$ylim[2])/10), 
                       opa$log), labels = NA, tcl = -0.25)
    axis(t, at = F0inv(r, opa$log), labels = r * 100, tcl = -0.75)
  }
  abline(h = 0, lty = 3, col = opa$col.grid)
  title(main = opa$main, line = 3)

  plotConfsInAbrem <- function(abrem) {
    if (!is.null(abrem$fit)) {
      ret <- lapply(abrem$fit, plotConfsInFit, opadata = abrem$options, 
                    ...)
    }
    else {
      if (!is.null(opa)) 
        if (opa$verbosity >= 1) 
          message("plotConfsInAbrem: This Abrem object contains no fits ", 
                  "or confidence calculations.")
    }
  }
  lapply(x, plotConfsInAbrem)
  
  plotFitsInAbrem <- function(abrem) {
    opadata <- modifyList(abrem$options, list(opa$xlim, opa$ylim))
    if (!is.null(abrem$fit)) {
      ret <- lapply(abrem$fit, plotSingleFit, opadata = opadata, 
                    ...)
    }
    else {
      if (!is.null(opa)) 
        if (opa$verbosity >= 1) 
          message("plotFitsInAbrem: This Abrem object contains no fits.")
    }
  }
  lapply(x, plotFitsInAbrem)
  
  plotSingleDataSet <- function(x) {
    if (opa$is.plot.pp) {
      opadata <- modifyList(x$options, list(...))
      if (!is.null(x$data) && !is.null(ti <- x$data$time) && 
          !is.null(ra <- x$data[, paste0("rank.", opadata$pp[1])])) {
        t0 <- 0
        if (is.logical(opadata$threshold)) 
          if (opadata$threshold) 
            warning("opadata$threshold is a logical value but numeric value was expected. Proceeding...")
        if (is.numeric(opadata$threshold)) 
          t0 <- opadata$threshold
        points(ti - t0, F0inv(ra, opadata$log), pch = opadata$pch, 
               col = opadata$col, lwd = opadata$lwd.points, 
               cex = opadata$cex.points)
      }
      else {
        stop("This Abrem object contains no probability plot positions.")
      }
    }
  }
  lapply(x, plotSingleDataSet)
  
  lolegends <- NULL
  buildListOfLegends <- function(abrem) {
    ret <- NULL
    if (!is.null(abrem$fit) && any(sapply(abrem$fit, function(fi) !is.null(fi)))) {
      ret <- lapply(abrem$fit, buildSingleFitLegend, opadata = abrem$options, 
                    ...)
    }
    else {
      if (abrem$options$is.plot.legend && opa$is.plot.legend) {
        ret <- list(buildSingleDataLegend(abrem, opadata = abrem$options, 
                                          ...))
        if (!is.null(opa)) 
          if (opa$verbosity >= 1) 
            message("buildListOfLegends: This Abrem object contains no fits.")
      }
    }
    ret
  }
  lolegends <- unlist(lapply(x, buildListOfLegends), FALSE)
  lolegends <- lolegends[sapply(lolegends, function(lol) !is.null(lol))]
  
  plotSingleLegend <- function(le, x, y) {
    if (identical(label <- le$label, "")) 
      label <- NULL
    if (is.null(le$legend)) 
      le$legend <- ""
    legend(x = x, y = y, legend = le$legend, title = label, 
           cex = le$legend.text.size, bg = "white", lty = unlist(le$lty), 
           lwd = unlist(le$lwd), pch = unlist(le$pch), col = unlist(le$col), 
           text.col = "black", xpd = TRUE)
  }
  if (!is.null(lolegends) && any(sapply(lolegends, function(lol) !is.null(lol)))) {
    lx <- rep(lolegends[[1]]$rect$left, length(lolegends))
    ly <- lolegends[[1]]$rect$top + c(0, cumsum(sapply(lolegends, 
                                                       function(le) le$rect$h)[-1]))
    if (opa$log %in% c("x", "xy", "yx")) 
      lx <- 10^lx
    if (opa$log %in% c("y", "xy", "yx")) 
      ly <- 10^ly
    for (i in 1:length(lolegends)) {
      plotSingleLegend(lolegends[[i]], lx[i], ly[i])
    }
  }else{
    if (opa$verbosity >= 1) 
      message("plot.abrem: There is no legend to plot.")
  }
  invisible()
}



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
