library(abrem)
library(dplyr)
source("./data/all.R")


# Method 1
dfa <- Abrem(onefm) 
dfa <- abrem.fit(dfa)
dfa <- abrem.conf(dfa, method.conf.blives="lrb", cl=.85,
                  unrel.n=25, S=100000000, lty=2, 
                  lty=1, lwd=3, col="red") 
plot(dfa)
contour.abrem(dfa)
abrem.conf(da1,method.conf.blives="lrb",cl=0.95,
           +     in.legend.blives=F,is.plot.cb=F,lty=5)

print_all <- function(data, conf.levels=.9){
  # Method 1
  dfa <- Abrem(data) 
  dfa <- abrem.fit(dfa)
  levels.calc <- as.list(conf.levels)
  
  dfa <- lapply(levels.calc, FUN = function(X){
    abrem.conf(dfa, method.conf.blives="lrb", cl=X,
               unrel.n=25, S=10000, is.plot.cb=T,  in.legend.blives=F, 
               lty=1, lwd=3, col="red") 
  })
  
  par(mfrow=c(1,2))
  plot.abrem(dfa, is.plot.legend=F)
  contour.abrem(dfa)
  par(mfrow=c(1,1))
  return(dfa)
  
}
print_all(onefm, conf.levels =seq(.5, .95, by = .05))


## OTHER ATTEMPT!!
library(abrem)
data("onefm")
weibull.data <- Abrem(onefm) 
weibull.data <- abrem.fit(weibull.data)

levels.condidence <- function(weibull.data, conf.levels= c(.5, .8, .95) ){
  levels.calc <- as.list(conf.levels)
  
  dfa <- lapply(levels.calc, FUN = function(X){
    abrem.conf(weibull.data, method.conf.blives="lrb", cl=X,
               unrel.n=25, S=10000, is.plot.cb=T,  in.legend.blives=F, 
               lty=1, lwd=3, col="red") })
  return(dfa)
  
}
list.data.plot <- lapply(levels.condidence(weibull.data), function(X){
  data.frame(
      eta = X$fit[[1]]$conf$blives[[1]]$MLEXContour[[1]]$Eta, 
      beta = X$fit[[1]]$conf$blives[[1]]$MLEXContour[[1]]$Beta, 
      Plevel =  paste0("P", X$fit[[1]]$conf$blives[[1]]$options$cl*100) 
    )
  })

df.plot <- data.table::rbindlist(list.data.plot)


df.dot <- data.frame(
  eta= weibull.data$fit[[1]]$eta, 
  beta= weibull.data$fit[[1]]$beta
)

v <- ggplot() 
v + geom_path(lwd=1) 

v + geom_polygon(data = df.plot,
                 aes(x = eta,y = beta, fill = Plevel, color=Plevel),alpha = .05, lty=5) + 
  geom_point(data=df.dot, aes(x=eta, y=beta), color="black")


v + stat_density2d(geom="polygon", aes(fill = factor(..level..)))
path <- v + geom_path(lwd=1) 
path + geom_text(data = 
                   df.plot %>% group_by(Plevel) %>% 
                   top_n(n = -1, wt = beta), 
                 aes(label=Plevel, vjust=1, hjust=1, size=2))
path +  geom_text(data=subset(df.plot,Year==2006 | Year==2014), aes(label=Year, vjust=1, hjust=1))

df.plot %>% group_by(Plevel) %>% 
  top_n(n = 1, wt = beta)


##### Other approach
##### 
p80 <- data.frame(
  eta = dfa[[1]]$fit[[1]]$conf$blives[[1]]$MLEXContour[[1]]$Eta, 
  beta = dfa[[1]]$fit[[1]]$conf$blives[[1]]$MLEXContour[[1]]$Beta, 
  Plevel =  "P50")

p90 <- data.frame(
  x = dfa[[2]]$fit[[1]]$conf$blives[[1]]$MLEXContour[[1]]$Eta, 
  y = dfa[[2]]$fit[[1]]$conf$blives[[1]]$MLEXContour[[1]]$Beta, 
  z =  "P90")

p <- rbind(p80, p90)

v  <- ggplot(p, aes(x, y, group=as.factor(z), color=z) ) 
v  + geom_contour()
v + geom_point()
v + geom_path()

ggplot(p, aes(x=x, y=y, fill=z)) +
  stat_density2d(geom="polygon", aes(fill = factor(..level..)))
