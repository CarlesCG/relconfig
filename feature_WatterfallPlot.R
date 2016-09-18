# Vizualize your plot in 3D
#

############## 1st attemp ############
n <- 60
m <- 50
x <- seq(-4,4, len=m)

# Make up some fake y data

y <- matrix(NA, n, m)
for (i in 1:n) y[i,] <- dnorm(x)*runif(m, 0.5,1)

par(bg="black")
yrange <- range(c(y, y+n/20))

plot(x, x, type="n", axes=FALSE, bg="black", ylim=yrange)

for (i in n:1) {
  y1 <- c(y[i,] + i/20, 0, 0)
  x1 <- c(x, x[m], x[1])
  polygon(x1,y1,col="black")
  
  lines(x, y[i,] + i/20, col="white")
  
}


############## 2nd attemps ############## 
slicedens<-function(x,y,slices=50,lboost=1,gboost=1,xinc=0,yinc=0.01,
                    bcol="black",fcol="black",lcol="white",lwidth=1) {
  ycut <- min(y)+((0:(slices))*(max(y)-min(y))/slices)
  height<-gboost*((slices*yinc)+max(density(x)$y))
  plot( c(min(x),max(x)+((max(x)-min(x))/4)),
        c(0,height),
        xaxt="n",yaxt="n",ylab="",xlab="")
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=bcol)
  for(i in slices:1) {
    miny<-ycut[i]
    maxy<-ycut[i+1]
    gx<-(i-1)*(max(x)-min(x))*xinc
    gy<-(i-1)*(height)*yinc
    dd<-density(x[y>=miny & y<maxy])
    polygon(dd$x+gx,lboost*dd$y+gy,col=fcol)
    lines(dd$x+gx,lboost*dd$y+gy,col=lcol,lwd=lwidth)
  }
}

# Example:
y<-runif(5000,min=-1,max=1)
x<-runif(5000,min=-1,max=1)+rnorm(5000,mean=1/(y+1.1),sd=0.8-(y*0.5))
slicedens(x,y,lboost=0.2,fcol=rgb(0,0,0,200,maxColorValue=255))

df <- readr::read_csv("./data/Weibull_template.csv")


slicedens(x = df$row_id, y = df$time, slices=3,
          lboost= 0.2, fcol= rgb(0,0,0,200,maxColorValue=255))


## 3rd ###
library(plotly)
kd <- with(df, MASS::kde2d( time, event,  n = 20))
with(kd, plot_ly(x = x, y = y, z = z, type = "surface"))


## Maybe and heaxabin?
library(tidyverse)
ggplot(df, aes(x= time, y=row_id,  
               colour= as.factor(event)) ) + 
  geom_point() + geom_line() 

ggplot(df, aes(x= time, fill=as.factor(event))) + 
  geom_density(alpha=.2) +
  scale_fill_manual(name=" ", 
                    values=c("green","red"), 
                    labels=c("Survived", "Failed")) +
  geom_vline(data= df %>% 
               filter(event ==1) %>% 
               summarise( stat = median(time, na.rm = T) ), 
             aes(xintercept=stat),
             linetype="dashed", size=1) +
  theme_bw() +
  ggtitle("Where did the event take place?")

## Other 
install.packages("heatmaply")
library(heatmaply)
heatmaply(mtcars, k_col = 2, k_row = 3) 
df %>% select(time, event) %>% 
heatmaply(x= ., kcol=2 )

