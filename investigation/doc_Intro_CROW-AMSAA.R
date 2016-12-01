# http://blogs2.datall-analyse.nl/2016/02/17/rcode_crow_amsaa/

library(bbmle)

#data from Crow (2011), Example 2

#lower limit of time period
HoursL <- c(0, 62, 100, 187, 210, 350)
#upper limit of time period
HoursU <- c(62, 100, 187, 210, 350, 500)
#number of failures within each time period
Failures <- c(12,6,15,3,18,16)
#cumulative failures
CF <- cumsum(Failures)

grData <- data.frame(HoursL,HoursU,Failures,CF,MTBF=HoursU/CF)

#obtain start value for maximum likelihood optimization:
#since the Crow-AMSAA model is related to the Duane model
#the Duane model will be used for obtaining start values

#fit Duane model to grouped data
sv <- lm(log(MTBF) ~ log(HoursU), data=grData)

#start values for Crow-AMSAA parameters (i.e., lambda and beta)
lambda <- 1/exp(coef(sv)[1])
beta <- 1-coef(sv)[2]



#log-likelihood function of Crow-AMSAA model for grouped data
#note: log transformation of parameters enforces the parameters to be positive
#specifically, the log transformation of the parameters ensures that
#the limits of the Fisher matrix confidence bounds do not take negative values
llikCROWg <- function (lambda, beta) {
  l <- exp(lambda)
  b <- exp(beta)
  -(sum(ns*log(l*TU^b - l*TL^b) - (l*TU^b - l*TL^b)) )
}

#optimization of log-likelihood
NHPP.mle <- mle2(minuslogl=llikCROWg, optimizer="optim", method="BFGS",
                 data=list(TU=HoursU, TL=HoursL,
                           ns=Failures),
                 start=list(lambda=log(lambda), beta=log(beta)))

#MLEs of parameters
#note: the estimates are identical to those reported by Crow (2011)
exp(coef(NHPP.mle))
#log-likelihood of model
(loglikModel <- -NHPP.mle@min)
#variance covariance matrix
vcovNHPP <- exp(coef(NHPP.mle))%*%t(exp(coef(NHPP.mle)))*vcov(NHPP.mle)
rownames(vcovNHPP) <- colnames(vcovNHPP)
round(vcovNHPP, 8)
#standard errors of parameters
(stErr <- sqrt(diag(vcovNHPP)))
#normal-approximation 95% confidence intervals (=quadratic approximation)
#these intervals are also known as Fisher matrix confidence bounds
round(exp(confint(NHPP.mle, level=0.95, method="quad")), 4)



#it is also possible to obtain likelihood based intervals
round(exp(confint(NHPP.mle, level=0.95)), 4)

#profiling lambda
profile.parm <- function(ps) {
  plkmu <- rep(0, length(ps))
  
  for (i in 1:length(ps)) {
    temp <- mle2(minuslogl=llikCROWg, optimizer="optim", method="BFGS",
                 data=list(TU=HoursU, TL=HoursL,
                           ns=Failures),
                 fixed=list(lambda=ps[i]),
                 start=list(beta=log(beta)))
    plkmu[i] <- -temp@min
  }
  plkmu
}

#MLE for lambda
exp(coef(NHPP.mle))[1]
#fixed values for lambda
ps <- log(seq(0.09, 1.6, length.out=30))

#likelihood profile
ll <- profile.parm(ps)

#plot profile likelihood
plot(exp(ps),ll, type='l', xlab=expression(lambda), ylab="log-likelihood")
#include lower limit for log-likelihood values
limit <- loglikModel - .5*qchisq(.95, df=1)
abline(h=limit,col="blue",lty=2)
#95% confidence limits of likelihood based interval
#limits were obtained with confint() above
abline(v=c(0.1051, 1.5139), col="red", lty=2)



#profiling beta
profile.parm <- function(ps) {
  plkmu <- rep(0, length(ps))
  
  for (i in 1:length(ps)) {
    temp <- mle2(minuslogl=llikCROWg, optimizer="optim", method="BFGS",
                 data=list(TU=HoursU, TL=HoursL,
                           ns=Failures),
                 fixed=list(beta=ps[i]),
                 start=list(lambda=log(lambda)))
    plkmu[i] <- -temp@min
  }
  plkmu
}

#MLE for beta
exp(coef(NHPP.mle))[2]
#fixed values for Beta
ps <- log(seq(0.5, 1.1, length.out=30))

#likelihood profile
ll <- profile.parm(ps)

#plot profile likelihood
plot(exp(ps),ll, type='l', xlab=expression(beta), ylab="log-likelihood")
#include lower limit for log-likelihood values
limit <- loglikModel - .5*qchisq(.95, df=1)
abline(h=limit,col="blue",lty=2)
#95% confidence limits of likelihood based interval
#limits were obtained with confint() above
abline(v=c(0.6208, 1.0433), col="red", lty=2)



#finally, explore the log-likelihood function in the neighborhood
#of the start values
#more specifically, locate in a contour plot of log-likelihood values
#the positions of both the start values (obtained with the Duane model)
#and the MLEs for beta and lambda of the Crow-AMSAA model
llikCROWgexp <- function (theta, TU, TL, ns) {
  l <- exp(theta[1])
  b <- exp(theta[2])
  sum(ns*log(l*TU^b - l*TL^b) - (l*TU^b - l*TL^b))
}

#construct grid and calculate log-likelihood values
lambdavalues <- log(seq(.3,.6,length.out=75))
betavalues <- log(seq(.7,.9,length.out=75))
lbgrid <- expand.grid(lambda=lambdavalues, beta=betavalues)
lbgrid$ll <- apply(lbgrid, 1, llikCROWgexp, ns=Failures,
                   TU=HoursU, TL=HoursL)
ll.matrix <- matrix(lbgrid$ll, nrow=length(lambdavalues))

#contour plot of log-likelihood values
contour(exp(lambdavalues), exp(betavalues), ll.matrix,
        levels=seq(80.5,110.5,5),
        xlab=expression(lambda), ylab=expression(beta))
#position of start values for lambda and beta (blue cross)
points(x=lambda, y=beta, col="blue", pch=4)
#position of MLEs for lambda and beta (red dot)
points(x=exp(coef(NHPP.mle))[1], y=exp(coef(NHPP.mle))[2], col="red", pch=20)

