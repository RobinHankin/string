## light paths near a black hole

rm(list=ls())

source("usefulfuncs.R") # defines lightstringpoints()


## plot setup:
jj <- 6
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',axes=FALSE,,xlab='',ylab='')

r_start <- 3.6

maximal_L <- function(E,r){E*r*sqrt(r/(r-1))}

E <- 1
pars <- c(E=E , L=maximal_L(E,r_start)-0.01)
xy <- stringlightpoints(r_start=r_start, phi_start=0 , lambda=seq(from=0,to=7,len=30))

points(xy,col='red',type='b')

polargrid()
event_horizon()
