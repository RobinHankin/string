## light paths near a black hole; uses u=1/r.

## ODE is d^2u/d phi^2 = 3u^2/2-u  

rm(list=ls())

source("usefullightfuncs.R") # defines lightstringpoints()

## plot setup:
jj <- 6
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',axes=FALSE,,xlab='',ylab='')

r_start <- 3.00001


pars <- c(b=1/sqrt(1/r_start^2-1/r_start^3))
xy <- stringlightpoints(r_start=r_start, phi_start=0 , lambda=seq(from=0,to=7,len=100))

points(xy,col='red',type='l')

polargrid()
event_horizon()
