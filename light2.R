## light paths near a black hole; uses u=1/r.

## ODE is d^2u/d phi^2 = 3u^2/2-u  

rm(list=ls())

source("usefulfuncs.R") # defines polargrid() etc
source("usefullightfuncs.R") # defines lightstringpoints()

## plot setup:
jj <- 6
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',axes=FALSE,,xlab='',ylab='')

dummypars <- c(eel=3) # dummy

phi <- seq(from=0,to=1)
xy <-
  stringu(
      r_start=3/2,  # r=3/2 is a circular (but unstable) orbit
      dubydphistart=0,
      phi=seq(from=0,to=2,len=10)
  )

points(xy,col='red',type='l')

polargrid()
event_horizon()
