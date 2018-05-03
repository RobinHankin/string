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


xy1 <-
  stringu(
      r_start=3/2-0.01,  # r=3/2 is a circular (but unstable) orbit
      dubydphistart=0,
      phi=seq(from=0,to=6,len=100)
  )

points(xy1,col='red',type='l')

xy2 <-
  stringu(
      r_start=3/2+0.01,  # r=3/2 is a circular (but unstable) orbit
      dubydphistart=0,
      phi=seq(from=0,to=6,len=100)
  )

points(xy2,col='red',type='l')



polargrid()
event_horizon()
