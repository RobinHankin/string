## light paths near a black hole

rm(list=ls())

source("usefulfuncs.R") # defines lightstringpoints()


## plot setup:
jj <- 4
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',axes=FALSE,,xlab='',ylab='')

pars <- c(E=1 , L=2*sqrt(2)-0.001)
xy <- stringlightpoints(r_start=2 , phi_start=0 , lambda=seq(from=0,to=1.2,len=30))

points(xy,col='red',type='b')

polargrid()
event_horizon()
