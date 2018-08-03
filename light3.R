## Same as light2.R but also plots some circles for comparison

## ODE is d^2u/d phi^2 = 3u^2/2-u  


source("usefulfuncs.R") # defines polargrid() etc
source("usefullightfuncs.R") # defines lightstringpoints()


r_start <- 1.9 # starting radius for light ray

## plot setup:
jj <- 10
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',axes=FALSE,,xlab='',ylab='')

`circ` <- function(center,r,...){
  theta <- seq(from=0,to=2*pi,len=100)
  points(center[1]+r*cos(theta),center[2]+r*sin(theta),type='l',...)
  return(0)
}

f <- function(r){2*r^2/3}  # radius of curvature of a ray at radius r moving tangentially
`mycirc` <- function(x,r,...){circ(c(x-f(r),0),f(r),...)}  # shows an osculating circle

dummypars <- c(eel=3) # dummy


xy1 <-
  stringu(
      r_start=r_start,  # r=3/2 is a circular (but unstable) orbit
      dubydphistart=0,
      phi=seq(from=0,to=pi/1.3,len=100)
  )

points(xy1,col='red',type='l')
points(xy1[,1],-xy1[,2],type='l',col='red')
mycirc(r_start,f(r_start),col='blue')
circ(c(0,0),r_start,col='blue')


polargrid()
event_horizon(fill=TRUE)


