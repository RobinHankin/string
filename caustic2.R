## This function plots a coffee-cup caustic, or a cardioid, and uses
## mask() to eliminate superfluous plotting.

## Rays of light emerge from the circumference of the circle, at the 6
## o'clock position, coordinates (0,-1).

source("usefulfuncs.R")

## Minor trig ratios (not implemented in base R, for some reason):
`cosec` <- function(x){1/sin(x)}
`cot` <- function(x){1/tan(x)}
`sec` <- function(x){1/cos(x)}

## First plot the bounding circle:
a <- seq(from=0,to=2*pi,len=100)  # 'a' for angle
plot(sin(a),cos(a),asp=1,type='l',axes=FALSE,xlab='',ylab='')





x <- seq(from=-0.99,to=0.99,len=50)
for(i in x){
  # vertical ray first:	
  segments(x0=i,y0=-2,y1=sqrt(1-i^2),lwd=0.3)
  # now reflected ray;
  x0 <- i
  y0 <- sqrt(1-i^2)
  angle <- pi/2-2*asin(i)
  x1 <- x0 - cos(angle)
  y1 <- y0 - sin(angle)
  segments(x0=x0,y0=y0,x1=x1,y=y1,lwd=0.3)
  }  



