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

f <- function(a){## gives the Cartesian coords of the caustic as a function of 'a'
  x <- (
    2*sin(2*a) + 2*cos(2*a)*cot(3*a) - 3*cosec(3*a)^2*sin(2*a)
  )/(
    -3*cosec(3*a)^2
  )
  
  y <- cos(2*a) + cot(3*a)*(x-sin(2*a))
  return(cbind(x,y))
}

n <- 100
maxangle <- pi/2
a <- seq(from=maxangle/n,to=maxangle,len=n+1)

## Now plot the caustic itself:
points(f(a),type='l',col='red')

## Now rays from (0,-1) to the circumference of the circle at angle
## 'a' [interpreted as bearing]:
if(FALSE){
  segments(x0=0, y0=-1, x1=sin(2*a), y1=cos(2*a), lwd=0.1,col='gray')
}

# now the reflected rays (whose envelope forms the caustic):
for(aa in a){
  abline(
      a = cos(2*aa) -  sin(2*aa)*tan(pi/2-3*aa),
      b = tan(pi/2-3*aa),
      lwd = 0.2 
  )
}

## Vertical line is indeterminate, has to be plotted explicitly:
segments(0,-1,0,1,lwd=0.2)

## Mask outside of circle:
mask(1.001)
