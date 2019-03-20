## Null geodesics in the Schwarzschild geometry.

## This script runs standalone but is designed to be called from
## maker.R [cf Makefile], which creates the pdf file
## "light_closest_approach.pdf"

## Here, the light starts tangentially from phi=0.
## It is like closest_approach.R and closest_approach2.R but for light
## beams rather than string.  Compare light_start_at_r_equals_2.R,
## which has light beams starting from (2,0) going in all directions.

## The filename is a bit of a misnomer because the closest approach of
## the light beams is not the start value for r_start < 1.5 (recall
## that there is a circular photon orbit at radius 3M).  Photons that
## start at less than 1.5 are therefore inside this circular orbit and
## fall inwards.

## This file uses the same ODE engine and idiom as
## light_start_at_r_equals_2.R (henceforth "lsar2"), but differs in
## detail, particularly cutoffmatrix.

## ODE is d^2u/d phi^2 = 3u^2/2-u  (where u=1/r)

source("usefulfuncs.R") # defines polargrid() etc
source("usefullightfuncs.R") # defines lightpoints()

r_start <- 2 # starting radius for light ray
domask <- TRUE    # set to FALSE to see entire geodesic (TRUE is time-consuming but needed for publication quality)
size_of_plot <- 3.5

## plot setup:
par(xpd=TRUE)

jj <- c(-size_of_plot, size_of_plot)
plot(NULL,asp=1,xlim=jj,ylim=jj,type='l',axes=FALSE,xlab='',ylab='')


## The chief numerical difficulty here is defining the maximum angle
## that the path (here a light ray, elsewhere a taut string) can
## attain.  We are starting at (r=2,phi=0) and indexing by angle made
## by the string to the tangent at that point.  We need to determine
## what the maximum value of phi is.  For example, consider flat space
## and a light ray starting tangentially at (2,0).  Then the maximum
## value of phi would be pi/2.  In curved Schwarzschild space, this
## angle is more than pi/2.

## Variable 'cutoffmatrix' has two columns.  The first gives the
## radius of the string at its start point (compare lsar2 where the
## two columns are starting direction and finishing angle thetamax).
## The second column is the same as lsar2: it gives the maximum value
## of phi to integrate to.  Make this too big, and the Runge-Kutta
## integration fails, and returns an error; make it too small and the
## string terminates too early and looks bad (because it is soooo much
## nicer to have the string [apparently] go to infinity than have a
## free end in space).

cutoffmatrix  <- matrix(c(
    0,0.001 ,
    0.01,0.2,
    0.02,0.3,
    0.05,0.4,
    0.07,0.5,
    0.08,0.5,
    0.1,0.7,
    0.2,1.0,
    0.5,1.8,
    0.7,2.2,
    0.8,2.5,
    0.9,2.8,

    1,3,
    1.1, pi+0.2,
    
    1.2,pi + 0.6,
    1.3,pi + 1.1,

    1.35,pi + 1.4,
    1.40,pi + pi/2+0.3,
    1.41,pi + pi/2+0.3,
    1.42,pi+2.2,
    1.43,pi+2.3,
    1.44,pi+2.4,
    1.47,pi+pi          ,
    1.48,pi+pi,
    1.5,2*pi,
    1.505,2*pi+0.1,
    1.51,6,
    1.52,5.1,
    1.53,5,
    1.54,4.7,
    1.55,4.6,
    1.57,4,
    1.6,3.9,
    1.7,pi/2 + 1.1,
    1.9,pi/2 + 0.9,
    2.0,pi/2 + 0.7,
    2.5,pi/2 + 0.6,
    3.0,pi/2 + 0.1,
    3.7,pi/4,
    4.001,pi/6
    )
   ,ncol=2,byrow=TRUE)

colnames(cutoffmatrix) <- c("cuts","vals")
fmax <- fun(cutoffmatrix[,1],cutoffmatrix[,2])
fmax <- approxfun(cutoffmatrix[,1],cutoffmatrix[,2])


r_start <- seq(from=0.5,to=4,len=200)
r_start <- seq(from=0.01,to=4,len=70)

for(r in r_start){

  final_phi <- pmin(fmax(r),2*pi)
  print(r)
  xy1 <-
    stringu(
        r_start=r,  # r=3/2 is a circular (but unstable) orbit
        dubydphistart = 0,  # going vertically at the start
        phi=seq(from=0,to=final_phi,len=100)
    )
  points(xy1,col='red',type='l')
}

## grid:
polargrid()


## Mask strings too far from the black hole:
if(domask){ mask(4) }

circ <- function(r,...){
  th <- seq(from=0,to=2*pi,len=300)
  points(r*cos(th),r*sin(th),type="l",...)
}
circ(1.5,lwd=3,lty=1,col="red")   # ergosphere
#circ(4.0,lwd=3,lty=2)   # cut-off for pretty printing


event_horizon(fill=FALSE)

legend("bottomleft",bg="white",legend=c("null geodesic","closed photon orbit","event horizon"),
       lty=1,col=c("red","red","black"),lwd=c(1,3,1))
