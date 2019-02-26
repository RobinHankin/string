## Null geodesics.  Here, the light starts tangentially from phi=0.
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
mask <- FALSE    # set to FALSE to see entire geodesic (TRUE is time-consuming but needed for publication quality)
size_of_plot <- 3

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
    1,pi/2,
    1.5,pi/2 + 0.1,
    1.6,pi/2 + 2.0,
    1.7,pi/2 + 1,
    3.0,pi/2 + 0.3,
    3.7,pi/4,
    4.001,pi/6
    )
   ,ncol=2,byrow=TRUE)

colnames(cutoffmatrix) <- c("cuts","vals")
fmax <- fun(cutoffmatrix[,1],cutoffmatrix[,2])


r_start <- seq(from=0.5,to=4,len=200)

for(r in r_start){

  final_phi <- fmax(r)
  print(final_phi)
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
if(mask){
  xy <- cbind(c(4,30,30,4),c(-0.5,-0.5,0.5,0.5))
  howmany <- 100  # 100 for production, 10 for testing
  for (theta in seq(from=0,to=2*pi,len=howmany)){
    jjxy <- xy %*% rotmat(theta)
    polygon(jjxy[,1],jjxy[,2],col="white",border=NA) # white for production
  }
}

circ <- function(r,...){
  th <- seq(from=0,to=2*pi,len=300)
  points(r*cos(th),r*sin(th),type="l",...)
}
circ(1.5,lwd=3,lty=2)   # ergosphere
circ(4.0,lwd=3,lty=2)   # cut-off for pretty printing


event_horizon(fill=FALSE)


