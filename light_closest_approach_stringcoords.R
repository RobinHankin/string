## Null geodesics in the Schwarzschild geometry.  Same as
## light_closest_approach.R but plots in string coordinates.

## This script runs standalone but is designed to be called from
## maker.R [cf Makefile], which creates the pdf file
## "light_closest_approach_stringcoords.pdf"

## Here, the light starts tangentially from phi=0.
## It is like closest_approach.R and closest_approach2.R but for light
## beams rather than string.  Compare light_start_at_r_equals_2.R,
## which has light beams starting from (2,0) going in all directions.


## This file uses the same ODE engine and idiom as
## light_start_at_r_equals_2.R (henceforth "lsar2"), but differs in
## detail, particularly cutoffmatrix.

## ODE is d^2u/d phi^2 = 3u^2/2-u  (where u=1/r)

source("usefulfuncs.R") # defines polargrid() etc
source("usefullightfuncs.R") # defines lightpoints()
source("stringfuncs.R") # u1() and u2() etc; lives in the schwarzschild repo

domask <- TRUE    # set to FALSE to see entire geodesic (TRUE is time-consuming but needed for publication quality)
size_of_plot <- 3.5

## plot setup:
par(xpd=TRUE)

jj <- c(-size_of_plot, size_of_plot)
plot(NULL,asp=1,xlim=jj,ylim=jj,type='l',axes=FALSE,xlab='',ylab='')

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
fmax <- approxfun(cutoffmatrix[,1],cutoffmatrix[,2])


r_start <- seq(from=1.1,to=4,len=47)

for(r in r_start){

  final_phi <- pmin(fmax(r),2*pi)
  print(r)
  o <-
    nullgeodesic(
        r_start=r,  # r=3/2 is a circular (but unstable) orbit
        dubydphistart = 0,  # going vertically at the start
        phi=seq(from=0,to=final_phi,len=100),
        include=TRUE
    )

    r <- o[,3]
    phi <- o[,4]
    u <- u1(r)
    xy <- cbind(u*cos(phi),u*sin(phi))
    points(xy,col='red',type='l')
}

## grid:
polargrid()


## Mask strings too far from the black hole:
if(domask){ mask(4) }

circ <- function(r,...){
  th <- seq(from=0,to=2*pi,len=300)
  points(r*cos(th),r*sin(th),type="l",...)
}
circ(u1(1.5),lwd=3,lty=1,col="red")   # ergosphere
#circ(4.0,lwd=3,lty=2)   # cut-off for pretty printing

legend("bottomleft",bg="white",legend=c("null geodesic","closed photon orbit"),
       lty=1,col=c("red","red"),lwd=c(1,3,1))
