## lots of blue strings with varying closest approach distances.  The
## emphasis here is on self-intersecting strings.

## This script runs standalone but is designed to be called from
## maker.R [cf Makefile], which creates the pdf file
## "closest_approach2.pdf"

source("usefulfuncs.R") # defines stringpoints()


mask <- TRUE
pars <- c(eel=1)  # dummy

## plot setup:
jj <- 6
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',axes=FALSE,,xlab='',ylab='')

polargrid(rlab=6.5)
## setup ends

dist <- exp(seq(from=log(1.01), to=log(1.076109317),len=300))
dist <- exp(seq(from=log(1.005), to=log(1.076109317),len=3000))[1:200]

thetamax <- 0.1 + dist*0  # default value, (almost) always do-able

cutoffmatrix <- matrix(c(
    1.000, pi + 0.00  ,
    1.004, pi + 1.70  ,
    1.005, pi + 1.70  ,
    1.006, pi + 1.69  ,
    1.007, pi + 1.55  ,
    1.008, pi + 1.45  ,
    1.009, pi + 1.35  ,
    1.010, pi + 1.27  ,
    1.011, pi + 1.20  ,
    1.012, pi + 1.20  ,
    1.013, pi + 1.15  ,
    1.015, pi + 1.05  ,
    1.017, pi + 0.91  ,
    1.020, pi + 0.83  ,
    1.025, pi + 0.71  ,
    1.030, pi + 0.57  ,
    1.035, pi + 0.44  ,
    1.040, pi + 0.40  ,
    1.045, pi + 0.30  ,
    1.050, pi + 0.20  ,
    1.060, pi + 0.11  ,
    1.070, pi + 0.05  ,
    1.080, pi + 0.00
    ),byrow=TRUE,ncol=2)
f <- fun(cutoffmatrix[,1],cutoffmatrix[,2])


## Thus f(1.03) = pi, f(1.12) = 2.5

for(i in seq_along(dist)){
  tseq <- seq(from=0,to=f(dist[i]),len=100)
  xy <- stringpoints(y_start=dist[i],initial_string_angle = 0,theta=tseq)
  points(xy,type='l',col=rainbow(length(dist))[i])
  
#  xy <- stringpoints(y_start=dist[i],initial_string_angle = 0, theta=-tseq)
#  points(xy,type='l',col=rainbow(length(dist))[i])
}

## mask strings too far from the black hole:
if(mask){
  xy <- cbind(c(7,30,30,7),c(-0.5,-0.5,0.5,0.5))
  for (theta in seq(from=0,to=2*pi,len=100)){
    jjxy <- xy %*% rotmat(theta)
    polygon(jjxy[,1],jjxy[,2],col='white',border=NA)
  }
}

event_horizon()
