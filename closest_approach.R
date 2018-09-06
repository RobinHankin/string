## lots of blue strings with varying closest approach distances.


## This script runs standalone but is designed to be called from
## maker.R [cf Makefile], which creates the pdf file
## "closest_approach.pdf"

source("usefulfuncs.R") # defines stringpoints()

pars <- c(eel=1)  # dummy

## plot setup:
jj <- 6
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',axes=FALSE,,xlab='',ylab='')
## setup ends


dist <- dseq(from=1.076109317, to=7.5, len=100, power=2)

cutoffmatrix <- matrix(c(
 1.060, pi  ,
 1.076, 3.1 ,
 1.077, 3.1 ,
 1.078, 3.1 ,
 1.079, 3.1 ,
 1.080, 3.0 ,
 1.090, 3.0 ,
 1.10, 2.90 ,
 1.11, 2.87 ,
 1.13, 2.79 ,
 1.15, 2.71 ,
 1.17, 2.61 ,
 1.19, 2.51 ,
 1.21, 2.45 ,
 1.25, 2.42 ,
 1.31, 2.37 ,
 1.32, 2.30 ,
 1.33, 2.30 ,
 1.35, 2.30 ,
 1.41, 2.20 ,
 1.51, 2.10 ,
 1.70, 2.00 ,
 1.80, 1.90 ,
 1.90, 1.90 ,
 2.00, 1.90 ,
 2.10, 1.90 ,
 2.20, 1.60 ,
 2.40, 1.60 ,
 5.00, 1.60 ,
 9.00, 1.60 ),
 ncol=2,byrow=TRUE)

f <- fun(cutoffmatrix[,1],cutoffmatrix[,2])

for(i in seq_along(dist)){
  tseq <- seq(from=0,to=f(dist[i]),len=100)
  xy <- stringpoints(y_start=dist[i],initial_string_angle = 0,theta=tseq)
  points(xy,type='l',col=rainbow(length(dist))[i])
  
  xy <- stringpoints(y_start=dist[i],initial_string_angle = 0, theta=-tseq)
  points(xy,type='l',col=rainbow(length(dist))[i])
}

# mask strings too far from the black hole:
xy <- cbind(c(7,30,30,7),c(-0.5,-0.5,0.5,0.5))
for (theta in seq(from=0,to=2*pi,len=100)){
  jjxy <- xy %*% rotmat(theta)
  polygon(jjxy[,1],jjxy[,2],col='white',border=NA)
}


polargrid(rlab=6.5)
event_horizon()
