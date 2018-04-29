## lots of blue strings with varying closest approach distances.

rm(list=ls())

source("usefulfuncs.R") # defines stringpoints()

pars <- c(eel=1)  # dummy

## plot setup:
jj <- 6
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',axes=FALSE,,xlab='',ylab='')



polargrid(rlab=6.5)
## setup ends

dist <- exp(seq(from=log(1.01),to=log(6.5),len=40))

thetamax <- 0.1 + dist*0  # default value, (almost) always do-able


cuts <- c(        1,   1.06,  1.07, 1.08,   1.11,    1.13,    1.15,     1.17,  1.19,    1.21,    1.31,   1.41,  1.51, 1.7, 2.01,  5.00)
vals <- c(  NA,      pi,    pi,   3.1,   2.87,   2.79,    2.71,     2.61,   2.51,  2.4,      2.4,     2.2,   2.1,  2.0, 1.9,    1.6,     1.0)
f <- fun(cuts,vals)


## Thus f(1.03) = pi, f(1.12) = 2.5

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

event_horizon()