## Tries to find the closest string that does not self-intersect (or,
## equivalently, the most distant string that self intersects)

rm(list=ls())

source("usefulfuncs.R") # defines stringpoints()

pars <- c(eel=1)  # dummy

## plot setup:
jj <- 6
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',axes=FALSE,,xlab='',ylab='')


dist <- exp(seq(from=log(1.01),to=log(6.5),len=40))
polargrid(rlab=6.5)
## setup ends

dist <- seq(
    from = 1.076109317,   
    to   = 1.076109317,      #  1.076109317 is OK; but 1.076109318 is not (this value of closest_approach does not self intersect)
    len=3)



thetamax <- 0.1 + dist*0  # default value, (almost) always do-able


cuts <- c(        1,   100)
vals <- c(  NA,      pi )
f <- fun(cuts,vals)


## Thus f(1.03) = pi, f(1.12) = 2.8

for(i in seq_along(dist)){
  tseq <- seq(from=0,to=f(dist[i]),len=100)
  xy <- stringpoints(y_start=dist[i],initial_string_angle = 0,theta=tseq)
  points(xy,type='l',col=rainbow(length(dist))[i])
  
  xy <- stringpoints(y_start=dist[i],initial_string_angle = 0, theta=-tseq)
  points(xy,type='l',col=rainbow(length(dist))[i])
}

# mask strings too far from the black hole:
rect_coords <- cbind(c(7,30,30,7),c(-0.5,-0.5,0.5,0.5))
for (theta in seq(from=0,to=2*pi,len=100)){
  jjxy <- rect_coords %*% rotmat(theta)
  polygon(jjxy[,1],jjxy[,2],col='white',border=NA)
}

event_horizon()
