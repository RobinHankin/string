rm(list=ls())


pars <- c(eel=1)  # dummy

source("usefulfuncs.R")

## setup:
jj <- 2.5
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),axes=T,xlab='',ylab='') # for production
plot(NULL,asp=1,xlim=c(-1,2),ylim=c(-2,0),axes=FALSE,xlab='',ylab='')  # for testing


closest_approach <- matrix(0,0,2)  # used for debugging theta_end2 values
colnames(closest_approach) <- c("start_angle","closest_approach")
dist_from_hole <- 2

gap <- 0.006
start_angles <-
  c(seq(from=pi/2-gap,by= -gap,to=0.91))

## one string per start angle, each a different colour; start_angle =
## pi/2 is a radial string
             
# start_angles <- seq(from=0.89,to=.91,len=10)  # use this in conjunction with plot(closest_approach) to see where the upper limit of 0.9 comes from


start_angles <- sort(start_angles,decreasing=TRUE)

n <- length(start_angles)
cols <- rainbow(n+round(n/7))

theta_start <- 0

theta_end1 <- 0 + 0*start_angles ## upward/outward
theta_end2 <- pi*0.05 + 0*start_angles  # downward/inward

theta_end2[cont(start_angles, c(0.10,0.50))] <-  pi
theta_end2[cont(start_angles, c(0.50,1.00))] <-  pi
theta_end2[cont(start_angles, c(1.00,1.10))] <-  pi/2 + 0.1
theta_end2[cont(start_angles, c(1.10,1.19))] <-  pi/2
theta_end2[cont(start_angles, c(1.19,1.21))] <-  pi/2 
theta_end2[cont(start_angles, c(1.21,1.27))] <-  pi/3
theta_end2[cont(start_angles, c(1.27,1.29))] <-  pi/4
theta_end2[cont(start_angles, c(1.29,1.32))] <-  pi/4
theta_end2[cont(start_angles, c(1.32,1.34))] <-  pi/4
theta_end2[cont(start_angles, c(1.34,1.35))] <-  pi/4
theta_end2[cont(start_angles, c(1.35,1.36))] <-  pi/4
theta_end2[cont(start_angles, c(1.36,1.42))] <-  pi/6
theta_end2[cont(start_angles, c(1.42,1.44))] <-  pi/6
theta_end2[cont(start_angles, c(1.44,1.51))] <-  pi/10
theta_end2[cont(start_angles, c(1.51,1.53))] <-  pi/20
theta_end2[cont(start_angles, c(1.53,1.55))] <-  pi/30
theta_end2[cont(start_angles, c(1.55,1.56))] <-  pi/60
theta_end2[cont(start_angles, c(1.56,1.57))] <-  pi/120
theta_end2[cont(start_angles, c(1.57,1.58))] <-  pi/600
theta_end2[cont(start_angles, c(1.58,1.59))] <-  pi/30000


for(i in seq_along(start_angles)){

  ## First the upwards strings:
  xy <-
    stringpoints(
        y_start = dist_from_hole,
        initial_string_angle = start_angles[i],
        theta = seq(from=theta_start,to=theta_end1[i],len=100)
    )
  points(xy,type='l',col=cols[i],lwd=0)
}
## now the downward ones:
for(i in seq_along(start_angles)){
  xy <-
    stringpoints(
        y_start = dist_from_hole,
        initial_string_angle = -(pi-start_angles[i]),
        theta = -seq(from=theta_start,to=theta_end2[i],len=1000)
    )
  rsq <- c(1+sum(xy[1,]^2),rowSums(xy^2))
  inward <- diff(rsq)<0

  xy_inward  <- xy[ inward,]
  xy_outward <- xy[!inward,]

  points(xy_inward ,type='l',col=cols[i],lwd=1)
  points(xy_outward,type='l',col=cols[i],lwd=0)

  closest_approach <- rbind(
      closest_approach,
      c(start_angles[i],sqrt(min(rowSums(xy^2)))-1)
  )
  
}

points(dist_from_hole,0,pch=16)
polargrid(rlab=1.8)
segments(0,0,1,2)  # radial string
event_horizon()
