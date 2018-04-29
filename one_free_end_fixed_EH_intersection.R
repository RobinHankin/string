rm(list=ls())


pars <- c(eel=1)  # dummy

source("usefulfuncs.R")

## setup:
jj <- 4.1
par(pty='s')
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='n',axes=F,xlab='',ylab='')
polargrid(1:4,rlab=3.5)



dist_from_hole <- 3


start_angles <- c(seq(from=1.18,by=0.01,to=1.5) # one string per start angle, each one a different colour
                  )


start_angles <- rev(start_angles)

n <- length(start_angles)
cols <- rainbow(n+round(n/7))

theta_start <- 0

theta_end1 <- 0 + 0*start_angles ## upward/outward
theta_end2 <- pi*0.45 + 0*start_angles  # downward/inward

theta_end2[cont(start_angles, c(0.10,0.50))] <-  pi/2
theta_end2[cont(start_angles, c(0.50,1.00))] <-  pi/2 +0.3
theta_end2[cont(start_angles, c(1.00,1.19))] <-  pi*1.1
theta_end2[cont(start_angles, c(1.19,1.21))] <-  3*pi/2 
theta_end2[cont(start_angles, c(1.21,1.27))] <-  pi
theta_end2[cont(start_angles, c(1.27,1.29))] <-  pi*0.9
theta_end2[cont(start_angles, c(1.29,1.32))] <-  pi*0.8
theta_end2[cont(start_angles, c(1.32,1.34))] <-  pi*0.7
theta_end2[cont(start_angles, c(1.34,1.35))] <-  pi*0.62
theta_end2[cont(start_angles, c(1.35,1.36))] <-  pi*0.62
theta_end2[cont(start_angles, c(1.36,1.42))] <-  pi*0.4
theta_end2[cont(start_angles, c(1.42,1.44))] <-  pi*0.3
theta_end2[cont(start_angles, c(1.44,1.51))] <-  pi*0.2


closest_approach <- start_angles + NA


for(i in seq_along(start_angles)){
  xy <-
    stringpoints(
        y_start = dist_from_hole,
        initial_string_angle = -(pi-start_angles[i]),
        theta = -seq(from=theta_start,to=theta_end2[i],len=1000)
    )
  
  rsq <- c(1+sum(xy[1,]^2),rowSums(xy^2))  # dist from singularity squared
  inward <- diff(rsq)<0

  xy_inward  <- xy[ inward,]
  xy_outward <- xy[!inward,]

  dist_from_singularity <-  sqrt(rowSums(xy^2))
  closest_approach[i] <- min(dist_from_singularity-1)

  point_of_closest_approach <-
      drop(xy[which.min(dist_from_singularity),])

  angle_of_closest_approach <-
      atan(
          point_of_closest_approach[2]/
          point_of_closest_approach[1]
      )
  print(angle_of_closest_approach)
  if(angle_of_closest_approach>0){
      angle_of_closest_approach <-
          angle_of_closest_approach -pi
  }
  
  rm <- rotmat(angle_of_closest_approach)
  
  points(xy_inward  %*% rm ,type='l',col=cols[i],lwd=2)
  if(FALSE){
    points(xy_outward %*% rm ,type='l',col=cols[i],lwd=5)
  }
  
}
event_horizon()
points(1,0,pch=16,cex=2,col='gray')


