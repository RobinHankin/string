rm(list=ls())


pars <- c(eel=1)  # dummy

source("usefulfuncs.R")

## setup:
jj <- 3
par(pty='s')
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l')

event_horizon()



dist_from_hole <- 3


start_angles <- c(seq(from=0.1,by=0.01,to=1.5) # one string per start angle, each one a different colour
                  )

start_angles <- unique(sort(c(start_angles
                             

#                             , seq(from=1.21,to=1.5,by=0.01)


                              )))


n <- length(start_angles)
cols <- rainbow(n+round(n/7))

theta_start <- 0

theta_end1 <- 0 + 0*start_angles ## upward/outward
theta_end2 <- pi*0.45 + 0*start_angles  # downward/inward

theta_end2[cont(start_angles, c(0.10,0.50))] <-  pi/2
theta_end2[cont(start_angles, c(0.50,1.00))] <-  pi/2 +0.3
theta_end2[cont(start_angles, c(1.00,1.19))] <-  pi*1.1
theta_end2[cont(start_angles, c(1.19,1.21))] <-  3*pi/2 
theta_end2[cont(start_angles, c(1.21,1.27))] <-  pi*0.8
theta_end2[cont(start_angles, c(1.27,1.29))] <-  pi*1
theta_end2[cont(start_angles, c(1.29,1.32))] <-  pi*0.8
theta_end2[cont(start_angles, c(1.32,1.34))] <-  pi*0.7
theta_end2[cont(start_angles, c(1.34,1.35))] <-  pi*0.62
theta_end2[cont(start_angles, c(1.35,1.36))] <-  pi*0.62
theta_end2[cont(start_angles, c(1.36,1.42))] <-  pi*0.4
theta_end2[cont(start_angles, c(1.42,1.44))] <-  pi*0.4
theta_end2[cont(start_angles, c(1.44,1.51))] <-  pi*0.2


for(i in seq_along(start_angles)){

  ## First the upwards strings:
  xy <-
    stringpoints(
        y_start = dist_from_hole,
        initial_string_angle = start_angles[i],
        theta = seq(from=theta_start,to=theta_end1[i],len=100)
    )
  points(xy,type='l',col=cols[i],lwd=1)
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

  points(xy_inward ,type='l',col=cols[i])
  points(xy_outward,type='l',col=cols[i],lty=1)

}

points(dist_from_hole,0,pch=16)
polargrid()
