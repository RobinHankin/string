## lots of rainbow coloured strings all passing through the same point at different angles.

rm(list=ls())

pars <- c(eel=1)  # dummy


source("usefulfuncs.R")

## setup:
jj <- 6
par(pty='s')
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l')


points(0,0,pch=16,cex=0.3)  # singularity
th <- seq(from=0,to=2*pi,len=300)
points(cos(th),sin(th),type='l')   # event horizon
## setup ends


##xy <- stringpoints(y_start=9,initial_string_angle = -(pi-pi*1.42),theta=seq(from=0,to=-(pi+0.4),len=100))
#points(xy,type='l',col='red')

##xy <- stringpoints(y_start=9,initial_string_angle = -pi*1.42,theta=seq(from=0,to=pi+0.4,len=100))
#points(xy,type='l',col='red')

dist_from_hole <- 2

## define start_angles, specifying the angle that the string makes at (2,0) from a tangent:
start_angles <- seq(from=-0.8,to=0.0,by=0.021) # one string per start angle, each one a different colour

n <- length(start_angles)
cols <- rainbow(n+round(n/7))

theta_start <- 0

theta_end1 <- rep(pi/2, length(start_angles))
theta_end2 <- rep(pi/2-0.9, length(start_angles))

jj <- matrix(c(
    -9.83, pi/2  + 0.00,
    -0.79, pi/2  + 3.20,
    -0.78, pi/2  + 2.90,
    -0.73, pi/2  + 2.60,
    -0.72, pi/2  + 2.42,
    -0.71, pi/2  + 2.30,
    -0.69, pi/2  + 2.20,
    -0.67, pi/2  + 2.10,
    -0.64, pi/2  + 1.90,
    -0.60, pi/2  + 1.70,
    -0.55, pi/2  + 1.50,
    -0.50, pi/2  + 1.40,
    -0.45, pi/2  + 1.20,
    -0.40, pi/2  + 1.10,
    -0.35, pi/2  + 1.00,
    -0.30, pi/2  + 0.90,
    -0.25, pi/2  + 0.70,
    -0.20, pi/2  + 0.60,
    -0.15, pi/2  + 0.50,
    -0.10, pi/2  + 0.30,
    +0.01, pi/2  + 0.00
    )
   ,ncol=2,byrow=TRUE)

colnames(jj) <- c("cuts","vals")
f_upwards <- fun(jj[,1],jj[,2])

for(i in seq_along(start_angles)){

  ## First the upwards strings:
  xy <-
    stringpoints(
        y_start = dist_from_hole,
        initial_string_angle = start_angles[i],
        theta = seq(from=theta_start,to=f_upwards(start_angles[i]),len=1000)
    )
  points(xy,type='l',col=cols[i],lwd=1)
}

for(i in seq_along(start_angles)){
  xy <-
    stringpoints(
        y_start = dist_from_hole,
        initial_string_angle = -(pi-start_angles[i]),
        theta = -seq(from=theta_start,to=theta_end2[i],len=100)
    )

  points(xy,type='l',col=cols[i],lwd=1)
}
points(dist_from_hole,0,pch=16)






