## lots of rainbow coloured strings all passing through the same point
## at different angles.  For production PDF image, source maker.R,
## which creates the best PDF.

rm(list=ls())

pars <- c(eel=1)  # dummy


source("usefulfuncs.R")

## setup:
jj <- 6.5
par(pty='s')
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',xlab='',ylab='',axes=FALSE)
polargrid(rlab=6.5)


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

## following construction uses `cutoffmatrix` to specify maximum angle
## for each string, using fun() which is defined in usefulfuncs.R:

cutoffmatrix  <- matrix(c(
    -9.83, pi/2  + 0.00,
    -0.79, pi/2  + 3.30,
    -0.78, pi/2  + 2.99,
    -0.76, pi/2  + 2.93,
    -0.75, pi/2  + 2.70,
    -0.73, pi/2  + 2.67,
    -0.72, pi/2  + 2.62,
    -0.71, pi/2  + 2.40,
    -0.69, pi/2  + 2.30,
    -0.67, pi/2  + 2.10,
    -0.64, pi/2  + 2.00,
    -0.60, pi/2  + 1.90,
    -0.55, pi/2  + 1.71,
    -0.50, pi/2  + 1.54,
    -0.45, pi/2  + 1.30,
    -0.40, pi/2  + 1.20,
    -0.35, pi/2  + 1.00,
    -0.30, pi/2  + 0.90,
    -0.25, pi/2  + 0.80,
    -0.20, pi/2  + 0.60,
    -0.15, pi/2  + 0.50,
    -0.10, pi/2  + 0.40,
    +0.01, pi/2  + 0.30
    )
   ,ncol=2,byrow=TRUE)

colnames(cutoffmatrix) <- c("cuts","vals")
f_upwards <- fun(cutoffmatrix[,1],cutoffmatrix[,2])

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


## Now the downward strings:
if(FALSE){
for(i in seq_along(start_angles)){
  xy <-
    stringpoints(
        y_start = dist_from_hole,
        initial_string_angle = -(pi-start_angles[i]),
        theta = -seq(from=theta_start,to=theta_end2[i],len=100)
    )

  points(xy,type='l',col=cols[i],lwd=5)
}

}
points(dist_from_hole,0,pch=16)


# mask strings too far from the black hole:
xy <- cbind(c(7,30,30,7),c(-0.5,-0.5,0.5,0.5))
howmany <- 100  # 100 for production, 10 for testing
for (theta in seq(from=0,to=2*pi,len=howmany)){
  jjxy <- xy %*% rotmat(theta)
  polygon(jjxy[,1],jjxy[,2],col='white',border=NA) # white for production
}




event_horizon()