## Lots of rainbow coloured strings all passing through the same point
## at different angles.  For production PDF image, source file
## "maker.R", which creates the best PDF.

## This script runs standalone but is designed to be called from
## maker.R [cf Makefile], which creates the pdf file
## "angle_at_r_equals_2.pdf".


pars <- c(eel=1)  # dummy

source("usefulfuncs.R")


mask <- TRUE    # set to FALSE to see entire string

## setup:
jj <- 6.5
par(pty='s')
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',xlab='',ylab='',axes=FALSE)
polargrid(rlab=6.5)


dist_from_hole <- 2

## Define start_angles, specifying the angle that the string makes at
## (2,0) from a tangent:


## One string per start angle, each one a different colour:
start_angles <- seq(from=-0.8,to=0.0,by=0.015) 


n <- length(start_angles)
cols <- rainbow(n+round(n/7))

theta_start <- 0

theta_end1 <- rep(pi/2, length(start_angles))
theta_end2 <- rep(pi/2-0.9, length(start_angles))

## following construction uses `cutoffmatrix` to specify maximum angle
## for each string, using fun() which is defined in usefulfuncs.R:

cutoffmatrix  <- matrix(c(
    -9.83, pi/2  + 0.00,
    -0.80, pi/2  + 3.30,
    -0.79, pi/2  + 3.20,  # -0.8 to -0.79
   -0.785, pi/2  + 3.10,
    -0.78, pi/2  + 2.99,
    -0.77, pi/2  + 2.93,
    -0.76, pi/2  + 2.83, # -0.77 to -0.76
    -0.75, pi/2  + 2.70,
    -0.74, pi/2  + 2.67,
    -0.73, pi/2  + 2.57,
    -0.72, pi/2  + 2.52,
    -0.71, pi/2  + 2.40,
    -0.69, pi/2  + 2.34,
    -0.67, pi/2  + 2.25,
    -0.64, pi/2  + 2.10,
    -0.60, pi/2  + 1.90,
    -0.55, pi/2  + 1.71,
    -0.50, pi/2  + 1.54,
    -0.45, pi/2  + 1.40,
    -0.40, pi/2  + 1.20,
    -0.35, pi/2  + 1.10,
    -0.30, pi/2  + 0.90,
    -0.25, pi/2  + 0.80,  # -0.3 to -0.25
    -0.20, pi/2  + 0.70,
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


## Mask strings too far from the black hole:
if(mask){
  xy <- cbind(c(7,30,30,7),c(-0.5,-0.5,0.5,0.5))
  howmany <- 100  # 100 for production, 10 for testing
  for (theta in seq(from=0,to=2*pi,len=howmany)){
    jjxy <- xy %*% rotmat(theta)
    polygon(jjxy[,1],jjxy[,2],col='white',border=NA) # white for production
  }
}



event_horizon()
