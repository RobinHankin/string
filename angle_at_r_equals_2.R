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
userainbow <- FALSE   # colour of strings (FALSE = all strings blue)
dist_from_hole <- 2
## setup ends

## Define start_angles, specifying the angle that the string makes at
## (2,0) from a tangent:


## One string per start angle, each one a different colour:
delta_angle <- 0.02
start_angles <- seq(from=-0.8,to=pi/2,length=100)
start_angles <- start_angles[-length(start_angles)]


n <- length(start_angles)
cols <- rainbow(n+round(n/7))

  if(!userainbow){
    cols[] <- "blue"
  }

theta_start <- 0

## following construction uses `cutoffmatrix` to specify maximum angle
## for each string, using approxfun() 

cutoffmatrix  <- matrix(c(
    -0.90, pi/2  + 0.00,
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
   +0, pi/2  + 0.30,
  0.1,pi/2-0.1,
  0.2,pi/2-0.1,
   0.3,pi/3+0.1,
   0.5,pi/3,
   0.8,pi/5,
  0.9,pi/6,
  1.0,pi/7,
  1.1,pi/8,
  1.2,pi/13,
  1.3,pi/15,
  1.4,pi/29,
  pi/2-0.1,pi/39,
  pi/2-0.05,pi/79,
   pi/2-0.015,pi/300
    )
   ,ncol=2,byrow=TRUE)

colnames(cutoffmatrix) <- c("cuts","vals")
f_upwards <- approxfun(cutoffmatrix[,1],cutoffmatrix[,2])

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

## Radial string:
points(rbind(c(2,0),c(100,0)),col=cols[i+1],type='l')

## Now the downward strings:
if(FALSE){
    for(i in seq_along(start_angles)){
        xy <-
            stringpoints(
                y_start = dist_from_hole,
                initial_string_angle = -start_angles[i],
                theta = seq(from=theta_start,to=-f_upwards(-start_angles[i]),len=100)
            )
        xy[,2] <- -xy[,2]
        points(xy,type='l',col=cols[i],lwd=0.2)
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


polargrid(rlab=6.5)
event_horizon()
