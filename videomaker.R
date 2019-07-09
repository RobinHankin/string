## This file is based on 'light_start_at_r_equals_2.R' but modified to
## produce a sequence of PDF files for use as a video.  See
## 'light_start_at_r_equals_2.R' for more documentation.

source("usefulfuncs.R") # defines polargrid() etc
source("usefullightfuncs.R") # defines lightpoints()

r_start <- 2 # starting radius for light ray
domask <- TRUE    # set to FALSE to see entire geodesic
size_of_plot <- 3

## plot setup:
par(xpd=TRUE)


cutoffmatrix  <- matrix(c(
    -pi/2,0.01,
    -1.57,0.01,
    -1.55, 0.01,
    -1.53, 0.03,
    -1.51, 0.04,
    -1.49, 0.05,
    -1.47, 0.07,
    -1.45, 0.07,
    -1.4, 0.09,
    -1.3, 0.15,
    -1.2, 0.2,
    -1.1, 0.3,
    -1.0, 0.35,
    -0.9, 0.4,
    -0.8, 0.5,
    -0.7, 0.6,
    -0.6, 0.8,
    -0.5, 0.9,
    -0.4, 1.1,
    -0.3, 1.2,
    -0.2, 1.4,
    -0.1, pi/2,
    0.0, pi/2 + 0.4,
    0.1, pi/2   + 0.9,
    0.15, pi/2  + 1.56,
    0.2, pi/2   + pi/2,
    0.22, pi/2  + 2.3,
    0.23, pi/2  + 2.3,
    0.26, pi/2  + 2.5,
    0.28, pi/2  + pi,
    0.285, pi/2  + 3.6,
    0.29, pi/2  + 4.6,
    0.295, pi/2  + 3*pi/2,
    0.30, pi/2   + 3*pi/2,
    0.301, pi/2 + 5,
    0.302, pi/2 + 6,
    0.305, pi/2 + 6,
    0.31, pi/2 + 6,
    0.32, pi/2 + 6,
    0.325, pi/2 + 4.5,
    0.33, pi/2 + 4.4,
    0.335, pi/2 + 4.3,
    0.34, pi/2 + 4.2,
    0.35, pi/2 + 4,
    0.36, pi/2 + 4,
    0.37, pi/2 + 3.7,
    0.38, pi/2 + 3.5,
    0.4, pi/2 + 3.4,
    0.43, pi/2 + 3.2,
    0.45, pi/2 + 2.9,
    0.5, pi/2 + 2.8,
    0.55, pi/2 + 2.4,
    0.6, pi/2 + 2.3,
    0.7, pi/2 + 2.1,
    0.8, pi/2 + 1.7,
    0.9, pi/2 + 1.5,
    1.0, pi/2 +1.2,
    1.1, pi/2 +1.1,
    1.15, pi/2 +1.03,
    1.2, pi/2 +0.9,
    1.25, pi/2 +0.8,
    1.3, pi/2 +0.6,
    1.35, pi/2+0.4 ,
    1.4, pi/2+0.2 ,
    1.45, pi/2 ,
    1.47, 1.3,
    1.48, 1.447,
    1.50,1.2,
    1.51,1.1,
    1.52,1.1,
    1.53,0.9,
    1.54,0.8,
    1.55,0.65,
    1.56,0.65,
    1.57,0.5,
    pi/2-0.00005,0.0001,
    pi/2+0.1, 0.1
    )
   ,ncol=2,byrow=TRUE)

colnames(cutoffmatrix) <- c("cuts","vals")
fmax <- approxfun(cutoffmatrix[,1],cutoffmatrix[,2])

start_angle <- seq(from=-pi/2,to=pi/2-0.01,len=1000)
start_angle <- start_angle[-c(1,length(start_angle))]


## Here the file differs from 'light_start_at_r_equals_2.R' in that we
## need the index 'i' to create the name of the PDF files.
for(i in seq_along(start_angle)){
  initialangletotangent <- start_angle[i]
  final_phi <- min(fmax(initialangletotangent),2*pi)

  xy1 <-
    nullgeodesic(
        r_start=r_start,  # r=3/2 is a circular (but unstable) orbit
        dubydphistart = tan(initialangletotangent)/r_start,
        phi=seq(from=0,to=final_phi,len=100)
    )

  ## Here the file differs from 'light_start_at_r_equals_2.R' in that
  ## the plot commands are all inside the loop, so we can produce a
  ## sequence of PDF files.
  numst <- formatC(i,width=3,format='d',flag='0')
  pdf(file = paste("blackhole",numst,".pdf",sep=""))
  jj <- c(-size_of_plot, size_of_plot)
  plot(NULL,asp=1,xlim=jj,ylim=jj,type='l',axes=FALSE,xlab='',ylab='')
  points(xy1,col='red',type='l')

  ## grid:
  polargrid()
  
  ## Mask strings too far from the black hole:
  if(domask){mask(4)}
  
  event_horizon(fill=FALSE)
  points(2,0,pch=16)
  
  
  dev.off()
}



