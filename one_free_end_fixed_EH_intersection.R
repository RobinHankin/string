## this script runs standalone but is designed to be called from
## maker.R [cf Makefile], which creates the pdf file
## "one_free_end_fixed_EH_intersection.pdf".

pars <- c(eel=1)  # dummy

source("usefulfuncs.R")

## setup:
jj <- c(-2.5,2.5)
par(xpd=T)
par(pty='m')
plot(NULL,asp=1,xlim=jj,ylim=jj,type='n',axes=F,xlab='',ylab='')
dist_from_hole <- 3
userainbow <- FALSE  # TRUE = rainbow, FALSE = all strings blue
## setup ends


## One string per start angle, each one a different colour:
start_angles <- c(seq(from=1.18,by=0.03,to=1.56))

start_angles <- rev(start_angles)

n <- length(start_angles)+1
cols <- rainbow(n+round(n/7))[-1]  # "[-1]" so the radial string is correctly coloured
if(!userainbow){  cols[] <- "blue"}

theta_start <- 0


cutoffmatrix <-	 matrix(c(
0.00,     pi/2,
0.10,     pi/2,
0.50, 	  pi/2 +0.3,
1.00,     pi*1.1,
1.19,     3*pi/2,
1.21, 	  pi,
1.27, 	  pi*0.9,
1.29, 	  pi*0.8,
1.32, 	  pi*0.7,
1.34, 	  pi*0.62,
1.35, 	  pi*0.62,
1.36, 	  pi*0.4,
1.42, 	  pi*0.3,
1.44, 	  pi*0.2,
1.45, 	  pi*0.2,
1.46, 	  pi*0.2,
1.47, 	  pi*0.2,
1.48, 	  pi*0.2,
1.49, 	  pi*0.2,
1.50, 	  pi*0.15,
1.51, 	  pi*0.1,
1.52, 	  pi*0.1,
1.53, 	  pi*0.07,
1.54, 	  pi*0.05,
1.55, 	  pi*0.02,
1.56, 	  pi*0.02),ncol=2,byrow=TRUE)

f <- approxfun(cutoffmatrix[,1],cutoffmatrix[,2])

closest_approach <- start_angles + NA


## radial string (kludge):
points(x=c(1,3),y=c(0,0),type='l',col=cols[1],lwd=2)


for(i in seq_along(start_angles)){
  xy <-
    stringpoints(
        y_start = dist_from_hole,
        initial_string_angle = -(pi-start_angles[i]),
        theta = -seq(from=theta_start,to=f(start_angles[i]),len=1000)
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
#  print(angle_of_closest_approach)
  if(angle_of_closest_approach>0){
      angle_of_closest_approach <-
          angle_of_closest_approach -pi
  }
  
  rm <- rotmat(angle_of_closest_approach)
  
  jj <- xy_inward %*% rm 

  ## uncomment the following line ("jj <- xy") for debugging the
  ## theta_end2 stuff above; uncommenting will show the "real" strings
  ## rather than the rotated ones with only ingoing segments plotted
  ## jj <- xy

  points(jj,type='l',col=cols[i],lwd=2)


  ## now plot reflected strings:
  jj[,2] <- -jj[,2] 
  points(jj,type='l',col=cols[i],lwd=2)
  
}




polargrid(1:3,rlab=2.5)
event_horizon()  # event horizon last: the black hole should be truly black
