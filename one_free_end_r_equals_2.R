## This script runs standalone but is designed to be called from
## maker.R [cf Makefile], which creates the pdf file
## "one_free_end_r_equals_2.pdf"

pars <- c(eel=1)  # dummy for the ODE solver

source("usefulfuncs.R")

## setup:
jj <- 2
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),axes=FALSE,xlab='',ylab='')  # close-up


closest_approach <- matrix(0,0,2)  # used for debugging theta_end2 values
colnames(closest_approach) <- c("start_angle","closest_approach")
dist_from_hole <- 2
userainbow <- FALSE

## one string per start angle, each a different colour; start_angle =
## pi/2 is a radial string:
gap <- 0.06
start_angles <-
  c(seq(from=pi/2-gap, by= -gap, to=0.91))

             
# start_angles <- seq(from=0.89,to=.91,len=10)  # use this in conjunction with plot(closest_approach) to see where the upper limit of 0.9 comes from


start_angles <- sort(start_angles,decreasing=TRUE)

n <- length(start_angles)
cols <- rainbow(n+round(n/7))
if(!userainbow){  cols[] <- "blue"}

theta_start <- 0


theta_end2 <- pi*0.05 + 0*start_angles  # downward/inward
cutoffmatrix <- matrix(c(
0.10 ,   pi,
0.50 ,   pi,
1.00 ,   pi/2 + 0.1,
1.10 ,   pi/2,
1.19 ,   pi/2 ,
1.21 ,   pi/3,
1.27 ,   pi/4,
1.29 ,   pi/4,
1.32 ,   pi/4,
1.34 ,   pi/4,
1.35 ,   pi/4,
1.36 ,   pi/6,
1.42 ,   pi/6,
1.44 ,   pi/10,
1.51 ,   pi/20,
1.53 ,   pi/30,
1.55 ,   pi/60,
1.56 ,   pi/120,
1.57 ,   pi/600,
1.58 ,   pi/30000),byrow=TRUE,ncol=2)

f <- approxfun(cutoffmatrix[,1],cutoffmatrix[,2])

## now the downward ones:
for(i in seq_along(start_angles)){
  xy <-
    stringpoints(
        y_start = dist_from_hole,
        initial_string_angle = -(pi-start_angles[i]),
        theta = -seq(from=theta_start,to=f(start_angles[i]),len=1000)
    )
  rsq <- c(1+sum(xy[1,]^2),rowSums(xy^2)) # rsq == r-squared
  inward <- diff(rsq)<0      # TRUE if the string is moving inward

  xy_inward  <- xy[ inward,]
  xy_outward <- xy[!inward,]

  xy_inward[,2] <- -xy_inward[,2] 
  points(xy_inward ,type='l',col=cols[i],lwd=1)
  if(FALSE){  points(xy_outward,type='l',col=cols[i],lwd=0)}

  closest_approach <- rbind(
      closest_approach,
      c(start_angles[i],sqrt(min(rowSums(xy^2)))-1)
  )
}

polargrid(rlab=1.8)
segments(1,0,2,0,col=cols[1])  # radial string
points(dist_from_hole,0,pch=16)
event_horizon(fill=TRUE)
