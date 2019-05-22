## This script runs standalone but is designed to be called from
## maker.R [cf Makefile], which creates the pdf file
## "curvature_switch.pdf"

source("usefulfuncs.R") # defines stringpoints()

domask <- TRUE
## plot setup:
jj <- 5.5
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',axes=FALSE,,xlab='',ylab='')


polargrid(rlab=6.5)
## setup ends


dist <- exp(seq(from=log(1.1),to=log(7.5),len=40))

thetamax <- 0.1 + dist*0  # default value, (almost) always do-able


cuts <- c(        1,   1.06,  1.07, 1.08,   1.11,    1.13,    1.15,     1.17,  1.19,    1.21,    1.31,   1.41,  1.51, 1.7, 2.01,    3,    6, 7,  9)
vals <- c(  NA,      pi,    pi,   3.1,   2.87,   2.79,    2.71,     2.61,   2.51,  2.45,      2.38,     2.2,   2.1,  2.0, 1.9,   1.7, 1.4,  1.2, 1)
f <- approxfun(cuts,vals)

## Thus f(1.03) = pi, f(1.12) = 2.5

for(i in seq_along(dist)){
#  print(paste("starting ",dist[i],sep = ""))
  tseq <- seq(from=0,to=f(dist[i]),len=1000)
  bh <- stringpoints(y_start=dist[i],initial_string_angle = 0,theta=tseq,give=TRUE)
  r <- bh[,2]
  theta <- bh[,1]
  rdash <- bh[,3]

  thing <- rdash^2<2*r*(r-1)  # TRUE for ROC>0, FALSE for ROC<0

  rr <- r # temporary
  rr <- rr*thing
                                        # make negative ROC ignorable
  wm <-  which.min(rr)
  theta1 <- theta-theta[wm]
  xy1 <- cbind(r*cos(theta1),r*sin(theta1))
  points(xy1[ thing,],type='l',col='blue')   # blue
  points(xy1[!thing,],type='l',col='black')
 
  theta2 <- -theta-theta[wm]
  xy2 <- cbind(r*cos(theta2),r*sin(theta2))
  points(xy2[ thing,],type='l',col='blue')
  points(xy2[!thing,],type='l',col='black')

#  print(paste("done ",dist[i],sep = ""))

}

if(domask){mask(7)}
event_horizon()
