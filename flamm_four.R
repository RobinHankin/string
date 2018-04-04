## plots Flamm's paraboloid; quite slow to plot on the screen, but fast to make a pdf.

rm(list=ls())
library(plot3D)
source("usefulfuncs.R")

pars <- c(eel=1) # dummy

thetavals <- seq(from=0,to=2*pi,len=100)
rvals <-  seq(from=1,to=10,len=30)
rvals2 <- sort(unique(c(rvals,seq(from=1,to=1.3,len=10))))

redpoints <- function(offset, quat){
  out <- matrix(NA,1,3)

  for(theta in thetavals){
    x <- rvals*cos(theta)
    y <- rvals*sin(theta)
    z <- theta*0 -4*sqrt(rvals-1)

    out <- rbind(out, cbind(x,y,z), NA)

    out <- out + offset
    out <- rotate(out,quat)
    return(out)
  }


  getblue <- function(quat){
  jj <- cbind(
      x = r*cos(thetavals),
      y = r*sin(thetavals),
      z = thetavals*0 -4*sqrt(r-1)
  )
  return(rotate(jj,quat))
}

getred <- function(quat){

  x <- rvals*cos(theta)
  y <- rvals*sin(theta)
  z <- theta*0 -4*sqrt(rvals-1)
  
  rotate(cbind(x,y,z),quat)
}

  ## values of r and theta to plot: use longer vectors to make the plot
  ## look better but this takes longer



  ## set up the plot, and the axis:
  scatter3D(x=1:2, y=1:2, z=1:2,   # dummy points
            bty=NULL,type='n',lwd=1,pch=NA,
            r=1000,box=FALSE,scale=FALSE,colkey=FALSE,
#            xlim=c(-10,10),ylim=c(-10,10),zlim=c(-32,0),
            xlim=c(-4,4),ylim=c(-4,4),zlim=c(-32,0),
            ...   # typically phi and theta.
            )

  dpoints <- function(quat){


    
  ## plot circles of constant r:
  for(r in rvals){
    xyz <- getblue(quat)
    lines3D(
        x = xyz[,1],
        y = xyz[,2],
        z = xyz[,3],
            bty = NULL, colkey=FALSE, 
            ticktype = "detailed", lwd = 0.1,add=TRUE,col='blue')
  }

  ## now plot radial lines

  ## kludge to improve appearance of radial lines near r=1:
  rvals <- 

  for(theta in thetavals){

    xyz <- getred(quat)
    lines3D(
        x = xyz[,1],
        y = xyz[,2],
        z = xyz[,3],
        bty = NULL, colkey=FALSE, 
            ticktype = "detailed", lwd = 0.1,
            add=TRUE,col='red')

    
  }

  ## Now draw a taut string:

    dist_from_hole <- 1.4   # distance of closest approach


    xy <-
      stringpoints(
          y_start = dist_from_hole,
          initial_string_angle = 0,
          theta = seq(from=0,to=pi*0.67,len=100)
      )


    ## xy shows string from closest approach moving outward anticlockwise:
    r <- sqrt(rowSums(xy^2))
    x <- xy[,1]
    y <- xy[,2]
    z <- -4*sqrt(r-1)


    ## now add string from closest approach moving clockwise [rev() means
    ## that the points are in sequence along the string]:
    x <- c(rev(x),x)
    y <- c(-rev(y),y)
    z <- c(rev(z),z)

  
  xyz <- rotate(cbind(x,y,z),quat)
  lines3D(
      x = xyz[,1],
      y = xyz[,2],
      z = zyz[,3],  bty = NULL, colkey=FALSE, 
      lwd=2, col='black',
      add=TRUE,ticktype='detailed'
  )


  }
  dpoints(H1)
  dpoints(

  legend("topright",
                         legend = c(
                             expression(paste("lines of constant ",r)),
                             expression(paste("lines of constant ",phi)),
                             "taut string"),
                         col = c("blue","red","black"),
                         lty = 1,
                         lwd = c(0.1,0.1,2)
                         )

}
