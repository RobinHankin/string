## plots Flamm's paraboloid; intended to be called by maker.R.
## It is quite slow to plot on the screen, but fast to make a pdf.

## This script runs standalone but is designed to be called from
## maker.R [cf Makefile], which creates the pdf files
## "flamm_string.pdf" and "flamm_nostring.pdf"


library("plot3D")
source("usefulfuncs.R")

flamm <- function(string=TRUE, drawlegend=FALSE, ...){

  ## string, drawlengend: Boolean specifying whether or not to draw
  ## the string and/or the legend.  "...", further arguments passed to
  ## scatter3D().

  ## values of r and theta to plot: use longer vectors to make the plot
  ## look better but this takes longer


  thetavals <- seq(from=0,to=2*pi,len=100)
  rvals <-  seq(from=1,to=10,len=30)

  ## set up the axis:
  scatter3D(x=1:2, y=1:2, z=1:2,   # dummy points
            type='n',lwd=1,pch=NA,
            r=100000,box=FALSE,scale=FALSE,colkey=FALSE,
            xlim=c(-5,5),ylim=c(-5,5),zlim=c(-16,0),
            ...
            )


  par(mfrow = c(1, 1))


  ## plot circles of constant r:
  for(r in rvals){
    x <- r*cos(thetavals)
    y <- r*sin(thetavals)
    z <- thetavals*0 -4*sqrt(r-1)
    lines3D(x, y, z,  bty = NULL, colkey=FALSE, 
            ticktype = "detailed", lwd = 0.1,add=TRUE,col='blue')
  }

  ## plot radial lines

  ## kludge to improve appearance of radial lines near r=1:
  rvals <- sort(unique(c(rvals,seq(from=1,to=1.3,len=10))))

  for(theta in thetavals){
    x <- rvals*cos(theta)
    y <- rvals*sin(theta)
    z <- theta*0 -4*sqrt(rvals-1)
    lines3D(x, y, z,  bty = NULL, colkey=FALSE, 
            ticktype = "detailed", lwd = 0.1,
            add=TRUE,col='red')
  }

  ## Now draw a taut string:
  if(isTRUE(string)){
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

    par(lend=1)
    ## actually draw the string:
    lines3D(x, y, z,  bty = NULL, colkey=FALSE, 
            lwd=2, col='black',
            add=TRUE,ticktype='detailed'
            )

    if(drawlegend){
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
  } else {
    if(drawlegend){
      legend("topright",
             legend = c(
                 expression(paste("lines of constant ",r)),
                 expression(paste("lines of constant ",phi))),
             col = c("blue","red","black"),
             lty = 1,
             lwd = c(0.1,0.1)
             )
    }
  }
}  # closes flamm <- function

