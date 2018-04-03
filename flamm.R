## plots Flamm's paraboloid; quite slow to plot on the screen, but fast to make a pdf.

rm(list=ls())

pdf(file="flamm.pdf")
library(plot3D)
source("usefulfuncs.R")

scatter3D(1:2,1:2,1:2,bty=NULL,type='n',lwd=1,pch=NA,
          phi=50,r=1000,box=TRUE,scale=FALSE,
          xlim=c(-10,10),ylim=c(-10,10),zlim=c(-32,0),colkey=FALSE
          )


par(mfrow = c(1, 1))
thetavals <- seq(from=0,to=2*pi,len=10)
rvals <-  seq(from=1,to=10,len=30)
for(r in rvals){
                  
    x <- r*cos(thetavals)
    y <- r*sin(thetavals)
    z <- thetavals*0 -4*sqrt(r-1)
    lines3D(x, y, z,  bty = NULL, colkey=FALSE, 
            ticktype = "detailed", lwd = 0.1,add=TRUE,col='blue')
}

for(theta in thetavals){
    x <- rvals*cos(theta)
    y <- rvals*sin(theta)
    z <- theta*0 -4*sqrt(rvals-1)
    lines3D(x, y, z,  bty = NULL, colkey=FALSE, 
            ticktype = "detailed", lwd = 0.1,add=TRUE,col='red')
}


## Now draw a string

dist_from_hole <- 3
pars <- c(eel=1) # dummy
xy <-
    stringpoints(
        y_start = dist_from_hole,
        initial_string_angle = 0,
        theta = seq(from=0,to=pi*0.2,len=100)
    )

r <- sqrt(rowSums(xy^2))
x <- xy[,1]
y <- xy[,2]
z <- -4*sqrt(r-1)

lines3D(x, y, z,  bty = NULL, colkey=FALSE, 
        ticktype = "detailed", lwd = 5,add=TRUE,col='black')


dev.off()
