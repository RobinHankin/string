## plots Flamm's paraboloid; quite slow to plot on the screen, but fast to make a pdf.

rm(list=ls())
library(plot3D)
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

