## plots Flamm's paraboloid; quite slow to plot on the screen, but fast to make a pdf.

rm(list=ls())
library(plot3D)
library(onion)
options(use.R = TRUE)  #needed to handle NA values
source("usefulfuncs.R")
source("flammfuncs.R")


pdf(file="~/f.pdf")
pars <- c(eel=1) # dummy

thetavals <- seq(from=0,to=2*pi,len=100)
rvals <-  seq(from=1,to=10,len=30)
rvals2 <- sort(unique(c(rvals,seq(from=1,to=1.3,len=10))))

## set up the plot, and the axis:
scatter3D(x=1:2, y=1:2, z=1:2,   # dummy points
          bty=NULL,type='n',lwd=1,pch=NA,
          r=1000,box=TRUE,scale=FALSE,colkey=FALSE,
          xlim=c(-52,52),ylim=c(-52,52),zlim=c(-32,0),
          phi=0,theta=0
          )


offsets <-
    matrix(c(
        00,00,0,
        20,00,0,
        00,20,0,
        20,20,0
    ), byrow=TRUE,ncol=3
    )

quats <- c(H1,Hi,Hi+Hj,Hi+Hj+Hk)

for(o in seq_along(quats)){
    reds <- matrix(NA,1,3)
    jj <-  redpoints(offsets[o,],quats[o],thetavals=thetavals,rvals=rvals)
    reds <- rbind(reds,jj)

    lines3D(
        x = reds[,1],
        y = reds[,2],
        z = reds[,3],
        bty = NULL, colkey=FALSE, 
        ticktype = "detailed", lwd = 0.1,
        add=TRUE,col='red')

    blues <- matrix(NA,1,3)
    jj <- bluepoints(offsets[o,],quats[o],thetavals=thetavals,rvals=rvals)
    blues <- rbind(blues,jj)

    lines3D(
        x = blues[,1],
        y = blues[,2],
        z = blues[,3],
        bty = NULL, colkey=FALSE, 
        ticktype = "detailed", lwd = 0.1,
        add=TRUE,col='blue')

    strings <- allstringpoints(offsets,quats)
    lines3D(
        x = strings[,1],
        y = strings[,2],
        z = strings[,3],
        bty = NULL, colkey=FALSE, 
        ticktype = "detailed", lwd = 2,
        add=TRUE,col='black')
}   

legend("topright",
       legend = c(
           expression(paste("lines of constant ",r)),
           expression(paste("lines of constant ",phi)),
           "taut string"),
       col = c("blue","red","black"),
       lty = 1,
       lwd = c(0.1,0.1,2)
       )

dev.off()
