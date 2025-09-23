## This function plots a coffee-cup caustic, or a cardioid.  It is
## modelled on caustic2.R but various parameters can be set by
## sliders, courtesy of the "manipulate" package.

## Rays of light emerge from the circumference of the circle, at the 6
## o'clock position, coordinates (0,-1).

library("manipulate")


f <- function(ad1, ad2, ad3){
    ## First plot the bounding circle:
    a <- seq(from=0, to=2*pi, len=100)  # 'a' for angle
    plot(sin(a), cos(a), asp=1, type='l', axes=FALSE, xlab='', ylab='')

    x <- seq(from=-0.99, to=0.99, len=50)
    for(i in x){
        ## vertical ray first:	
        segments(x0=i, y0= -2, y1=sqrt(1-i^2), lwd=0.3)
        ## now reflected ray;
        x0 <- i
        y0 <- sqrt(1 - i^2)
        angle <- pi/2 - 2*asin(i * (1+ad3)) + ad1
        x1 <- x0 - cos(angle + ad2*angle^2)
        y1 <- y0 - sin(angle + ad2*angle^2)
        segments(x0=x0, y0=y0, x1=x1, y=y1, lwd=0.3)
    }
}

manipulate(f(ad1, ad2, ad3),
           ad1 = slider(-0.30, 0.30, initial=0),
           ad2 = slider(-0.05, 0.05, initial=0),
           ad3 = slider(-0.10, 0.10, initial=0)
           )
