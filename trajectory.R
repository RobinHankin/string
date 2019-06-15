## Plots a trajectory of an (infinitesimal mass) object orbiting a
## black hole, showing non-closed orbit.  Works standalone but 

## notation follows Rindler, p239
library("deSolve")
source("usefulfuncs.R")
     
LVmod <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
    #    V <- (r-2*m)*(r^2+h^2)/r^3
        dVdr <- -2*h^2/r^3 + 2*m/r^2 + 6*m*h^2/r^4 # inefficient idiom
        rdotdot <- -dVdr/2
        phidot <- h/r^2
        return(list(c(rdot,rdotdot,phidot)))
    })
}

## parameters, mass m and angular momentum h:
pars  <- c(m=1/2, h=3)    # m=1/2, so r_schwarz=1

## Initial conditions, rdot=0 means periastron:
yini  <- c(r=30,rdot=0,phi=0)

## times means *proper* times:
times <- seq(from=0,to=3100,by=3)

out   <- ode(yini, times, LVmod, pars)
jj <- cbind(out[,2]*cos(out[,4]),out[,2]*sin(out[,4]))

## Plot commands start:
plot(jj, asp=1, type='l',
     axes=FALSE, xlab='',ylab='',
     main="Timelike geodesics in the Schwarzschild geometry")

polargrid(r=(1:6)*5)
text((1:6)*5,-1,(1:6)*5)
event_horizon(fill=TRUE)
