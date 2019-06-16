## Plots a trajectory of an (infinitesimal mass) object orbiting a
## black hole, showing non-closed orbit.  Works standalone but is
## intended to be called by maker.R which creates a pdf file.

## Notation follows Rindler, p239

library("deSolve")
source("usefulfuncs.R")
source("mercury.R")
     
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
pars  <- c(m=d, h=h)    # m=1/2, so r_schwarz=1

## Initial conditions, rdot=0 means periastron:
yini  <- c(r=merc_perihelion,rdot=0,phi=0)

## times means *proper* times:
times <- seq(from=0,to=15e17,len=5000000)

out   <- ode(yini, times, LVmod, pars)
jj <- cbind(out[,2]*cos(out[,4]),out[,2]*sin(out[,4]))

## Plot commands start:
plot(jj, asp=1, type='l',
     axes=FALSE, xlab='',ylab='',
     main="Timelike geodesics in the Schwarzschild geometry")

#polargrid(r=(1:6)*5)
#text((1:6)*5,-1,(1:6)*5)
event_horizon(fill=TRUE)

out <- as.data.frame(out)

jj1 <- c(NA,out$rdot)
jj2 <- c(out$rdot,NA)

change <- (jj1>0) & (jj2<0)


tee    <- out$time[change]

phi <- out$phi[change]
phi <- (phi) %% (2*pi)


plot(tee,phi)
