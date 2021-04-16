## Some useful null geodesic functions, designed to be called from
## file "shapiro.R".  Functionality here differs from that in
## usefullightfuncs.R in that here we parametrize a light path in
## terms of coordinate time t, so we can simulate the Shapiro delay
## between earth and Mercury.  In file usefullightfuncs.R we
## parametrize in terms of azimuthal angle phi, not time.

library("deSolve")   # ode()
source("usefulfuncs.R")
source("mercury.R")   # defines M=2e30, the mass of the sun, orbital
                      # params of Mercury, etc


`nullgeodesicequation` <- function(Time, State, Pars) {
    with(as.list(c(State, Pars
                   )), {
                     jj <- 1-1/r  # 2GM=1
                     drbydt <- jj*sqrt(1-b^2*jj/r^2) ## NB: positive root
                     dphibydt <- b*jj/r^2
                     return(list(c(rdot=drbydt,phidot=dphibydt)))
                   })
}

`nullgeodesic` <- function(r_start, b, tau=NULL, SI=FALSE){

  if(is.null(tau)){
        tau <- seq(from=0,to=100.1,len=100)
  } 

  yini <- c(
      r    = r_start,
      phi  = 0  # Start at 3 o'clock
  )

  bh <- ode(y=yini, times=tau, func=nullgeodesicequation, parms=c(b=b),rtol=1e-8,atol=1e-7)
  r   <- bh[,2]
  phi <- bh[,3]

  xy <- cbind(x=r*cos(phi),y=r*sin(phi))
  return(as.data.frame(cbind(bh,xy)))
}



