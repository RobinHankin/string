library("deSolve")   # ode()
source("usefulfuncs.R")

`geodesic` <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      ##    V <- (r-2*m)*(r^2+h^2)/r^3  (with GR term)
      if(GR){
        dVdr <- -2*h^2/r^3 + 2*m/r^2 + 6*m*h^2/r^4 # inefficient idiom
        } else {
          dVdr <- -2*h^2/r^3 + 2*m/r^2
        }
      rdotdot <- -dVdr/2
      phidot <- h/r^2
      return(list(c(rdot,rdotdot,phidot)))
    })
}

`trajectory` <- function(r_start, m, h, tau=NULL, GR=TRUE){
  if(is.null(tau)){tau<- seq_len(3000)} # works nicely with trajectory(30, 1/2, 3)

  yini <- c(
      r    = r_start,
      rdot = 0,
      phi  = 0
  )
  
  bh <- as.data.frame(ode(y=yini, times=tau, func=geodesic, c(m=m, h=h, GR=GR), rtol=1e-6))
  
  r   <- bh[,2]
  phi   <- bh[,4]

  xy <- cbind(x=r*cos(phi),y=r*sin(phi))
  return(cbind(bh,xy))
}

## Example use-cases
if(FALSE){
jj <- trajectory(30, 1/2, 3) # m=1/2 -> Schwarzschild radius = 1
plot(jj$x,jj$y, asp=1, type='l', axes=FALSE, ylim=c(-30,30), xlab='', ylab='')
polargrid(r=10*(1:3))
event_horizon()


jj <- trajectory(30, 1/2, 3, GR=FALSE)  # Newtonian approximation
plot(jj$x,jj$y, asp=1, type='l', axes=FALSE, ylim=c(-30,30), xlab='', ylab='')
polargrid(r=10*(1:3))
event_horizon()


source("mercury.R")
jj <- trajectory(merc_perihelion, m=d, h=h, tau=seq(from=0,to=15e17,len=1e6))
plot(jj$x,jj$y, asp=1, type='l', axes=FALSE, xlab='', ylab='')
event_horizon()

}
