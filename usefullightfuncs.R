library("deSolve")   # ode()

## function light() deals with r; cf lightu() which works with u=1/r
`light` <- function(lambda, state, pars){
  with(as.list(c(state,pars)),{
        dr <- (1-1/r)*sqrt(1-b^2*(1/r^2-1/r^3))
        dphi <- b*(1/r^2-1/r^3)
        return(list(c(dr,dphi)))
    })
}

## function lightpoints() deals with r; cf lightu() which works with u=1/r
`lightpoints` <- function(r_start, phi_start, lambda){
    yini <- c(r=r_start , phi=phi_start)
    bh <- ode(yini, lambda, light, pars, rtol=1e-6)
    
    r <- bh[,2]
    theta <- bh[,3]
    
    xy <- cbind(r*cos(theta),r*sin(theta))
    return(xy)
}


## function lightu() deals with u=1/r
## ODE is d^2u/dphi^2 =3u^2/2-u
`lightu` <- function(lambda, state, pars){
  with(as.list(c(state,pars)),{  # state is c(u,du)
    d2u <- 3*u^2/2-u
    return(list(c(du,d2u)))
  })
}

`stringu` <- function(r_start,dubydphistart,phi,include=FALSE){
  yini <- c(
      u  = 1/r_start,
      du = dubydphistart
  )
  
  bh <- ode(y=yini, times=phi, func=lightu, parms=c(dummy=0), rtol=1e-6) 
  
  phi <- bh[,1]  # column heading is "time" for some reason
  r <- 1/bh[,2]  # r=1/u is the dependent variable
  ## NB: bh[,3] is du/phi
  
  xy <- cbind(r*cos(phi),r*sin(phi))
  if(include){
      xy <- cbind(xy,r,phi)
  }
      
  return(xy)
}

