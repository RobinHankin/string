library("deSolve")   # ode()

## function light() deals with r; cf lightu() which works with u=1/r
`light` <- function(lambda, state, pars){
  with(as.list(c(state,pars)),{
        dr <- (1-1/r)*sqrt(1-b^2*(1/r^2-1/r^3))
        dphi <- b*(1/r^2-1/r^3)
        return(list(c(dr,dphi)))
    })
}

## function stringlightpoints() deals with r; cf lightu() which works with u=1/r
`stringlightpoints` <- function(r_start, phi_start, lambda){
    yini <- c(r=r_start , phi=phi_start)
    bh <- ode(yini, lambda, light, pars, rtol=1e-6)
    
    r <- bh[,2]
    theta <- bh[,3]
    
    xy <- cbind(r*cos(theta),r*sin(theta))
    return(xy)
}


## function lightu() deals with u=1/r
`lightu` <- function(lambda, state, pars){
  with(as.list(c(state,pars)),{  # state is c(u,du)
    d2u <- 3*u^2/2-u
    return(list(c(du,d2u)))
  })
}

`stringu` <- function(r_start,dubydphistart,phi){
  yini <- c(
      u  = 1/r_start,
      du = dubydphistart
  )
  
  bh <- ode(y=yini, times=phi, func=lightu, parms=c(dummy=0), rtol=1e-6) 
  
  r <- 1/bh[,2]
  phi <- bh[,1]
  
  xy <- cbind(r*cos(phi),r*sin(phi))
  return(xy)
}

