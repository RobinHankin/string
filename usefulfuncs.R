## Sets up the ODE for minimal length string (Euler-Lagrange formula)

library("deSolve")   # ode()

`schwarzschild` <- function(theta, state, pars){
  with(as.list(c(state,pars)),{
  
    dy <- ydash
    dydash <- -1 + y + ydash^2*(-3 + 4*y)/(2*(y-1)*y)


  return(list(c(dy,dydash)))
  })
}

#stringpoints <- function(y_start <- 9,theta_start <- -1.3)

`stringpoints` <- function(y_start,initial_string_angle, theta=seq(from=0, to=pi, len=100)){
  yini <- c(y=y_start, ydash=y_start*tan(initial_string_angle))

  bh <- ode(yini,theta,schwarzschild,pars,rtol=1e-6)

  r <- bh[,2]
  theta <- bh[,1]

  xy <- cbind(r*cos(theta),r*sin(theta))
  return(xy)
}



## function cont() returns TRUE if x is in the interval specified
`cont` <- function(x,interval){ (x-interval[1])*(x-interval[2]) <=0}


`polargrid` <- function(r=1:7,n=12,rlab=max(r)/2, labels=TRUE,...){
  fish <- c(
      expression(pi/6),    # Note off-by-one error
      expression(pi/3),
      expression(pi/2),
      expression(2*pi/3),
      expression(5*pi/6),
      expression(pi),
      expression(7*pi/6),
      expression(4*pi/3),
      expression(3*pi/2),
      expression(5*pi/3),
      expression(11*pi/6),
      expression(0)
)
      
         
  jj <- seq(from=0,to=2*pi,len=100)
  xy <- cbind(cos(jj),sin(jj))
  for(i in r){ points(xy*i,type='l',lty=3,lwd=0.3, ...) }
  angs <- seq_len(n)*2*pi/n
  for(a in seq_along(angs)){
    segments(x0=0,y0=0,x1=cos(angs[a])*max(r),y1=sin(angs[a])*max(r),lwd=0.3,lty=3, ...)
    if(labels){
      text(cos(angs[a])*rlab,sin(angs[a])*rlab,fish[a])
    }
  }
}


`event_horizon` <- function(...){
  points(0,0,pch=16,cex=0.3)  # singularity
  th <- seq(from=0,to=2*pi,len=300)
  polygon(cos(th),sin(th),lwd=0.1, col='black', ...)   # event horizon
  ## setup ends
}


fun <- function(cuts,vals){
  return(function(x){vals[findInterval(x, c(-Inf, cuts))]})
}

rotmat <- function(theta){
    matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),2,2)
}
    
