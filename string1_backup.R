## lots of blue strings with varying closest approach distances.

library("deSolve")

schwarzschild <- function(theta, state, pars){
  with(as.list(c(state,pars)),{
  
    dy <- ydash
    dydash <- -1 + y + ydash^2*(-3 + 4*y)/(2*(y-1)*y)


  return(list(c(dy,dydash)))
  })
}

pars <- c(eel=1)  # dummy

#stringpoints <- function(y_start <- 9,theta_start <- -1.3)

stringpoints <- function(y_start,initial_string_angle, theta=seq(from=0, to=pi, len=100)){
  yini <- c(y=y_start, ydash=y_start*tan(initial_string_angle))

  bh <- ode(yini,theta,schwarzschild,pars,rtol=1e-5)

  r <- bh[,2]
  theta <- bh[,1]

  xy <- cbind(r*cos(theta),r*sin(theta))
  return(xy)
}


## plot setup:
jj <- 6
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l')


points(0,0,pch=16,cex=0.3)  # singularity
th <- seq(from=0,to=2*pi,len=100)
points(cos(th),sin(th),type='l')   # event horizon
## setup ends

wanted <- c(1.01,1.02,
            1.04,1.08,
            1.1,1.2,
            1.3, 1.4,
            1.5,1.75,
            2,3,4,5,6)
thetamax <- pi/2 -0.1 + wanted*0

special <- c(pi+1.2,pi+0.8,
             pi+0.4, pi-0.18,
             pi-.2,pi-.6,
             pi-0.9, pi-.9,
             pi-1.2, pi-1.2,
             pi-1.2)

thetamax[seq_along(special)] <- special

    
for(i in seq_along(wanted)){
  tseq <- seq(from=0,to=thetamax[i],len=100)
  xy <- stringpoints(y_start=wanted[i],initial_string_angle = 0,theta=tseq)
  points(xy,type='l',col='blue')
  
  xy <- stringpoints(y_start=wanted[i],initial_string_angle = 0, theta=-tseq)
  points(xy,type='l',col='blue')
}









