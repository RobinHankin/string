library("deSolve")

schwarzschild <- function(theta, state, pars){
  with(as.list(c(state,pars)),{
  
    dy <- ydash
    dydash <- (y^3-3*ydash^2/2 - 2*y^2 +y +2*y*ydash^2)/(y^2-y)

  return(list(c(dy,dydash)))
  })
}

pars <- c(eel=1)  # dummy

#stringpoints <- function(y_start <- 9,theta_start <- -1.3)

stringpoints <- function(y_start,initial_string_angle, theta=seq(from=0, to=pi, len=100)){
  yini <- c(y=y_start, ydash=y_start*tan(initial_string_angle))

  bh <- ode(yini,theta,schwarzschild,pars)

  r <- bh[,2]
  theta <- bh[,1]

  xy <- cbind(r*cos(theta),r*sin(theta))
  return(xy)
}


## setup:
jj <- 6
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l')


points(0,0,pch=16,cex=0.3)  # singularity
th <- seq(from=0,to=2*pi,len=100)
points(cos(th),sin(th),type='l')   # event horizon
## setup ends


xy <- stringpoints(y_start=9,initial_string_angle = -(pi-pi*1.42),theta=seq(from=0,to=-(pi+0.4),len=100))
#points(xy,type='l',col='red')

xy <- stringpoints(y_start=9,initial_string_angle = -pi*1.42,theta=seq(from=0,to=pi+0.4,len=100))
#points(xy,type='l',col='red')


thetas <- seq(from=0,to=pi,len=100)
for(x in c(1.1,1.2,1.3,1.5,2,3,4,5,6)){
  xy <- stringpoints(y_start=x,initial_string_angle = 0,theta=thetas)
  points(xy,type='l',col='blue')
  
  xy <- stringpoints(y_start=x,initial_string_angle = 0,theta=-thetas)
  points(xy,type='l',col='blue')
}









