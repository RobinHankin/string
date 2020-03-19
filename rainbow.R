## This function plots a near-field raytracing diagram of a (2D)
## droplet, inspired by J. E. McDonald 1962. "Caustics of the primary
## rainbow", American Journal of Physics 31, p282

source("usefulfuncs.R")

## Minor trig ratios (not implemented in base R, for some reason):
`cosec` <- function(x){1/sin(x)}
`cot` <- function(x){1/tan(x)}
`sec` <- function(x){1/cos(x)}

n <- 4/3 # refractive index


## First some helper functions.  Function intersect() gives the
## intersection of line (y-y0)/(x-x0) = g with x^2+y^2=1.  It returns
## a two-row matrix with rows corresponding to Cartesian coordinates
## of the intersection points.

`intersect` <- function(v){
  x0 <- v[1]
  y0 <- v[2]
  g <- tan(v[3])

  A <- 1+g^2
  B <- 2*g*(y0-x0*g)
  C <- (y0-x0*g)^2-1

  x <- c(
      (-B+sqrt(B^2-4*A*C))/(2*A),
      (-B-sqrt(B^2-4*A*C))/(2*A)
  )

  y <- y0 + g*(x-x0)
  return(cbind(x,y))
}


## Draw a segment of a radial line
`bitofradial` <- function(x,y,inner=0.9,outer=1.1,...){
  segments(inner*x,inner*y,outer*x,outer*y,...)
}

## Function f() defined below takes a single argument d, the distance
## from optical axis of the drop to the incoming ray.  It returns a
## 3x3 matrix M with rows corresponding to the three refractive
## points. The Cartesian coordinates of point i are given by M[i,1:2]
## and the angle of the outgoing ray from that point is given by
## M[i,3].  Angles are given in radians in the sense defined by
## McDonald.  Note that this system is easily generalizable to the
## higher order bows.

## Notation follows McDonald's diagram 1 where possible, except the
## radius of the drop is unity: x^2+y^2=1.

## We follow the "Arbitrary ray".

f <- function(d){
  i <- asin(d)
  
  M <- matrix(NA,3,3)
  colnames(M) <- c("x","y","angle")

  p1 <- c(-cos(i),sin(i))


  M[1,1] <- p1[1]  # x
  M[1,2] <- p1[2]  # y
  print(i)
  M[1,3] <- -i +asin(sin(i)/n)

  jj <- intersect(M[1,,drop=TRUE])
  dist_squared <- (jj[,1]-M[1,1])^2 + (jj[,2]-M[1,2])^2
  p2 <- jj[which.max(dist_squared),,drop=TRUE] 
  ## Now p2 is the coordinates of the second point.

  ## Fill in second row of M:
  M[2,1:2] <- p2
  M[2,3] <- -M[1,3]-2*atan(p2[1]/p2[2])

  jj <- intersect(M[2,,drop=TRUE])
  dist_squared <- (jj[,1]-M[2,1])^2 + (jj[,2]-M[2,2])^2
  p3 <- jj[which.max(dist_squared),,drop=TRUE] 
  ## Now p is the coordinates of the third point.

  M[3,1:2] <- p3
  jj <- n*sin(atan(p3[1]/p3[2]))
  if(jj<1){
    M[3,3] <- asin(jj) + atan(p3[1]/p3[2])
  } else {
    M[3,3] <- NA
  }


  
  return(M)
}

## First plot the bounding circle:
size <- 0.3
a <- seq(from=0,to=2*pi,len=1000)  # 'a' for angle
plot(sin(a),cos(a),asp=1,type='l',xlab='',ylab=''
     ,xlim=c(-size,size),ylim=c(-1-size,-1+size))

drawray <- function(d,...){
  M <- f(d)
  segments(x0=-10,y0=d,x1=-sqrt(1-d^2),y1=d,col='red')
  segments(
      x0=M[1,1],y0=M[1,2],
      x1=M[2,1],y1=M[2,2],
      ...)
  segments(
      x0=M[2,1],y0=M[2,2],
      x1=M[3,1],y1=M[3,2],
      ...)
  if(M[3,3]>0){
  segments(
      x0=M[3,1],y0=M[3,2],
      x1=M[3,1]-0.7*cos(M[3,3]),
      y1=M[3,2]-0.7*sin(M[3,3]),
      ...)
  }
   
#  bitofradial(M[1,1],M[1,2],lty=3)
#  bitofradial(M[2,1],M[2,2],lty=3)
#  bitofradial(M[3,1],M[3,2],lty=3)
}

for(a in seq(from=0.62,to=0.80,by=0.004)){
  M <- f(a)
  if(!is.na(M[3,3])){  drawray(a,lwd=0.2)}
}


drawray(atan(1/n),col='red',lwd=1)

## Axial line is indeterminate, has to be plotted explicitly:
abline(h=0)

## Mask outside of circle:
#mask(1.001)
