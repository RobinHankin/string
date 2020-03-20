## This function plots a near-field raytracing diagram of a (2D)
## droplet, inspired by J. E. McDonald 1962. "Caustics of the primary
## rainbow", American Journal of Physics 31, p282

source("usefulfuncs.R")
source("usefulrainbowfuncs.R")

n <- 4/3 # refractive index



## First plot the bounding circle:
size <- 2.0
a <- seq(from=0,to=2*pi,len=1000)  # 'a' for angle
plot(sin(a),cos(a),asp=1,type='l',xlab='',ylab='',xlim=c(0.25,0.35),ylim=c(-1.1,-0.8))
plot(sin(a),cos(a),asp=1,type='l',xlab='',ylab='',xlim=c(-5,5))

small <- 1e-5
for(a in seq(from=0.52,to=sin(2*asin(1/n))-small,by=0.002)){
  M <- f(a)
#  if(!is.na(M[3,3])){  drawray(a,lwd=0.2)}
drawray(a,lwd=0.2)
}

drawray(atan(1/n),col='red',lwd=1)
drawray(sin(2*asin(1/n))-small,col='blue')
M <- f(sin(2*asin(1/n))-small)


## Axial line is indeterminate, has to be plotted explicitly:
abline(h=0)

## Mask outside of circle:
#mask(1.001)
