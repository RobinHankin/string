dashdash <- function(r){-1+r}   # r'' as a function of r provided r'=0

radius <- function(r){  # radius of curvature, as a function of r, if r'=0; 
  R <- r^3/(r^2 -r*dashdash(r))
  return(abs(R))
}

r <- seq(from=1.1,to=10,len=100)
plot(c(0,r),c(NA,1/radius(r)),ylim=c(0,10))
abline(h=0)
