# Numerical verification of the Shapiro delay experiment

source("usefulnullgeodesicfuncs.R")

st <- function(x){(x-min(x))/(max(x)-min(x))}

r0 <- r_sun/d # grazing incidence of the sun in Schwarzschild radii
b_radial <- r0/sqrt(1-1/r0)

small <- 1e-6  # needed to prevent numerical problems

d_earth <- seq(from=r_earth/r_s * 0.999, to=r_earth/r_s * 1.001,len=100)
d_venus <- seq(from=r_venus/r_s * 0.999, to=r_venus/r_s * 1.001,len=100)


out_earth <- nullgeodesic(r_start=r0,b=b_radial*(1-small),tau=c(0,d_earth))
out_venus <- nullgeodesic(r_start=r0,b=b_radial*(1-small),tau=c(0,d_venus))

g_earth <- approxfun(out_earth$r,out_earth$r)
g_venus <- approxfun(out_venus$r,out_venus$r)


transit_time_relativistic  <-   # transit time in seconds
(g_earth(r_earth)+g_venus(r_venus))*r_s/sol

  
transit_time_classical <- (r_earth + r_venus)/sol # seconds

plot(out_earth$x,out_earth$y,type='o',col='red',cex=st(out_earth$time),asp=1)
event_horizon()



shapiro_delay <- transit_time_relativistic - transit_time_classical
# compare observation of 200 microseconds (!)


