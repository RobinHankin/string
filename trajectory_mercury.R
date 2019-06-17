## Plots a trajectory of an (infinitesimal mass) object orbiting a
## black hole, showing non-closed orbit.  Works standalone but is
## intended to be called by maker.R which creates a pdf file.

## Notation follows Rindler, p239

source("usefulgeodesicfuncs.R")
source("mercury.R")  # orbital parameters for Mercury in geometrized units
     
out <- trajectory(merc_perihelion, m=d, h=h, tau=seq(from=0,to=35e16,len=1e7))

plot(out$x,out$y, asp=1, type='l', axes=FALSE, xlab='', ylab='')
event_horizon()

out <- as.data.frame(out)

jj1 <- c(NA,out$rdot)
jj2 <- c(out$rdot,NA)

change <- (jj1>0) & (jj2<0)

tee    <- out$time[change]

phi <- out$phi[change]
phi <- (phi) %% (2*pi)

dev.new()
plot(tee,phi-pi)


## Now calculate perihelion advance in seconds of arc per century:
conversion <-
  c(
      seconds_per_minute = 60,
      minutes_per_hour   = 60,
      hours_per_day      = 24,
      days_per_year      = 365.25,
      years_per_century  = 100,
      degrees_per_radian = 180/pi,
      minutes_per_degree = 60,
      seconds_per_minute = 60
  )

lm(phi~tee)$coefficients[2] * prod(conversion)



## Compare with Einstein's value of 44
