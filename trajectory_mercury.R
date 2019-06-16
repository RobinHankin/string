## Plots a trajectory of an (infinitesimal mass) object orbiting a
## black hole, showing non-closed orbit.  Works standalone but is
## intended to be called by maker.R which creates a pdf file.

## Notation follows Rindler, p239

source("usefulgeodesicfuncs.R")
source("mercury.R")  # orbital parameters for Mercury in geometrized units
     
out <- trajectory(merc_perihelion, m=d, h=h, tau=seq(from=0,to=35e17,len=5e6))
plot(out$x,out$y, asp=1, type='l', axes=FALSE, xlab='', ylab='')
event_horizon()
dev.new()


out <- as.data.frame(out)

jj1 <- c(NA,out$rdot)
jj2 <- c(out$rdot,NA)

change <- (jj1>0) & (jj2<0)

tee    <- out$time[change]

phi <- out$phi[change]
phi <- (phi) %% (2*pi)


plot(tee,phi-pi)
