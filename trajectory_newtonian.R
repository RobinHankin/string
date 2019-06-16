## Plots a trajectory of an (infinitesimal mass) object orbiting a
## black hole, under the Newtonian approximation of slow speeds and
## large distances compared with the Schwarzschild radius.  We see a
## closed elliptical orbit as per Keplerian dynamics.

## Compare the result of trajectory.R which shows the same simulation
## but without making the Newtonian approximation.

## Works standalone but is intended to be called by maker.R which
## creates a pdf file.

## Notation follows Rindler, p239


source("usefulfuncs.R")
source("usefulgeodesicfuncs.R")
     
jj <- trajectory(30, 1/2, 3, GR=FALSE) # m=1/2 -> Schwarzschild radius = 1
plot(jj$x,jj$y, asp=1, type='l', axes=FALSE, xlab='', ylab='',main='Newtonian orbit',sub='Three successive orbits plotted, note closed path')
polargrid(r=10*(1:3))
event_horizon()
