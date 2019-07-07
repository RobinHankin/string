## Plots a trajectory of an (infinitesimal mass) object orbiting a
## black hole, showing the effect of the relativistic correction,
## specifically GR's prediction of a non-closed orbit.

## Compare the result of trajectory_newtonian.R which shows the same
## simulation but under the Newtonian approximation.

## Works standalone but is intended to be called by maker.R which
## creates a pdf file.

## Notation follows Rindler, p239


source("usefulfuncs.R")
source("usefulgeodesicfuncs.R")
     
jj <- trajectory(30, 1/2, 3) # m=1/2 -> Schwarzschild radius = 1
plot(jj$x,jj$y, asp=1, type='l', axes=FALSE, xlab='', ylab='',main='Geodesics in the Schwarzschild geometry')
polargrid(r=10*(1:3))
event_horizon()
