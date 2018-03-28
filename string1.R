## lots of blue strings with varying closest approach distances.

rm(list=ls())

source("usefulfuncs.R") # defines stringpoints()

pars <- c(eel=1)  # dummy


## plot setup:
jj <- 6
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l')

event_horizon()
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









