## Here are some lines indexed by distance from the singularity to the
## point of inflection.

rm(list=ls())
pars <- c(eel=1)  # dummy

 0.57721566490153286060 -> em  # Euler-Mascheroni

source("usefulfuncs.R")

## setup:
jj <- 6.1
par(pty='s')
plot(NULL,
     asp = 1,  
     xlim = c(-jj,jj),ylim=c(-jj,jj),
     xlab = '2GM/c^2',ylab='2GM/c^2',
     main = 'Light inextensible strings under tension
in the Schwarzschild geometry')


points(0,0,pch=16,cex=0.3)  # singularity
th <- seq(from=0,to=2*pi,len=300)
points(cos(th),sin(th),type='l',lwd=0.5)   # event horizon

#points(2*cos(th),2*sin(th),type='l',lwd=0.1) 
## setup ends

dist <- seq(from=1.8,to=5,len=17)
mindist <- dist + NA  # minimal distances

rdash <- sqrt(2*dist*(dist-1))

critical_start_angle <- atan(rdash/dist)
start_angles <- critical_start_angle # one string per start angle, each one a different colour

n <- length(start_angles)
cols <- rainbow(n+round(n/7))

theta_start <- 0

theta_end1 <- 0.5 + 0*start_angles
theta_end2 <- 2.4 + 0*start_angles

theta_end1[dist<2.3] <- 0.7


theta_end2[cont(dist, c(1.6,1.9))] <- pi + 3.2
theta_end2[cont(dist, c(1.9,2.1))] <- pi + 1.5
theta_end2[cont(dist, c(2.1,2.3))] <- pi + 1.0
theta_end2[cont(dist, c(2.3,2.5))] <- pi + 0.7
theta_end2[cont(dist, c(2.5,2.7))] <- pi + 0.5
theta_end2[cont(dist, c(2.7,3.0))] <- pi + 0.1
theta_end2[cont(dist, c(3.0,3.5))] <- pi + 0.0
theta_end2[cont(dist, c(3.5,9.9))] <- pi - 0.3
for(i in seq_along(start_angles)){

  ## First the upwards strings:
  xy <-
    stringpoints(
        y_start = dist[i],
        initial_string_angle = start_angles[i],
        theta = seq(from=theta_start,to=theta_end1[i],len=100)
    )
  points(xy,type='l',col=cols[i],lwd=2)
}

for(i in seq_along(start_angles)){
  xy <-
    stringpoints(
        y_start = dist[i],
        initial_string_angle = -(pi-start_angles[i]),
        theta = -seq(from=theta_start,to=theta_end2[i],len=1000)
    )

  points(xy,type='l',col=cols[i],lwd=2)
  mindist[i] <- sqrt(min(rowSums(xy^2)))
  points(dist[i],0,pch=16,cex=0.6)
}

legend("topright",
       lty=c(1,1,NA,1),
       pch=c(NA,NA,16,NA),
       lwd=c(2,2,0,0.5),
       legend = c(
           'light inextensible',
           'strings under tension',
           'point of inflection',
           'event horizon'
       ),
       col=c(cols[c(1,length(cols)-3)],"black")
       )
