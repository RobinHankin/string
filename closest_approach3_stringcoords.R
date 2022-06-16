## Shows paths of light inextensible string near a black hole but
## using string coordinates.

## This script runs standalone but is designed to be called from
## maker.R [cf Makefile], which creates the pdf file
## "closest_approach3_stringcoords.pdf"

source("usefulfuncs.R") # defines stringpoints() and dseq()
source("stringfuncs.R") # defines u1() and u2() ; file stringfuncs.R
                        # lives in the schwarzschild repo under R/

domask <- TRUE


## plot setup:
jj <- 6
par(xpd=TRUE)
plot(NULL,asp=1,xlim=c(-jj,jj),ylim=c(-jj,jj),type='l',axes=F,xlab='',ylab='')
userainbow <- FALSE   # colour of strings (FALSE = all strings blue)
## setup ends


dist <- dseq(from=(1.00012), to=1.076109317,len=70,power=3)

cutoffmatrix <- matrix(c(
    1.0000, pi + 3.30 -3.3 ,
    1.00011, pi + 4.6 ,
    1.00012, pi + 4.4,
    1.00013, pi + 4.3,
    1.00014, pi + 4.3,
    1.00015, pi + 4.2,
    1.00016, pi + 4.1,
    1.00017, pi + 4.1  ,
    1.00020, pi + 4.06  ,
    1.00023, pi + 3.95  ,
    1.00025, pi + 3.90 ,
    1.00027, pi + 3.80 ,
    1.00030, pi + 3.70  ,
    1.00035, pi + 3.60 ,
    1.00040, pi + 3.60 ,
    1.00045, pi + 3.40 ,
    1.0005, pi + 3.345 ,
    1.0006, pi + 3.20  ,
    1.0007, pi + 3.10  ,
    1.0008, pi + 3.00 ,
    1.0009, pi + 3.00  ,
    1.0010, pi + 2.90  ,
    1.0011, pi + 2.85  ,
    1.0012, pi + 2.75  ,
    1.0014, pi + 2.68  ,
    1.0016, pi + 2.56  ,
    1.0018, pi + 2.47  ,
    1.0020, pi + 2.45  ,
    1.0022, pi + 2.32  ,
    1.0025, pi + 2.30  ,
    1.0028, pi + 2.20  ,
    1.0030, pi + 2.15  ,
    1.0035, pi + 2.05  ,
    1.0040, pi + 1.95  ,
    1.0045, pi + 1.88  ,
    1.0050, pi + 1.82  ,
    1.0055, pi + 1.70  ,
    1.0060, pi + 1.69  ,
    1.0070, pi + 1.55  ,
    1.0080, pi + 1.45  ,
    1.0090, pi + 1.35  ,
    1.010, pi + 1.27  ,
    1.011, pi + 1.20  ,
    1.012, pi + 1.20  ,
    1.013, pi + 1.15  ,
    1.015, pi + 1.05  ,
    1.017, pi + 0.91  ,
    1.020, pi + 0.83  ,
    1.025, pi + 0.71  ,
    1.030, pi + 0.57  ,
    1.035, pi + 0.44  ,
    1.040, pi + 0.40  ,
    1.045, pi + 0.30  ,
    1.050, pi + 0.20  ,
    1.060, pi + 0.11  ,
    1.070, pi + 0.05-0.05  ,
    1.080, pi + 0.00
    ),byrow=TRUE,ncol=2)
f <- approxfun(cutoffmatrix[,1],cutoffmatrix[,2])
## Thus f(1.05) = pi + 0.2


for(i in seq_along(dist)){

  if(userainbow){
    stringcol <- rainbow(length(dist))[ii]
  } else {
    stringcol <- "blue"
  }

  tseq <- seq(from=0,to=f(dist[i]),len=100)
  o <- stringpoints(y_start=dist[i],initial_string_angle = 0,theta=tseq,give=TRUE)
  u <- u1(o[,2])
  theta <- o[,1]
  xy <- cbind(x=u*cos(theta),y=u*sin(theta))
  points(xy,type='l',col=stringcol,lwd=2)
  
#  xy <- stringpoints(y_start=dist[i],initial_string_angle = 0, theta=-tseq)
#  points(xy,type='l',col=rainbow(length(dist))[i])
}

extra <- dist[52]
tseq <- seq(from=0,to=f(extra),len=100)
o <- stringpoints(y_start=extra ,initial_string_angle = 0,theta=tseq,give=TRUE)
u <- u1(o[,2])
theta <- o[,1]
xy <- cbind(x=u*cos(theta),y=u*sin(theta))

points(xy,type='l',col="green",lwd=3)
xy[,2] <- -xy[,2]
points(rbind(xy,jj),type='l',col="green",lwd=3)
## mask strings too far from the black hole:
if(domask){mask(7)}

polargrid(rlab=6.5)

