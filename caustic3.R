x <- seq(from=0,to=1,len=400)
plot(0:1,0:1,asp=1,type="n",axes=FALSE,xlab='',ylab='')
segments(x0=x,y0=0, x1=x + sin(sin(x*7)*10 + x^2*10)/25,y1=1,lwd=0.3)


