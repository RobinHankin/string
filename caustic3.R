x <- seq(from=0,to=1,len=400)
plot(0:1,0:1,asp=1,type="n",axes=FALSE,xlab='',ylab='')
for(a in x){
  segments(x0=a,y0=0,x1=a + sin(sin(a*7)*10 + a^2*10)/25,y1=1,lwd=0.3)
}
