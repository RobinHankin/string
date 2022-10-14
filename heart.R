drawstring <- function(n=256,d=pi/(30 + 1/4),sleep=0.1, ...){
    theta <- rep(0,n)
    theta[1] <- 0    # start point
    theta[2] <- pi  # start point

    for(i in 3:n){
        if(i%%4==2){  # starts here
            theta[i] <- theta[i-3] + d
        } else if(i%%4==3){
            theta[i] <- theta[i-1] + d
        } else if(i%%4==0){
            theta[i] <- theta[i-3] + 2*d
        } else if(i%%4==1){
            theta[i] <- theta[i-1] + 2*d
        } else {
            stop()
        }
    }
    
    plot(c(-1,1),c(-1,1),asp=1,type="n",axes=FALSE,xlab='',ylab='')
    ## o <- seq(from=0,to=2*pi,len=100)
    ## points(cos(o),sin(o),type="l",col="lightgray")

    for(i in seq(from=1,to=n)){
        print(i)
        segments(
            x0=cos(theta[i  ]),
            y0=sin(theta[i  ]),
            x1=cos(theta[i+1]),
            y1=sin(theta[i+1]),
            ...
        )
        if(sleep>0){Sys.sleep(sleep)}
    }
}
    
    

