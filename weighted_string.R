library("deSolve")

rdashdash <- function(r,rdash,lambda,M=1/2){
    x <- sqrt(1-2*M/r)
    return(
    (M*(4+lambda*(4-3*x))*r^2 + (-1+lambda*(-1+x))*r^3 + M*(3 + lambda*(3-2*x))*rdash^2 +
     2*r*(M^2*(-2+lambda*(-2+x)) + (-1+lambda*(-1+x))*rdash^2))/((-1+lambda*(-1+x))*r*(-2*M+r))
    )
}

string <- function(lambda){

    parameters <- c(lambda = lambda, M = 1/2)
    state <- c(r = 2, rdash = 0)
    times <- seq(from = 0, to = 0.5, by = 0.01)

    Lorenz <- function(t, state, parameters){
        with(as.list(c(state, parameters)), {
            dr <- rdash
            drdash <- rdashdash(r, rdash, lambda, M)
            list(c(dr, drdash))
        })
    }
    
    out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
    
    
    phi <- out[,1]
    r <- out[,2]
    
    return(cbind(r*cos(phi),r*sin(phi)))
}

plot(c(-3,3),c(-3,3),type="n",asp=1)
th <- seq(from=0,to=2*pi,len=100)
points(cos(th),sin(th),type='l')  # event horizon
points(string(0)) 

