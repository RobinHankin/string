`redpoints` <- function(offset, quat, thetavals, rvals){  #radial lines
    out <- matrix(NA,1,3)
    
    for(theta in thetavals){
        x <- rvals*cos(theta)
        y <- rvals*sin(theta)
        z <- theta*0 -4*sqrt(rvals-1)
        
        out <- rbind(out, cbind(x,y,z), NA)  # NA makes lines distinct
    }

    out <- sweep(out,2,offset,"+")
    return(rotate(out,quat))
}

`bluepoints` <- function(offset, quat,thetavals,rvals){  # circles
    out <- matrix(NA,1,3)
    for(r in rvals){
        x <- r*cos(thetavals)
        y <- r*sin(thetavals)
        z <- thetavals*0 -4*sqrt(r-1)
    
        out <- rbind(out, cbind(x,y,z), NA)  # NA makes lines distinct
    }
    out <- sweep(out,2,offset,"+")
    return(rotate(out,quat))
}

 single_stringpoints <- function(offset,quat){

     dist_from_hole <- 1.4   # distance of closest approach

     xy <-
         stringpoints(
             y_start = dist_from_hole,
             initial_string_angle = 0,
             theta = seq(from=0,to=pi*0.67,len=100)
         )
     
     ## xy shows string from closest approach moving outward anticlockwise:
     r <- sqrt(rowSums(xy^2))
     x <- xy[,1]
     y <- xy[,2]
     z <- -4*sqrt(r-1)

     ## now add string from closest approach moving clockwise [rev() means
     ## that the points are in sequence along the string]:
     x <- c(rev(x),x)
     y <- c(-rev(y),y)
     z <- c(rev(z),z)
     
     out <- cbind(x,y,z)
     out <- sweep(out,2,offset,"+")
     return(rotate(out,quat))
 }

`allstringpoints` <- function(offset, quat){ # string, duh
    out <- matrix(NA,1,3)
    for(i in seq_len(nrow(offset))){
        xyz <- single_stringpoints(offset=offset[i,],quat=quat[i])
        out <- rbind(out, xyz, NA)  # NA makes lines distinct
    }
    return(out)
}
