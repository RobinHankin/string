##  Deflection of light grazing the limb of the sun.  Einstein
##  predicted (and Eddington measured) 1.75 seconds of arc and I will
##  reproduce that figure numerically.

## There is no graphical component here.

## More documentation is given in light_starts_at_r_equals_2.R

source("usefullightfuncs.R") # defines lightpoints()

G <- 6.67408e-11  # Gravitational constant (SI)
M <- 1.989e30     # mass of sun (SI)
sol <- 299792458  # speed of light
radius_of_sun <- 695510e3 # (m)
r_schwarz <- 2*G*M/sol^2 # Schwarzschild radius of sun (m)
r_start  <- radius_of_sun/r_schwarz # starting radius for light ray

initialangletotangent <- 0  # tangential
 
small <- 0.1

final_angle <- pi/2
final_delta <- 0.00001

light <-
    nullgeodesic(
        r_start=r_start,  
        dubydphistart = tan(initialangletotangent)/r_start,
        phi = seq(from=0,to=final_angle,by=final_delta),
        include=TRUE
    )

jj <- tail(light,2)

deflection <- -2*as.vector(diff(jj[,1]) / diff(jj[,2])) # radians (NB double)

print(deflection*180/pi*60^2)  # arcseconds; compare Einstein's value of 1.75
