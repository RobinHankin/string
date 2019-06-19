##  Deflection of light grazing the limb of the sun.  Einstein
##  predicted (and Eddington measured) 1.75 seconds of arc and I will
##  reproduce that figure numerically.

## There is no graphical component here.

## More documentation is given in light_starts_at_r_equals_2.R

source("usefullightfuncs.R") # defines nullgeodesic()


G <- 6.67408e-11  # Gravitational constant (SI)
M <- 1.989e30     # mass of sun (SI)
sol <- 299792458  # speed of light
radius_of_sun <- 695510e3 # (m)
r_schwarz <- 2*G*M/sol^2 # Schwarzschild radius of sun (m)
r_start  <- radius_of_sun/r_schwarz # starting radius for light ray


cat(paste("Einstein's prediction: ",4*G*M/sol^2/radius_of_sun*180/pi*60^2,"\n"))
cat(paste("Eddington's measurement: ",1.75,"\n"))
## calculated value printed at the end
cat("calculating...\n")
Sys.sleep(10)  # dramatic pause (the actual calculation is almost instantaneous)




initialangletotangent <- 0  # tangential ray
 
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

angle <- as.vector(diff(jj[,1]) / diff(jj[,2]))  # radians

deflection <- -2*angle

## In the above, note multiplication by [minus] two because the
## astronomical measurement---which we are trying to predict---is of
## total deflection of distant starlight.  The total deflection is
## comprised of deflection in the ingoing leg *plus* deflection on the
## outgoing leg.  Function nullgeodesic() gives deflection on the
## outgoing leg, as the light starts at (r=radius_of_sun, phi=0) [that
## is, at the 3 o'clock position, at the radius of the sun, moving
## parallel to the y-axis].  Variable 'angle' is the angle that the
## light beam makes from the y-axis, at a large distance [one day I'll
## get round to deriving this properly] from the sun.  Ingoing
## deflection and outgoing deflection are equal by symmetry, and we
## need to add the two deflections, which is why we have a factor of 2
## here.



arcseconds <- deflection*180/pi*60^2


cat(paste("direct numerical integration:", arcseconds, "\n"))
