## This file creates some variables to be used by trajectory_mercury.R
## to analyse Mercury's orbit.


G <-  6.674e-11     # big G in SI
M  <- 1.9885e30     # mass of Sun (kg)
M_merc <- 3.3011e23 # mass of Mercury (kg)
sol <- 299792458    # speed of light (m/s)

r_sun <- 695510e3   # radius of sun in meters


orbital_radius_venus <- 108000000e3 # Venus's orbital radius (m)
orbital_radius_earth <- 150000000e3 # Earth's orbital radius (m)

d <- G*M/sol^2  # mass of sun in meters
r_s <- 2*d      # Schwarzschild radius of Sun (=2M)
ecc_wiki <- 0.205630 # eccentricity of Mercury's orbit from wikipedia
per <- 87.969 * 3600*24   # period of Mercury's orbit (s)
merc_perihelion <- 46001200e3  # meters
merc_aphelion   <- 69816900e3


omega <- 2*pi/per         # mean angular velocity of Mercury, radian/sec

## Now calculate ellipse parameters from observational data:
a <- (merc_perihelion + merc_aphelion)/2
cee <- a-merc_perihelion
b <- sqrt(a^2-cee^2)
ecc <- sqrt(1-(b/a)^2)  # should match ecc_wiki above
ell <- (a^2-cee^2)/a  # semi-latus rectum

mu <- G*(M + M_merc)

# equation 19 of wikipedia says v = sqrt(mu/ell) * (1+e*cos(theta))
vmin <- sqrt(mu/ell)*(1-ecc)
vmax <- sqrt(mu/ell)*(1+ecc)

# Now we need to express the specific angular momentum in relativistic
# units in which G=c=1:
   
specific_angular_momentum_SI <- merc_perihelion * vmax

# consistency check:
error <- merc_perihelion * vmax - merc_aphelion * vmin
# 'error' should be zero to numerical precision

# Angular momentum has units m^2/s so:

h <- specific_angular_momentum_SI/sol #geometrized units


# Variables above can be used as follows:

if(FALSE){  # do not run; takes a long time
  out <- trajectory(merc_perihelion, m=d, h=h, tau=seq(from=0,to=35e17,len=5e6))
  plot(out$x,out$y,type='l',asp=1)
  ## this is done better in trajectory_mercury.R
}


conversion1 <- prod(c(  # used in the Rmd file
    seconds_per_minute = 60, 
    minutes_per_hour   = 60,
    hours_per_day      = 24,
    days_per_year      = 365.25,
    years_per_century  = 100,
    degrees_per_radian = 180/pi,
    minutes_per_degree = 60,
    seconds_per_minute = 60
))

seconds_per_orbit <- prod(c(
    seconds_per_minute = 60,
    minutes_per_hour   = 60,
    hours_per_day      = 24,
    days_per_orbit     = 87.969
))

seconds_per_century <- prod(c(
    seconds_per_minute = 60,
    minutes_per_hour   = 60,
    hours_per_day      = 24,
    days_per_year      = 365.25,
    years_per_century  = 100
))

orbits_per_century <- seconds_per_century/seconds_per_orbit
arcseconds_per_radian <- prod(
    c(
        degrees_per_radian = 180/pi,
        minutes_per_degree = 60,
        seconds_per_minute = 60
        ))


precession <- 24*pi^3*a^2/(per^2*sol^2*(1-ecc^2))  # radians/orbit
arcseconds_per_orbit <- precession*arcseconds_per_radian
arcseconds_per_century <- arcseconds_per_orbit * orbits_per_century
