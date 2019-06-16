G <-  6.674e-11  # big G in SI
M  <- 1.9885e30 # mass of Sun (kg)
M_merc <- 3.3011e23 # mass of Mercury
sol <- 299792458    # speed of light

d <- G*M/sol^2  # mass of sun
r_s <- 2*d  # Schwarzschild radius of Sun (=2M)
ecc_wikipedia <- 0.205630 # eccentricity of Mercury's orbit as given by wikiped
per <- 87.969 * 3600*24
	
merc_perihelion   <- 46001200e3  # meters
merc_aphelion <- 69816900e3

a <- (merc_perihelion + merc_aphelion)/2
cee <- a-merc_perihelion
b <- sqrt(a^2-cee^2)
ecc <- sqrt(1-(b/a)^2)  # should match ecc above
ell <- (a^2-cee^2)/a  # semi-latus rectum

mu <- G*(M + M_merc)
# equation 19 says v = sqrt(mu/ell) * (1+e*cos(theta))
vmin <- sqrt(mu/ell)*(1-ecc)
vmax <- sqrt(mu/ell)*(1+ecc)

# Now we need to express the specific angular momentum in relativistic
#  units in which G=c=1:

   
specific_angular_momentum_SI <- merc_perihelion * vmax

# consistency check:
error <- merc_perihelion * vmax - merc_aphelion * vmin
print(error / specific_angular_momentum_SI)

# Angular momentum has units m^2/s so

h <- specific_angular_momentum_SI/sol #geometrized units





