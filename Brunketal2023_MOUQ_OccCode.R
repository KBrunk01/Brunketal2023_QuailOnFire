
########################################################
#
# Code for reproducing results from Brunk et al. 2023
#
# Quail on fire: changing fire regimes may benefit mountain 
#                quail in fire-adapted forests
#
# DOI : 10.1186/s42408-023-00180-9
#
########################################################

# Load packages
library(spOccupancy)

# Read in data file
setwd() # Set to location of saved data file
MOUQ <- readRDS("MOUQ_OccData.RData") # Read in data file

# Check structure of data
str(MOUQ)

# Specify detection model
Det <- ~ ssp.cat + scale(eff) + scale(cc) + 
  scale(elev) + scale(For_Evergr) + scale(tri) + (1|cell_id)

# Specify habitat model
Hab <- ~ scale(Latitude) + 
  scale(elev.resid) + 
  I(scale(elev.resid)^2) +
  scale(Shrub) + 
  scale(Grassland) + 
  scale(For_Evergr) +
  scale(For_DecidMixed) +
  scale(cc_cfo_mn) + 
  scale(cbh_cfo_mn) + 
  (1|cell_id)

# Specify fire model
Fire <- ~ scale(Latitude) + 
  scale(elev.resid) + 
  I(scale(elev.resid)^2) + 
  scale(fire1yr_high_prop) + 
  scale(fire2_5yr_high_prop) + 
  scale(fire6_10yr_high_prop) + 
  scale(fire11_35yr_high_prop) + 
  scale(fire1yr_lowmod_prop) + 
  scale(fire2_5yr_lowmod_prop) + 
  scale(fire6_10yr_lowmod_prop) + 
  scale(fire11_35yr_lowmod_prop) +  
  (1|cell_id)


# Initial values
mouq.inits <- list(alpha = 0,
                   beta = 0,
                   z = apply(MOUQ$y, 1, max, na.rm=T))

# Priors
mouq.priors <- list(alpha.normal = list(mean = 0, var = 2.72),
                    beta.normal = list(mean = 0, var = 2.72),
                    sigma.sq.p.ig = list(shape = 0.1, scale = 0.1),
                    sigma.sq.psi.ig = list(shape = 0.1, scale = 0.1))

# MCMC
n.samples <- 40000
n.burn <- 20000
n.thin <- 2
n.chains <- 3


# Run Habitat Model
HabMod <- PGOcc(occ.formula = Hab, 
              det.formula = Det, 
              data = MOUQ, 
              inits = mouq.inits, 
              n.samples = n.samples, 
              priors = mouq.priors, 
              n.omp.threads = 1, 
              verbose = T, 
              n.report = 1000, 
              n.burn = n.burn, 
              n.thin = n.thin, 
              n.chains = n.chains
             )

FireMod <- PGOcc(occ.formula = Fire, 
              det.formula = Det, 
              data = MOUQ, 
              inits = mouq.inits, 
              n.samples = n.samples, 
              priors = mouq.priors, 
              n.omp.threads = 1, 
              verbose = T, 
              n.report = 1000, 
              n.burn = n.burn, 
              n.thin = n.thin, 
              n.chains = n.chains
             )

## Note: Parameter estimates will be slightly different than those
# reported in the publication, due to the nature of a Bayesian analysis 
# (and the author's failure to set the seed). 
