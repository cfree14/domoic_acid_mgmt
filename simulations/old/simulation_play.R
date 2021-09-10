

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)

# NLMR:
# https://ropensci.github.io/NLMR/
# devtools::install_github("ropensci/NLMR")
library(NLMR)
library(landscapetools)

# gstat presents another approach
# https://www.aspexit.com/en/simulating-spatial-datasets-with-known-spatial-variability/
library(gstat)

# Parameters
################################################################################

# Height
ndeg <- 10
res <- 0.1
ncells <- ndeg/res

# Width
nmonths <- 8
ndays <- nmonths * 30


# Simulations
################################################################################

# Uniform linear dissipation
?nlm_planargradient
ras1 <- NLMR::nlm_planargradient(ncol=ndays, nrow=ncells, resolution = 1, direction = 90)
landscapetools::show_landscape(ras1)

# Diagonal linear dissipation
ras2 <- NLMR::nlm_planargradient(ncol=ndays, nrow=ncells, resolution = 1, direction = -45)
landscapetools::show_landscape(ras2)

# Bulge linear dissipation
?nlm_distancegradient
ras3 <- NLMR::nlm_distancegradient(ncol=ndays, nrow=ncells, resolution = 1, origin=c(50, 51, 0, 1))
landscapetools::show_landscape(ras3)

# Bulge linear dissipation
?nlm_distancegradient
ras3 <- NLMR::nlm_distancegradient(ncol=ndays, nrow=ncells, resolution = 1, origin=c(50, 51, 0, 0))
landscapetools::show_landscape(ras3)

# Spatially autocorrelated
?nlm_gaussianfield
ras4 <- nlm_gaussianfield(ncol=ndays, nrow=ncells, resolution = 1, autocorr_range = 10, mag_var=5, nug=1)
landscapetools::show_landscape(ras4)

# Other attractive options
# ?nlm_fbm
# ?nlm_mpd






