
# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"
outputdir <- "simulations/output"
codedir <- "simulations/functions"

# Read toxin grids
data1 <- readRDS(file.path(outputdir, "contamination_simulations.Rds"))
data2 <- readRDS(file.path(outputdir, "contamination_simulations_with_hotspots.Rds"))

# Source functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))


# Experiment with management model (no seed)
################################################################################

# Extract toxin scenarios
########################################

# Extract toxin scenarios
iter_do <- 13
toxin_grid_sm <- data1 %>%
  filter(scenario=="small" & iter==iter_do)
toxin_grid_md <- data1 %>%
  filter(scenario=="medium" & iter==iter_do)
toxin_grid_lg <- data1 %>%
  filter(scenario=="large" & iter==iter_do)


# Small - no seed - random
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=20, perfect=F, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=12, perfect=F, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=6, perfect=F, plot=T)
quantify_performance(output)

# 2 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=2, perfect=F, plot=T)
quantify_performance(output)


# Small - no seed - perfect
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=20, perfect=T, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=12, perfect=T, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=6, perfect=T, plot=T)
quantify_performance(output)

# 2 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=2, perfect=T, plot=T)
quantify_performance(output)


# Medium - no seed - random
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_md, nstations=20, perfect=F, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_md, nstations=12, perfect=F, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_md, nstations=6, perfect=F, plot=T)
quantify_performance(output)

# 2 stations (failed occasionanlly?)
output <- simulate_mgmt(toxin_grid_md, nstations=2, perfect=F, plot=T)
quantify_performance(output)

# Medium - no seed - perfect
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_md, nstations=20, perfect=T, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_md, nstations=12, perfect=T, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_md, nstations=6, perfect=T, plot=T)
quantify_performance(output)

# 2 stations
output <- simulate_mgmt(toxin_grid_md, nstations=2, perfect=T, plot=T)
quantify_performance(output)



# Large - no seed - random
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=20, perfect=F, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=12, perfect=F, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=6, perfect=F, plot=T)
quantify_performance(output)

# 2 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=2, perfect=F, plot=T)
quantify_performance(output)


# Large - no seed - perfect
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=20, perfect=T, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=12, perfect=T, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=6, perfect=T, plot=T)
quantify_performance(output)

# 2 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=2, perfect=T, plot=T)
quantify_performance(output)




# Experiment with management model (seeded)
################################################################################

# Extract toxin scenarios
########################################

# Extract toxin scenarios
iter_do <- 53
toxin_grid_sm <- data2 %>%
  filter(scenario=="small" & iter==iter_do)
toxin_grid_md <- data2 %>%
  filter(scenario=="medium" & iter==iter_do)
toxin_grid_lg <- data2 %>%
  filter(scenario=="large" & iter==iter_do)


# Small - seeded - random
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=20, repeat_interval = 4,
                        perfect=F, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=12, repeat_interval = 3,
                        perfect=F, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=6, repeat_interval = 2,
                        perfect=F, plot=T)
quantify_performance(output)

# 2 stations (DOES NOT WORK!!!!!)
output <- simulate_mgmt(toxin_grid_sm, nstations=2, repeat_interval = 4,
                        perfect=F, plot=T)
quantify_performance(output)


# Small - seeded - perfect
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=20, repeat_interval = 4,
                        perfect=T, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=12, repeat_interval = 6,
                        perfect=T, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=6, repeat_interval = 2,
                        perfect=T, plot=T)
quantify_performance(output)

# 2 stations (DOES NOT WORK!!!!!)
output <- simulate_mgmt(toxin_grid_sm, nstations=2, repeat_interval = 1,
                        perfect=T, plot=T)
quantify_performance(output)


# Medium - seeded - random
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_md, nstations=20, repeat_interval = 7,
                        perfect=F, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_md, nstations=12, repeat_interval = 3,
                        perfect=F, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_md, nstations=6, repeat_interval = 3,
                        perfect=F, plot=T)
quantify_performance(output)

# 2 stations
output <- simulate_mgmt(toxin_grid_md, nstations=2, repeat_interval = 2,
                        perfect=F, plot=T)
quantify_performance(output)

# Medium - seeded - perfect
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_md, nstations=20, repeat_interval = 5,
                        perfect=T, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_md, nstations=12, repeat_interval = 2,
                        perfect=T, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_md, nstations=6, repeat_interval = 1,
                        perfect=T, plot=T)
quantify_performance(output)

# 2 stations
output <- simulate_mgmt(toxin_grid_md, nstations=2, repeat_interval = 4,
                        perfect=T, plot=T)
quantify_performance(output)



# Large - seeded - random
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=20, repeat_interval = 4,
                        perfect=F, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=12, repeat_interval = 3,
                        perfect=F, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=6, repeat_interval = 2,
                        perfect=F, plot=T)
quantify_performance(output)

# 2 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=2, repeat_interval = 6,
                        perfect=F, plot=T)
quantify_performance(output)


# Large - seeded - perfect
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=20, repeat_interval = 5,
                        perfect=T, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=12, repeat_interval = 4,
                        perfect=T, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=6, repeat_interval = 3,
                        perfect=T, plot=T)
quantify_performance(output)

# 2 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=2, repeat_interval = 6,
                        perfect=T, plot=T)
quantify_performance(output)





# Experimentign with 0 interval

# Small - seeded - random
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=20, repeat_interval = 0,
                        perfect=F, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=12, repeat_interval = 0,
                        perfect=F, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_sm, nstations=6, repeat_interval = 0,
                        perfect=F, plot=T)
quantify_performance(output)

# 2 stations (DOES NOT WORK!!!!!)
output <- simulate_mgmt(toxin_grid_sm, nstations=2, repeat_interval = 0,
                        perfect=F, plot=T)
quantify_performance(output)




# Large - seeded - perfect
########################################

# 20 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=20, repeat_interval = 0,
                        perfect=T, plot=T)
quantify_performance(output)

# 12 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=12, repeat_interval = 0,
                        perfect=T, plot=T)
quantify_performance(output)

# 6 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=6, repeat_interval = 0,
                        perfect=T, plot=T)
quantify_performance(output)

# 2 stations
output <- simulate_mgmt(toxin_grid_lg, nstations=2, repeat_interval = 0,
                        perfect=T, plot=T)
quantify_performance(output)

