

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)
library(fitdistrplus)

# Directories
datadir <- "data/merged/processed"
plotdir <- "figures"

# Read data
data <- readRDS(file=file.path(datadir, "CA_OR_WA_da_survey_results.Rds"))


# Fit data
################################################################################


# Predictions
props <- seq(0,1, 0.01)
gamfit <- mgcv::gam(meanlog ~ s(pover_fit, bs = "cs"), data=data)
meanlogs <- predict(gamfit, newdata = tibble(pover_fit=props))

# Threshold
da_ppm_thresh <- 30

# Function to calculate sdlog
calc_sdlog1 <- function(meanlog, pover){
  sdlog <- (log(da_ppm_thresh) - meanlog) / (sqrt(2)*pracma::erfinv(1-2*pover))
  sdlog <- abs(sdlog)
  return(sdlog)
}

# Function to calculate sdlof
calc_sdlog2 <- function(meanlog, punder){
  sdlog <- ( log(da_ppm_thresh)-meanlog ) / (sqrt(2)*pracma::erfinv(2*punder-1))
  return(sdlog)
}

# Function to calculate sdlog
# meanlog <- 3.011; pover <- 0.38; sdlog <- 1
calc_sdlog3 <- function(meanlog, pover){

  # Convert p(over) to p(under)
  punder <- 1 - pover

  # Compare the target p(under) with the calculated p(under)
  minDiff <- function(sdlog){
    punder_i <- plnorm(q=30, meanlog=meanlog, sdlog=sdlog)
    diff_i <- abs(punder_i - punder)
    return(diff_i)
  }

  # Find the sdlog required to make X% of crabs be over the threshold given a mean
  fit <- optimize(f=minDiff, lower=0, upper=5)
  sdlog <- fit$minimum
  return(sdlog)
}


# Build data frame
pred_df <- tibble(pover=props,
                  meanlog=meanlogs) %>%
  # Calculate p(under)
  mutate(punder=1-pover) %>%
  # Derive DA (ppm)
  mutate(da_ppm_med=exp(meanlog)) %>%
  # Derive sdlog for achieving X% over threshold
  rowwise() %>%
  mutate(sdlog=calc_sdlog3(meanlog, pover)) %>%
  ungroup() %>%
  # Calculate % over threshhold to check
  mutate(pover_check=1-plnorm(da_ppm_thresh, meanlog=meanlog, sdlog=sdlog)) %>%
  # Calculate median/CV of distribution
  mutate(median_ppm=exp(meanlog),
         cv=sqrt(exp(sdlog^2)-1))

# C. log(mean)
g3 <- ggplot(data, aes(x=pover_fit, y=sdlog)) +
  # Plot regression
  geom_smooth(method="gam", se=TRUE,
              color="black", fill="grey80") +
  # Plot observations
  geom_point(color="grey40") +
  # Plot fitted values
  geom_point(data=pred_df, mapping=aes(x=pover, y=sdlog)) +
  # Labels
  labs(x="Proportion above\naction threshold",
       y="sdlog", tag="C") +
  # Theme
  theme_bw()
g3


# Plot data
################################################################################

# A. Mean
g1 <- ggplot(data, aes(x=pover_fit, y=da_ppm_med)) +
  # Plot regression
  geom_smooth(method="gam", se=TRUE,
              color="black", fill="grey80") +
  # Plot observations
  geom_point(color="grey40") +
  # Plot fitted values
  geom_point(data=pred_df, mapping=aes(x=pover, y=da_ppm_med)) +
  # Labels
  labs(x="Proportion above\naction threshold",
       y="Median contamination (ppm)", tag="A") +
  # Theme
  theme_bw()
g1

# B. log(mean)
g2 <- ggplot(data %>% filter(!is.na(meanlog)), aes(x=pover_fit, y=meanlog)) +
  # Plot observations
  geom_point(color="grey40") +
  # Plot regression
  geom_smooth(method="gam", se=TRUE,
              color="black", fill="grey80") +
  # Plot fitted values
  geom_point(data=pred_df, mapping=aes(x=pover, y=meanlog)) +
  # Labels
  labs(x="Proportion above\naction threshold",
       y="meanlog", tag="B") +
  # Theme
  theme_bw()
g2

# C. log(mean)
g3 <- ggplot(data, aes(x=pover_fit, y=sdlog)) +
  # Plot regression
  geom_smooth(method="gam", se=TRUE,
              color="black", fill="grey80") +
  # Plot observations
  geom_point(color="grey40") +
  # Plot fitted values
  geom_point(data=pred_df, mapping=aes(x=pover, y=sdlog)) +
  # Labels
  labs(x="Proportion above\naction threshold",
       y="sdlog", tag="C") +
  # Theme
  theme_bw()
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
g

# Plot shifting distributions
g4 <- ggplot(pred_df, mapping=aes(x=median_ppm, y=cv, fill=pover)) +
  geom_point(pch=21, size=2) +
  # Labels
  labs(x="Median contamination (ppm)", y="Coefficient of variation (CV)") +
  # Legend
  scale_fill_gradientn(name="Proportion above\naction threshhold",
                       na.value = "white",
                       lim=c(0,1),
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g4

# Shifting distributions
################################################################################

# Build data
dists_sim <- purrr::map_df(1:nrow(pred_df), function(x){

  # Parameters
  prop <- pred_df$pover[x]
  meanlog <- pred_df$meanlog[x]
  sdlog <- pred_df$sdlog[x]

  # Simulate data
  da_vals <- seq(0,100,1)
  dens_vals <- dlnorm(da_vals, meanlog=meanlog, sdlog=sdlog)
  df <- tibble(prop=prop,
               da_ppm=da_vals,
               density=dens_vals)


})

# Plot data
g5 <- ggplot(dists_sim, aes(x=da_ppm, y=density, color=prop, group=prop)) +
  geom_line(alpha=0.5) +
  # Reference line
  geom_vline(xintercept=30) +
  # Labels
  labs(x="Contamination (ppm)", y="Density") +
  lims(y=c(0, 0.1)) +
  # Legend
  scale_color_gradientn(name="Proportion above\naction threshhold",
                       na.value = "white",
                       lim=c(0,1),
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")
g5





