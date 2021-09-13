

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


#  Build procedure for estimaing mu/sigma given a p(over)
################################################################################

# Predictions
da_ppm_thresh <- 30
povers <- seq(0,1, 0.01)
gamfit <- mgcv::gam(sdlog ~ s(pover_fit, bs = "cs"), data=data)
sigmas <- predict(gamfit, newdata = tibble(pover_fit=povers))

# Function to calculate mu given p(over) and sigma
calc_mu <- function(pover, sigma){
  mu <- log(da_ppm_thresh) - sigma*sqrt(2)*pracma::erfinv(1-2*pover)
  return(mu)
}

# Build reference data frame
pred_df <- tibble(pover=povers,
                  sigma=sigmas) %>%
  # Derive mu
  mutate(mu=calc_mu(pover, sigma)) %>%
  # Calculate % over threshhold to check
  mutate(pover_check=1-plnorm(da_ppm_thresh, meanlog=mu, sdlog=sigma)) %>%
  # Calculate median/CV of distribution
  mutate(median_ppm=exp(mu),
         cv=sqrt(exp(sigma^2)-1))


# Simulate shifting distributions
dists_sim <- purrr::map_df(1:nrow(pred_df), function(x){

  # Parameters
  prop <- pred_df$pover[x]
  mu <- pred_df$mu[x]
  sigma <- pred_df$sigma[x]

  # Simulate data
  da_vals <- seq(0,100,1)
  dens_vals <- dlnorm(da_vals, meanlog=mu, sdlog=sigma)
  df <- tibble(prop=prop,
               da_ppm=da_vals,
               density=dens_vals)


})


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# A. Sigma
g1 <- ggplot(data, aes(x=pover_fit, y=sdlog)) +
  # Plot regression
  geom_smooth(method="gam", se=TRUE,
              color="black", fill="grey80") +
  # Plot observations
  geom_point(color="grey40") +
  # Plot fitted values
  geom_point(data=pred_df, mapping=aes(x=pover, y=sigma)) +
  # Labels
  labs(x="Proportion above\naction threshold",
       y="sdlog", tag="A") +
  # Theme
  theme_bw() + my_theme
g1

# B. Mu
g2 <- ggplot(data %>% filter(!is.na(meanlog)), aes(x=pover_fit, y=meanlog)) +
  # Plot observations
  geom_point(color="grey40") +
  # Plot regression
  geom_smooth(method="gam", se=TRUE,
              color="black", fill="grey80") +
  # Plot fitted values
  geom_point(data=pred_df, mapping=aes(x=pover, y=mu)) +
  # Labels
  labs(x="Proportion above\naction threshold",
       y="meanlog", tag="B") +
  # Theme
  theme_bw() + my_theme
g2

# Plot shifting distributions
g3 <- ggplot(pred_df %>% filter(is.finite(mu)), mapping=aes(x=median_ppm, y=cv, fill=pover)) +
  geom_point(pch=21, size=2) +
  # Labels
  labs(x="Median contamination (ppm)", y="Coefficient of variation (CV)", tag="C") +
  # Limits
  scale_x_continuous(lim=c(0,90), breaks=seq(0,90,15)) +
  scale_y_continuous(lim=c(0,4)) +
  # Legend
  scale_fill_gradientn(name="Proportion above\naction threshhold",
                       na.value = "white",
                       lim=c(0,1),
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.7))
g3

# D. Mean
g4 <- ggplot(data, aes(x=pover_fit, y=da_ppm_med_fit)) +
  # Plot regression
  geom_smooth(method="gam", se=TRUE,
              color="black", fill="grey80") +
  # Plot observations
  geom_point(color="grey40") +
  # Plot fitted values
  geom_point(data=pred_df, mapping=aes(x=pover, y=median_ppm)) +
  # Labels
  labs(x="Proportion above\naction threshold",
       y="Median contamination (ppm)", tag="D") +
  # Theme
  theme_bw() + my_theme
g4


# #. Shifting distributions
g5 <- ggplot(dists_sim, aes(x=da_ppm, y=density, color=prop, group=prop)) +
  geom_line(alpha=0.5) +
  # Reference line
  geom_vline(xintercept=30) +
  # Labels
  labs(x="Contamination (ppm)", y="Density", tag="E") +
  lims(y=c(0, 0.1)) +
  # Legend
  scale_color_gradientn(name="Proportion above\naction threshhold",
                        na.value = "white",
                        lim=c(0,1),
                        colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.7))
g5


# Merge
layout_matrix <- matrix(data=c(1,2,3,
                               4,5,5), nrow=2, byrow = T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, layout_matrix=layout_matrix)
g










