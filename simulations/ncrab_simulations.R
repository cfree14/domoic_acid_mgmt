
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories


# Run simulations
################################################################################

# Build results container
results <- expand.grid(mean_ppm=seq(10, 60, 10),
                       sd_ppm=seq(5, 30, 5),
                       ncrabs=seq(6, 36, 6),
                       niter=1:100) %>%
  # Arrange
  arrange(mean_ppm, sd_ppm, ncrabs, niter) %>%
  # Add columns
  mutate(pover_true=NA,
         pover_obs=NA)

# Loop through result
i <- 1
for(i in 1:nrow(results)){

  # Parameters
  thresh_ppm <- 30
  mu <- results$mean_ppm[i]
  sd <- results$sd_ppm[i]
  ncrabs <- results$sd_ppm[i]

  # Derive true proportion over threshold
  pover_true <- pnorm(q=thresh_ppm, mean=mu, sd=sd)
  results$pover_true[i] <- 1 - pover_true

  # Conduct sampling
  samples <- rnorm(n=ncrabs, mean=mu, sd=sd)

  # Record sampling results
  nover_obs <- sum(samples >= thresh_ppm)
  pover_obs <- nover_obs / ncrabs
  results$pover_obs[i] <- pover_obs

  # Plot results
  if(F){
    x <- 1:100
    y <- dnorm(x=x, mean=mu, sd=sd)
    plot(y ~ x, type="l", main=paste0("Mean=", mu, " ; SD=", sd))
    abline(v=thresh_ppm, lty=3)
    points(x=samples, y=rep(0, length(samples)), pch=16)
  }

}

# Format results
################################################################################

# Format results
stats <- results %>%
  # Mutate
  mutate(action_true=ifelse(pover_true>=0.5, "close", "open"),
         action_obs=ifelse(pover_obs>=0.5, "close", "open"),
         obs_correct=ifelse(action_true==action_obs, "correct", "incorrect")) %>%
  # Add actions
  group_by(mean_ppm, sd_ppm, ncrabs) %>%
  summarize(n=n(),
            ncorrect=sum(obs_correct=="correct")) %>%
  ungroup() %>%
  mutate(pcorrect=ncorrect/n)


# Plot data
################################################################################

# Plot data
g <- ggplot(stats, aes(x=mean_ppm, y=sd_ppm, fill=pcorrect)) +
  facet_wrap(~ncrabs) +
  geom_raster() +
  # Labels
  labs(x="Mean contaminatin", y="Variability in contamination") +
  # Legend
  scale_fill_gradientn(name="% correct",
                       colors=RColorBrewer::brewer.pal(9, "Blues")[3:9]) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g

g <- ggplot(results, aes(x=pover_true, y=pover_obs, color=ncrabs)) +
  geom_point()
g


