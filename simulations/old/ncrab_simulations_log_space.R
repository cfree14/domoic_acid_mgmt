
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"

# Run simulations
################################################################################

# # Build results container (based strictly on meanlog)
# results <- expand.grid(meanlog_ppm=seq(0.5, 4.5, 0.5),
#                        sdlog_ppm=seq(0.5, 2, 0.5),
#                        ncrabs=seq(6, 36, 6),
#                        niter=1:100) %>%
#   # Arrange
#   arrange(meanlog_ppm, sdlog_ppm, ncrabs, niter) %>%
#   # Add columns
#   mutate(pover_true=NA,
#          pover_obs=NA)


# Build results container (based strictly on meanlog)
results <- expand.grid(median_ppm=seq(5,100,5),
                       sdlog_ppm=seq(0.5, 2, 0.5),
                       ncrabs=seq(6, 36, 6),
                       niter=1:100) %>%
  # Calculate meanlog and calculate CV
  mutate(meanlog_ppm=log(median_ppm),
         cv_ppm=sqrt(exp(sdlog^2)-1)) %>%
  # Arrange
  arrange(median_ppm, meanlog_ppm, cv_ppm, sdlog_ppm, ncrabs, niter) %>%
  # Add columns
  mutate(pover_true=NA,
         pover_obs=NA)

# Loop through result
i <- 1
for(i in 1:nrow(results)){

  # Parameters
  thresh_ppm <- 30
  meanlog <- results$meanlog_ppm[i]
  sdlog <- results$sdlog_ppm[i]
  ncrabs <- results$ncrabs[i]

  # Derive true proportion over threshold
  pover_true <- 1 - plnorm(q=thresh_ppm, meanlog=meanlog, sd=sdlog)
  results$pover_true[i] <- pover_true

  # Conduct sampling
  samples <- rlnorm(n=ncrabs, meanlog=meanlog, sd=sdlog)

  # Record sampling results
  nover_obs <- sum(samples >= thresh_ppm)
  pover_obs <- nover_obs / ncrabs
  results$pover_obs[i] <- pover_obs

  # Plot results
  if(F){
    x <- 1:100
    y <- dlnorm(x=x, meanlog=meanlog, sd=sdlog)
    plot(y ~ x, type="l", main=paste0("mu=", meanlog, " ; sigma=", sdlog))
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
  group_by(median_ppm, meanlog_ppm, sdlog_ppm, cv_ppm, ncrabs) %>%
  summarize(n=n(),
            ncorrect=sum(obs_correct=="correct")) %>%
  ungroup() %>%
  mutate(pcorrect=ncorrect/n)


# Plot data
################################################################################


# Plot data
g <- ggplot(stats, aes(x=median_ppm, y=cv_ppm, fill=pcorrect)) +
  facet_wrap(~ncrabs) +
  geom_raster() +
  # Lines
  geom_vline(xintercept = 30, linetype="dotted") +
  # Labels
  labs(x="Median contamination", y="Variability in contamination") +
  # Legend
  scale_fill_gradientn(name="% correct",
                       colors=RColorBrewer::brewer.pal(9, "RdBu")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g

g <- ggplot(results, aes(x=pover_true, y=pover_obs, color=ncrabs)) +
  geom_point()
g


