
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"
outputdir <- "simulations/output"

# Run simulations
################################################################################

# Number of iterations
niter <- 1000

# Parameters
medians <- seq(5,100,5)
cvs <- seq(0.25, 4.5, 0.25)
ncrabs <- seq(6, 36, 6)

# Build results container
results <- expand.grid(median_ppm=medians,
                       cv_ppm=cvs,
                       ncrabs=ncrabs,
                       niter=1:niter) %>%
  # Calculate meanlog and sdlog
  mutate(meanlog=log(median_ppm),
         sdlog=sqrt(log(cv_ppm^2+1))) %>%
  # Arrange
  arrange(median_ppm, cv_ppm, meanlog, sdlog, ncrabs, niter) %>%
  # Add columns
  mutate(pover_true=NA,
         pover_obs=NA) %>%
  # Remove unlikely rows
  filter(!(cv_ppm > 1.2 & median_ppm >= 40) & !(median_ppm < 40 & cv_ppm > (6-median_ppm*0.12 )))

# Loop through result
i <- 1
tictoc::tic()
for(i in 1:nrow(results)){

  # Parameters
  thresh_ppm <- 30
  meanlog <- results$meanlog[i]
  sdlog <- results$sdlog[i]
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
    x <- seq(1,100,1)
    y <- dlnorm(x=x, meanlog=meanlog, sd=sdlog)
    plot(y ~ x, type="l", main=paste0("mu=", meanlog, " ; sigma=", sdlog))
    abline(v=thresh_ppm, lty=3)
    points(x=samples, y=rep(0, length(samples)), pch=16)
  }

}
tictoc::toc()

# Export results
saveRDS(results, file=file.path(outputdir, "ncrab_simulation_output.Rds"))


# Analyze simulation
################################################################################

# Format results
stats <- results %>%
  # What should have been done, what was done, and was this correct?
  mutate(action_true=ifelse(pover_true>=0.5, "close", "open"),
         action_obs=ifelse(pover_obs>=0.5, "close", "open"),
         obs_correct=ifelse(action_true==action_obs, "correct", "incorrect")) %>%
  # Classify actions
  mutate(action_type=paste(action_true, action_obs, sep="-"),
         action_type=recode(action_type,
                            "close-close"="Closed correctly",
                            "close-open"="Opened recklessly",
                            "open-close"="Closed unnecessarily",
                            "open-open"="Opened correctly")) %>%
  # Quantify frequency of actions
  group_by(median_ppm, cv_ppm, meanlog, sdlog, ncrabs) %>%
  summarize(n=n(),
            ncorrect=sum(obs_correct=="correct"),
            n_opened_incorr=sum(action_type=="Opened recklessly"),
            n_closed_incorr=sum(action_type=="Closed unnecessarily")) %>%
  ungroup() %>%
  mutate(pcorrect=ncorrect/n,
         p_opened_incorr=n_opened_incorr/n,
         p_closed_incorr=n_closed_incorr/n)


# Visualize results
################################################################################

# General performance
g1 <- ggplot(stats, aes(x=median_ppm, y=cv_ppm, fill=1-pcorrect)) +
  facet_wrap(~ncrabs) +
  geom_raster() +
  # Lines
  geom_vline(xintercept = 30, linetype="dotted") +
  # Labels
  labs(x="Median contamination", y="Variability in contamination", title="Risk of an incorrect decision") +
  # Legend
  scale_fill_gradientn(name="Probability",
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g1


# Risk of opening recklessly
g2 <- ggplot(stats, aes(x=median_ppm, y=cv_ppm, fill=p_opened_incorr)) +
  facet_wrap(~ncrabs) +
  geom_raster() +
  # Lines
  geom_vline(xintercept = 30, linetype="dotted") +
  # Labels
  labs(x="Median contamination", y="Variability in contamination", title="Risk of opening recklessly") +
  # Legend
  scale_fill_gradientn(name="Probability",
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g2

# Risk of closing unnecessarily
g3 <- ggplot(stats, aes(x=median_ppm, y=cv_ppm, fill=p_closed_incorr)) +
  facet_wrap(~ncrabs) +
  geom_raster() +
  # Lines
  geom_vline(xintercept = 30, linetype="dotted") +
  # Labels
  labs(x="Median contamination", y="Variability in contamination", title="Risk of closing unnecessarily") +
  # Legend
  scale_fill_gradientn(name="Probability",
                       colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g3



