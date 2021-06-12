

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)
library(fitdistrplus)

# Directories
outputdir <- "simulations/output"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(outputdir, "ncrab_simulation_output.Rds"))


# Build data
################################################################################

# Format results
data <- data_orig %>%
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
         p_closed_incorr=n_closed_incorr/n) %>%
  # Simplify and reshape
  select(median_ppm:ncrabs, p_opened_incorr, p_closed_incorr) %>%
  gather(key="metric", value="probability", 6:7) %>%
  mutate(metric=recode_factor(metric,
                              "p_closed_incorr"="Risk of delaying\nunnecessarily",
                              "p_opened_incorr"="Risk of opening\nrecklessly")) %>%
  # Format crabs
  mutate(ncrabs_label=paste(ncrabs, "crabs"),
         ncrabs_label=factor(ncrabs_label, levels=paste(seq(6, 36, 6), "crabs"))) %>%
  # Remove 36 crab scenario
  filter(ncrabs!=36) %>%
  # Remove >60 ppm (b/c easy)
  filter(median_ppm<=60)



# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=7),
                  strip.text=element_text(size=7),
                  plot.title=element_text(size=10),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  legend.key.size = unit(0.5, "cm"))

# Plot data
g <- ggplot(data, aes(x=median_ppm, y=cv_ppm, fill=probability)) +
  facet_grid(metric~ncrabs_label) +
  geom_raster() +
  # Lines
  geom_vline(xintercept = 30, linetype="dotted", lwd=0.2) +
  # Labels
  labs(x="Median contamination (ppm)", y="Coefficient of variation (CV)") +
  scale_x_continuous(breaks=seq(0,90,15)) +
  # Legend
  scale_fill_gradientn(name="Probability",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")[2:9]) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig6_ncrab_simulations.png"),
       width=6.5, height=2.75, units="in", dpi=600)

