

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
outputdir <- "simulations/output"
plotdir <- "figures"

# Read simulation results
data_orig <- readRDS(file=file.path(outputdir, "ncrab_simulation_output.Rds"))

# Read survey results
surveys_orig <- readRDS(file=file.path(datadir, "CA_OR_WA_da_survey_results.Rds"))

# Number of combos
combo_key <- data_orig %>%
  select(median_ppm, cv_ppm) %>%
  unique()


# Format simulation results
################################################################################

# Define action threshold
thresh <- 1/6

# Format results
data <- data_orig %>%
  # What should have been done, what was done, and was this correct?
  mutate(action_true=ifelse(pover_true>=thresh, "close", "open"),
         action_obs=ifelse(pover_obs>=thresh, "close", "open"),
         obs_correct=ifelse(action_true==action_obs, "correct", "incorrect")) %>%
  # Classify actions
  mutate(action_type=paste(action_true, action_obs, sep="-"),
         action_type=recode(action_type,
                            "close-close"="Closed correctly",
                            "close-open"="Opened riskily",
                            "open-close"="Closed unnecessarily",
                            "open-open"="Opened correctly")) %>%
  # Quantify frequency of actions
  group_by(median_ppm, cv_ppm, meanlog, sdlog, ncrabs) %>%
  summarize(n=n(),
            ncorrect=sum(obs_correct=="correct"),
            n_opened_incorr=sum(action_type=="Opened riskily"),
            n_closed_incorr=sum(action_type=="Closed unnecessarily")) %>%
  ungroup() %>%
  mutate(pcorrect=ncorrect/n,
         p_opened_incorr=n_opened_incorr/n,
         p_closed_incorr=n_closed_incorr/n) %>%
  # Simplify and reshape
  select(median_ppm:ncrabs, pcorrect, p_opened_incorr, p_closed_incorr) %>%
  gather(key="metric", value="probability", 6:ncol(.)) %>%
  mutate(metric=recode_factor(metric,
                              "pcorrect"="Probability of\nclosing/opening correctly",
                              "p_closed_incorr"="Probability of\nclosing unnecessarily",
                              "p_opened_incorr"="Probability of\nopening riskily")) %>%
  # Format crabs
  mutate(ncrabs_label=paste(ncrabs, "crabs"),
         ncrabs_label=factor(ncrabs_label, levels=paste(seq(6, 36, 6), "crabs"))) %>%
  # Remove 36 crab scenario
  filter(ncrabs!=36)


# Format survey results
################################################################################

# Breaks
mu_breaks <- seq(0, 100, 5)
cv_breaks <- seq(0, 4.5, 0.25)

# Modify survey results for plotting
surveys <- surveys_orig %>%
  # Rename to match simulation reuslts
  rename(median_ppm=ln_median, cv_ppm=ln_cv) %>%
  # Classify within bins
  mutate(mu_bin=cut(median_ppm, breaks=mu_breaks),
         cv_bin=cut(cv_ppm, breaks=cv_breaks))

# Calculate number of decisions within bin
bin_n <- surveys %>%
  # Number of surveys in each category
  group_by(mu_bin, cv_bin) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  # Reduce to surveys with required data
  filter(!is.na(mu_bin)) %>%
  # Extract limits
  mutate(cv=as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", cv_bin)),
         mu=as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", mu_bin)))


# Build final dataset
################################################################################

# Stats for manuscript
stats <- data %>%
  # Add weights
  left_join(bin_n %>% select(cv, mu, n), by=c("cv_ppm"="cv", "median_ppm"="mu")) %>%
  # If no instances in observations, add zeroes
  mutate(n=ifelse(is.na(n), 0, n)) %>%
  # Compute likelihood-weighted stats
  group_by(metric, ncrabs) %>%
  summarize(prob_med=median(probability),
            prob_avg=mean(probability),
            prob_avg_wt=weighted.mean(x=probability, w=n),
            prob_sd_wt=Hmisc::wtd.var(x=probability, w=n) %>% sqrt(),
            prob_lo_wt=reldist::wtd.quantile (x=probability, q=0.05, weight=n),
            prob_hi_wt=reldist::wtd.quantile (x=probability, q=0.95, weight=n)) %>%
  ungroup()



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

# Plot means
g <- ggplot(stats, aes(x=ncrabs, y=prob_avg_wt, color=metric)) +
  facet_wrap(~metric) +
  # Plot data
  geom_ribbon(mapping=aes(x=ncrabs, ymin=prob_lo_wt, ymax=prob_hi_wt, fill=metric), inherit.aes=F, alpha=0.5) +
  geom_line() +
  geom_point() +
  # geom_line(data=theory_gains, mapping=aes(x=ncrabs, y=probability), inherit.aes = F) +
  # Reference line
  # Labels
  labs(x="Number of crabs\nsampled per survey", y="Probability") +
  # Axis
  scale_x_continuous(breaks=seq(6,30,6)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig6_ncrab_simulations_boxplots.png"),
       width=6.5, height=2.75, units="in", dpi=600)


