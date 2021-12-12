

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



# Format observations
################################################################################

# Breaks
mu_breaks <- seq(0, 100, 5)
cv_breaks <- seq(0, 4.5, 0.25)

# Modify survey results for plotting
surveys <- surveys_orig %>%
  # Rename to match simulation reuslts
  rename(median_ppm=da_ppm_med_fit, cv_ppm=cv_fit) %>%
  # Classify within bins
  mutate(mu_bin=cut(median_ppm, breaks=mu_breaks),
         cv_bin=cut(cv_ppm, breaks=cv_breaks)) %>%
  # Number of surveys in each category
  group_by(mu_bin, cv_bin) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  # Percentage of surveys in each category
  mutate(perc=n/sum(n)) %>%
  # Extract limits
  mutate(cv=as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", cv_bin)),
         mu=as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", mu_bin)))


# Format simulation results
################################################################################

# Define action threshold
thresh <- 1/6
thresh_ppm <- 30

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
  select(median_ppm:ncrabs, pcorrect, p_opened_incorr, p_closed_incorr) %>%
  gather(key="metric", value="probability", 6:ncol(.)) %>%
  mutate(metric=recode_factor(metric,
                              "pcorrect"="Probability of\nclosing/opening correctly",
                              "p_closed_incorr"="Risk of closing\nunnecessarily",
                              "p_opened_incorr"="Risk of opening\nriskily")) %>%
  # Format crabs
  mutate(ncrabs_label=paste(ncrabs, "crabs"),
         ncrabs_label=factor(ncrabs_label, levels=paste(seq(6, 36, 6), "crabs"))) %>%
  # Add percent over threshold
  mutate(pover=1 - plnorm(q=thresh_ppm, meanlog=meanlog, sd=sdlog),
         pover_centered=pover-thresh,
         pover_catg=cut(pover, breaks=c(0, 0.1, 0.2, 1), labels=c("<10%", "10-20%", ">20%"))) %>%
  # Add frequency
  left_join(surveys %>% select(mu, cv, n, perc), by=c("median_ppm"="mu", "cv_ppm"="cv")) %>%
  mutate(n=ifelse(is.na(n), 0, n),
         perc=ifelse(is.na(perc), 0, perc),
         perc_catg=cut(perc, breaks=c(0, 0.00001, 0.01, 0.05, Inf), labels=c("Never", "Rare", "Occasional", "Common"), right=F),
         perc_catg=factor(perc_catg, levels=c("Never", "Rare", "Occasional", "Common"))) %>%
  # Remove 36 crab scenario
  filter(ncrabs!=36)

freeR::complete(data)
sort(unique(data$perc_catg))

hist(surveys$perc, breaks=seq(0, 0.15, 0.01))

# Info plot
################################################################################

# Grid
grid <- data %>%
  select(median_ppm, cv_ppm, pover, pover_catg, perc) %>%
  unique()

g1 <- ggplot(grid, aes(x=median_ppm, y=cv_ppm, fill=pover_catg)) +
  geom_tile() +
  theme_bw()
g1

g2 <- ggplot(grid, aes(x=median_ppm, y=cv_ppm, fill=perc)) +
  geom_tile() +
  theme_bw()
g2

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
                  # Legend
                  legend.key.size = unit(0.3, "cm"))

# Plot data
g <- ggplot(data, aes(x=ncrabs, y=probability, group=ncrabs)) +
  facet_wrap(~metric) +
  # Plot jittered points
  geom_jitter(mapping=aes(fill=pover_catg, size=perc_catg), pch=21, stroke=0.2, alpha=0.7, width=1.2) +
  # Plot boxplot
  geom_boxplot(fill='grey95', color="black", lwd=0.4, outlier.shape=NA, alpha=0.4) +
  # Axes
  scale_x_continuous(breaks=seq(6,30, 6)) +
  scale_y_continuous(labels = scales::percent) +
  # Labels
  labs(x="Number of crabs", y="Probability") +
  # Legend
  scale_fill_manual(name="Percent above\naction threshold", values=c("yellow", "orange", "darkred")) +
  scale_size_manual(name="Historical\nfrequency", values=c(0.5, 1.5, 3, 5)) +
  guides(fill = guide_legend(order = 1), size = guide_legend(order = 2)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig6_power_analysis_boxplot.png"),
       width=6.5, height=2.5, units="in", dpi=600)


