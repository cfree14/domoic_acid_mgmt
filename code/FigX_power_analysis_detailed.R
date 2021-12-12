

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
  select(median_ppm:ncrabs, p_opened_incorr, p_closed_incorr) %>%
  gather(key="metric", value="probability", 6:7) %>%
  mutate(metric=recode_factor(metric,
                              "p_closed_incorr"="Probability of\nclosing unnecessarily",
                              "p_opened_incorr"="Probability of\nopening riskily")) %>%
  # Format crabs
  mutate(ncrabs_label=paste(ncrabs, "crabs"),
         ncrabs_label=factor(ncrabs_label, levels=paste(seq(6, 36, 6), "crabs"))) %>%
  # Remove 36 crab scenario
  filter(ncrabs!=36) %>%
  # Remove >60 ppm (b/c easy)
  filter(median_ppm<=60)


# Format survey results
################################################################################

# Breaks
mu_breaks <- seq(0, 100, 5)
cv_breaks <- seq(0, 4.5, 0.25)

# Modify survey results for plotting
surveys <- surveys_orig %>%
  # Rename to match simulation reuslts
  rename(median_ppm=da_ppm_med_fit, cv_ppm=cv_fit) %>%
  # Add metric and ncrabs to place in one corner of plot
  mutate(ncrabs_label=factor("30 crabs", levels=levels(data$ncrabs_label)),
         metric=factor("Probability of\nopening riskily", levels=levels(data$metric))) %>%
  # Classify within bins
  mutate(mu_bin=cut(median_ppm, breaks=mu_breaks),
         cv_bin=cut(cv_ppm, breaks=cv_breaks)) %>%
  # Mark as inside "risky" zone
  mutate(risky=ifelse(median_ppm>=15 & median_ppm<=30, "lower",
                      ifelse(median_ppm>30 & median_ppm<=45, "upper", "none")))


# Calculate number of decisions within bin
bin_n <- surveys %>%
  # Number of surveys in each category
  group_by(risky, mu_bin, cv_bin) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  filter(!is.na(risky))

# Calculate p(wrong decision) within bin
bin_p <- data %>%
  # Simplify
  select(ncrabs_label, ncrabs, median_ppm, cv_ppm, metric, probability) %>%
  # Add bin
  mutate(mu_bin=cut(median_ppm, breaks=mu_breaks),
         cv_bin=cut(cv_ppm, breaks=cv_breaks))

# Merge to calculate proportion of wrong decisions historically
bin_wrong_hist <- bin_n %>%
  # Add p(wrong decision)
  left_join(bin_p, by=c("mu_bin", "cv_bin")) %>%
  # Calculate number of expected wrong decisions
  mutate(nwrong_exp=probability * n) %>%
  # Summarize by scenario
  group_by(ncrabs_label, ncrabs, metric, risky) %>%
  summarize(n=sum(n),
            nwrong_exp=sum(nwrong_exp)) %>%
  ungroup() %>%
  # Calculate p(wrong)
  mutate(pwrong_exp=nwrong_exp/n,
         pwrong_exp_label=paste0(round(pwrong_exp*100,0), "%")) %>%
  # Reduce to scenarios of interest
  filter(risky!="none") %>%
  filter( (metric=="Probability of\nclosing unnecessarily" & risky=="lower") | (metric == "Probability of\nopening riskily" & risky=="upper")) %>%
  # Arrange
  arrange(metric, ncrabs_label)



# Plot data
################################################################################

# Challenging zones
challenge_zones <-tibble(metric=factor(x=c("Risk of closing\nunnecessarily", "Risk of opening\nriskily")),
                         x1=c(15,30),
                         x2=c(30,45))

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
  geom_tile(color="grey30", lwd=0.05) +
  # Plot unlikely
  geom_segment(x=40, xend=100, y=1.2, yend=1.2, linetype="solid", color="grey70", lwd=0.2) + # horizontal
  geom_segment(x=0, xend=40, y=6, yend=1.2, linetype="solid", color="grey70", lwd=0.2) + # diagonal (1.2-6)/40 = -0.12
  # annotate(geom="text", x=95, y=1.2, hjust=1, vjust=-0.8, label="Values above this line are unlikely", color="grey70", size=2.5) +
  # Plot survey points
  geom_point(data=surveys, mapping=aes(x=median_ppm, y=cv_ppm), color="grey40",
             inherit.aes = F, pch=1, alpha=0.6, size=0.8, stroke=0.2) +
  # Add percent expected wrong
  # geom_text(data=bin_wrong_hist, mapping=aes(x=60, y=4.5, label=pwrong_exp_label), hjust=1, size=2, inherit.aes = F) +
  # Plot challenging zones
  # geom_segment(data=challenge_zones, mapping=aes(x=x1, xend=x2, y=0.1, yend=0.1), inherit.aes = F) +
  # Lines
  geom_vline(xintercept = 30, linetype="dotted", lwd=0.2) +
  # Labels
  labs(x="Median contamination (ppm)", y="Coefficient of variation (CV)") +
  scale_x_continuous(breaks=seq(0,90,15), lim=c(0,60)) +
  # Legend
  # scale_color_manual(name="Risky", values=c("grey80", "black"), guide=F) +
  scale_fill_gradientn(name="Probability",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")[2:9],
                       labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_power_analysis_detailed.png"),
       width=6.5, height=2.75, units="in", dpi=600)

