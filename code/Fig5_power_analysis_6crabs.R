

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

# Inspect percent frequency
hist(surveys$perc, breaks=seq(0, 0.15, 0.01))


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
         p_incorr=1-pcorrect,
         p_opened_incorr=n_opened_incorr/n,
         p_closed_incorr=n_closed_incorr/n) %>%
  # Simplify and reshape
  select(median_ppm:ncrabs, p_incorr, p_opened_incorr, p_closed_incorr) %>%
  gather(key="metric", value="probability", 6:ncol(.)) %>%
  mutate(metric=recode_factor(metric,
                              "p_incorr"="Probability of\nclosing/opening incorrectly",
                              # "pcorrect"="Probability of\nclosing/opening correctly",
                              "p_closed_incorr"="Probability of closing\nunnecessarily",
                              "p_opened_incorr"="Probability of opening\nriskily")) %>%
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

# Inspect
freeR::complete(data)
sort(unique(data$perc_catg))

# Build parameter key
param_key <- data %>%
  select(median_ppm, cv_ppm, pover, pover_catg) %>%
  unique()

# Reduce to six crabs
data_6crabs <- data %>%
  filter(ncrabs==6)


# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=7),
                  legend.text=element_text(size=5),
                  legend.title=element_text(size=6),
                  strip.text=element_text(size=6, margin = margin(0.1,0.1,0.1,0.1, "cm")),
                  plot.tag =element_text(size=8),
                  plot.tag.position = c(0, 0.99),
                  plot.title = element_blank(),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.key.size = unit(0.3, "cm"),
                  legend.position = "bottom")

# Plot data
g1 <- ggplot(param_key, aes(x=median_ppm, y=cv_ppm, fill=pover, z=pover)) +
  geom_tile(color="grey30", lwd=0.05) +
  # Plot contours
  geom_contour(breaks=c(0.1, 0.2), color="black") +
  # Plot unlikely
  geom_segment(x=40, xend=100, y=1.2, yend=1.2, linetype="solid", color="grey70", lwd=0.2) + # horizontal
  geom_segment(x=0, xend=40, y=6, yend=1.2, linetype="solid", color="grey70", lwd=0.2) + # diagonal (1.2-6)/40 = -0.12
  # Plot survey points
  geom_point(data=surveys_orig, mapping=aes(x=da_ppm_med_fit, y=cv_fit), color="black",
             inherit.aes = F, pch=1, alpha=0.4, size=0.8, stroke=0.2) +
  # Lines
  geom_vline(xintercept = 30, linetype="dotted", lwd=0.2) +
  # Labels
  labs(x="Median contamination (ppm)", y="Coefficient of variation (CV)", tag="A") +
  scale_x_continuous(breaks=seq(0,90,15)) +
  # Legend
  scale_fill_gradientn(name="Percent above\naction threshold",
                       lim=c(0,1),
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position="top")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.7,0.8),
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.key.size = unit(0.3, "cm"))
g1

# Plot probability boxplots
colors <- RColorBrewer::brewer.pal(9, "YlOrRd")[c(1,5,9)]
g2 <- ggplot(data_6crabs, aes(x=probability, y=ncrabs)) +
  facet_wrap(~metric) +
  # Plot jittered points
  geom_jitter(mapping=aes(fill=pover_catg, size=perc_catg), pch=21, stroke=0.2, alpha=0.7) +
  # Plot boxplot
  geom_boxplot(fill='grey95', color="black", lwd=0.4, outlier.shape=NA, alpha=0.4) +
  # Axes
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(breaks=6) +
  # Labels
  labs(x="Probability", y="", tag="B") +
  # Legend
  scale_fill_manual(name="% above action threshold", values=colors) +
  scale_size_manual(name="Historical frequency", values=c(0.5, 1.5, 3, 4)) +
  guides(fill = guide_legend(order = 1, title.position="top"), size = guide_legend(order = 2, title.position="top")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top",
        axis.text.y=element_text(color="white"),
        axis.title.x=element_blank(),
        # axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(rep(-5, 4)))
g2


# Plot probability raster
g3 <- ggplot(data_6crabs, aes(x=median_ppm, y=cv_ppm, fill=probability, z=pover)) +
  facet_wrap(~metric, nrow=1) +
  # Plot raster
  geom_tile(color="grey30", lwd=0.05) +
  # Plot contours
  geom_contour(breaks=c(0.1, 0.2), color="black") +
  # Plot unlikely
  geom_segment(x=40, xend=100, y=1.2, yend=1.2, linetype="solid", color="grey70", lwd=0.2) + # horizontal
  geom_segment(x=0, xend=40, y=6, yend=1.2, linetype="solid", color="grey70", lwd=0.2) + # diagonal (1.2-6)/40 = -0.12
  # Lines
  geom_vline(xintercept = 30, linetype="dotted", lwd=0.2) +
  # Labels
  labs(x="Median contamination (ppm)", y="Coefficient of variation (CV)", tag="C") +
  scale_x_continuous(breaks=seq(0,90,15)) +
  # Legend
  scale_fill_gradientn(name="Probability",
                       lim=c(0,1),
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position="top")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.9,0.7),
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.key.size = unit(0.3, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_blank())
g3

# Merge plots
layout_matrix <- matrix(data=c(1,2,
                               1,3), byrow=T, ncol=2)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.3, 0.7), heights=c(0.4, 0.6))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig5_power_analysis_6crabs.png"),
       width=6.5, height=3, units="in", dpi=600)



