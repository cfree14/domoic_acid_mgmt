

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


# Plot data
################################################################################

# Theme
big_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=12),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))

small_theme <- theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))


# Build key for subplots
key <- matrix(c("B", 10, 2.5,
                "C", 25, 1.0,
                "D", 50, 0.5,
                "E", 25, 0.5,
                "F", 10, 0.5), ncol=3, byrow=T, dimnames = list(NULL, c("plot", "da_ppm_med_fit", "cv_fit"))) %>%
  as.data.frame() %>%
  mutate(da_ppm_med_fit=as.numeric(da_ppm_med_fit),
         cv_fit=as.numeric(cv_fit))

# Number of surveys
nsurveys <- nrow(data)
nsurveys_label <-paste(nsurveys, "surveys")

# Distribution of parameters
g_big <- ggplot(data, aes(x=da_ppm_med_fit, y=cv_fit, fill=pover_obs)) +
  # Plot points
  geom_point(pch=21, size=3, alpha=0.8) +
  # Plot call outs
  geom_text(data=key, mapping=aes(x=da_ppm_med_fit, y=cv_fit, label=plot), size=6, fontface="bold", inherit.aes = F) +
  # Plot unlikely
  geom_segment(x=40, xend=100, y=1.2, yend=1.2, linetype="solid", color="grey70", lwd=0.5) + # horizontal
  geom_segment(x=0, xend=40, y=6, yend=1.2, linetype="solid", color="grey70", lwd=0.5) + # diagonal (1.2-6)/40 = -0.12
  annotate(geom="text", x=95, y=1.2, hjust=1, vjust=-0.8, label="Values above this line are unlikely", color="grey70", size=2.5) +
  # Plot line
  geom_vline(xintercept = 30, linetype="dotted", color="black") +
  # Add survey label
  annotate(geom="text", x=95, y=0, label=nsurveys_label, hjust=1, vjust=1.5, size=2.5, color='grey70') +
  # Labels
  labs(x="Median contamination (ppm)", y='Coefficient of variation (CV)', tag="A") +
  scale_x_continuous(breaks=seq(0, 90, 15)) +
  # Legends
  scale_fill_gradientn(name="Percent above\naction threshold", lim=c(0,1),
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + big_theme +
  theme(legend.position = c(0.83, 0.8),
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g_big

# Plot example
plot_example <- function(da_ppm_med_fit, cv_fit, tag){

  # Derive meanlog/sdlog
  meanlog <- log(da_ppm_med_fit)
  sdlog <- sqrt(log(cv_fit^2+1))

  # Calculate percent over action threshold
  pover <- (1- plnorm(30, meanlog=meanlog, sdlog=sdlog))*100
  pover_label <- paste0(round(pover,1), "%")

  # Simulate data
  x <- seq(0,100,0.1)
  y <- dlnorm(x=x, meanlog=meanlog, sdlog=sdlog)

  # Plot data
  g <- ggplot(mapping=aes(x=x, y=y)) +
    geom_line() +
    # Labels
    labs(x="Contamination (ppm)", y="Density", tag=tag) +
    scale_x_continuous(breaks=seq(0, 90, 15), lim=c(0, 90)) +
    # Lines
    geom_vline(xintercept = 30, linetype="dotted") +
    # Add label
    annotate(geom="text", x=90, y=max(y), label=pover_label, hjust=1, vjust=1.1, size=2.5) +
    # Theme
    theme_bw() + small_theme
  print(g)
  # Return
  return(g)

}

# Plot examples
g1 <- plot_example(da_ppm_med_fit=key$da_ppm_med_fit[1], cv_fit=key$cv_fit[1], tag=key$plot[1])
g2 <- plot_example(da_ppm_med_fit=key$da_ppm_med_fit[2], cv_fit=key$cv_fit[2], tag=key$plot[2])
g3 <- plot_example(da_ppm_med_fit=key$da_ppm_med_fit[3], cv_fit=key$cv_fit[3], tag=key$plot[3])
g4 <- plot_example(da_ppm_med_fit=key$da_ppm_med_fit[4], cv_fit=key$cv_fit[4], tag=key$plot[4])
g5 <- plot_example(da_ppm_med_fit=key$da_ppm_med_fit[5], cv_fit=key$cv_fit[5], tag=key$plot[5])

# Merge plots
layout_matrix <- matrix(data=c(1,1,2,
                               1,1,3,
                               6,5,4), ncol=3, byrow=T)
g <- gridExtra::grid.arrange(g_big, g1, g2, g3, g4, g5,
                             layout_matrix=layout_matrix)
g

# Export plots
ggsave(g, filename=file.path(plotdir, "Fig6_survey_dist_data.png"),
       width=6.5, height=6.5, units="in", dpi=600)







