

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
data_orig <- readRDS(file=file.path(datadir, "CA_OR_WA_da_sampling_data.Rds"))


# Build data
################################################################################

# Data
data <- data_orig %>%
  # Dungeness crab
  filter(comm_name=="Dungeness crab" & tissue=="viscera") %>%
  # Build survey id
  mutate(survey_id=paste(date, location, sep="-")) %>%
  # Recode
  mutate(da_ppm=ifelse(da_ppm==0, 1, da_ppm))

# Survey key
survey_results <- data %>%
  # Summarize survey results
  group_by(year, date, location, survey_id) %>%
  summarize(nsamples=n(),
            nover=sum(da_ppm>=30),
            pover=nover/nsamples,
            da_ppm_avg=mean(da_ppm)) %>%
  ungroup() %>%
  # Reduce to full surveys
  filter(nsamples>=6) %>%
  # Add blank columns
  mutate(meanlog=NA,
         sdlog=NA)

# Loop through distributions and fit log-normal distribution
for(i in 1:nrow(survey_results)){

  # Subset data
  surveyid <- survey_results$survey_id[i]
  sdata <- data %>%
    filter(survey_id==surveyid)

  # Fit log-normal distribution
  lnfit <- try(fitdist(sdata$da_ppm, distr="lnorm"))
  if(!inherits(lnfit, "try-error")){
    survey_results$meanlog[i] <- lnfit$estimate["meanlog"]
    survey_results$sdlog[i] <- lnfit$estimate["sdlog"]
  }


}



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
key <- matrix(c("B", 2, 1.5,
                    "C", 3, 1.0,
                    "D", 4, 0.5,
                    "E", 2, 0.5,
                    "F", 0.5, 0.5), ncol=3, byrow=T, dimnames = list(NULL, c("plot", "mu", "sigma"))) %>%
  as.data.frame() %>%
  mutate(mu=as.numeric(mu),
         sigma=as.numeric(sigma))

# Distribution of parameters
g_big <- ggplot(survey_results, aes(x=meanlog, y=sdlog, fill=pover)) +
  # Plot points
  geom_point(pch=21, size=3, alpha=0.8) +
  # Plot call outs
  geom_text(data=key, mapping=aes(x=mu, y=sigma, label=plot), size=6, fontface="bold", inherit.aes = F) +
  # Labels
  labs(x="μ", y='σ', tag="A") +
  scale_fill_gradientn(name="% over limit", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + big_theme +
  theme(legend.position = c(0.83, 0.8),
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g_big

# Plot example
plot_example <- function(meanlog, sdlog, tag){
  x <- seq(0,60,0.1)
  y <- dlnorm(x=x, meanlog=meanlog, sdlog=sdlog)
  g <- ggplot(mapping=aes(x=x, y=y)) +
    geom_line() +
    # Labels
    labs(x="Domoic acid (ppm)", y="Density", tag=tag) +
    # Lines
    geom_vline(xintercept = 30, linetype="dotted") +
    # Theme
    theme_bw() + small_theme
  print(g)
  return(g)
}

# Plot examples
g1 <- plot_example(meanlog=key$mu[1], sdlog=key$sigma[1], tag=key$plot[1])
g2 <- plot_example(meanlog=key$mu[2], sdlog=key$sigma[2], tag=key$plot[2])
g3 <- plot_example(meanlog=key$mu[3], sdlog=key$sigma[3], tag=key$plot[3])
g4 <- plot_example(meanlog=key$mu[4], sdlog=key$sigma[4], tag=key$plot[4])
g5 <- plot_example(meanlog=key$mu[5], sdlog=key$sigma[5], tag=key$plot[5])

# Merge plots
layout_matrix <- matrix(data=c(1,1,2,
                               1,1,3,
                               6,5,4), ncol=3, byrow=T)
g <- gridExtra::grid.arrange(g_big, g1, g2, g3, g4, g5,
                             layout_matrix=layout_matrix)
g

# Export plots
ggsave(g, filename=file.path(plotdir, "FigX_survey_dist_results.png"),
       width=6.5, height=6.5, units="in", dpi=600)







