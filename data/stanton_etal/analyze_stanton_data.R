
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/stanton_etal"

# Read data
data <- read.csv(file.path(datadir, "Stanton_etal_2018_data.csv"), as.is=T)


# Fit models
################################################################################

# Fit exponential regression: y = a*exp(x*b)
expfit <- lm(log(meat_ppm) ~ viscera_ppm, data)

# Fit polynomial regression: y = a*exp(x*b)
polyfit <- lm(meat_ppm ~ viscera_ppm + I(viscera_ppm^2), data)

# Inspect fit
summary(expfit)
summary(polyfit)

# Param
x <- 0:150

# Build exponential regression line and confidence interval
exppred <- predict(expfit, newdata = tibble(viscera_ppm=x), interval = 'confidence') %>% exp() %>%
  as.data.frame() %>%
  rename(meat_ppm=fit,
         meat_ppm_lo=lwr,
         meat_ppm_hi=upr) %>%
  mutate(model="Exponential",
         viscera_ppm=x) %>%
  select(model, viscera_ppm, everything())

# Build poynomial regression line and confidence interval
polypred <- predict(polyfit, newdata = tibble(viscera_ppm=x), interval = 'confidence') %>%
  as.data.frame() %>%
  rename(meat_ppm=fit,
         meat_ppm_lo=lwr,
         meat_ppm_hi=upr) %>%
  mutate(model="Polynomial",
         viscera_ppm=x) %>%
  select(model, viscera_ppm, everything())

# Merge
reg <- bind_rows(polypred)


# Plot data
################################################################################

# Plot data
g <- ggplot(data, aes(x=viscera_ppm, y=meat_ppm)) +
  # Regression line
  geom_ribbon(data=reg, mapping=aes(x=viscera_ppm, ymin=meat_ppm_lo, ymax=meat_ppm_hi, fill=model), alpha=0.2) +
  geom_line(data=reg, mapping=aes(x=viscera_ppm, y=meat_ppm, color=model), inherit.aes = F, lwd=1.2) +
  # Reference lines
  geom_hline(yintercept=20, linetype="dotted") +
  geom_vline(xintercept=30, linetype="dotted") +
  # Points
  geom_point(size=2) +
  # Labels
  labs(x="Domoic acid\nin viscera (ppm)", y="Domoic acid\nin meat (ppm)") +
  guides(color=guide_legend(title="Model type"), fill=guide_legend(title="Model type")) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position = "none")
g

# Export
ggsave(g, filename=file.path(datadir, "meat_viscera_relationship.png"),
       width=6.5, height=4, units="in", dpi=600)
