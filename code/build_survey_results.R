

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

# Expand results
results <- survey_results %>%
  # Add distribution median/cv
  mutate(ln_median=exp(meanlog),
         ln_cv=sqrt(exp(sdlog^2)-1))


# Export data
################################################################################

# Export
saveRDS(results, file=file.path(datadir, "CA_OR_WA_da_survey_results.Rds"))





