
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/perl_etal"
plotdir <- "figures"

# Read data
data <- readxl::read_excel(file.path(datadir, "Perl_etal_1990_Table3.xlsx")) %>%
  # Factor type
  mutate(type=factor(type, levels=c("No symptoms", "GI symptoms", "Neurological symptoms", "Hospitalized", "ICU")))

# Reference points
ref_pts <- readxl::read_excel(file.path(datadir, "reference_points.xlsx")) %>%
  filter(name=="PARD")

# Read symptoms
symptoms_orig <- readxl::read_excel(file.path(datadir, "Perl_etal_1990_Table1.xlsx"))


# Build data
################################################################################

# Format data
symptoms <- symptoms_orig %>%
  # Calculate percents
  mutate(prop_y=n/noverall,
         prop_n=1-prop_y) %>%
  # Simplify
  select(category, symptom, prop_y, prop_n) %>%
  # Gather
  gather(key="yn", value="prop", 3:4) %>%
  # Categorize for color
  mutate(color=ifelse(yn=="prop_n", "None", category),
         color=ifelse(yn!="prop_n" & category %in% c("Treatment", "Death"), symptom, color)) %>%
  # Factors
  mutate(symptom=factor(symptom, levels=c("Nausea", "Vomiting", "Abdominal cramps", "Diarrhea", "Headache", "Memory loss", "Death", "Hospitalized", "ICU"))) %>%
  mutate(color=factor(color, levels=c("None", "Gastrointestinal", "Neurological", "Death", "Hospitalized", "ICU")))


# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    # Guides
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))


# Plot data
g1 <- ggplot(symptoms, aes(y=reorder(symptom, desc(symptom)), x=prop, fill=color)) +
  geom_bar(stat="identity", color="black", lwd=0.1) +
  # Labels
  labs(x="Percent of cases", y="", tag="A") +
  # Limits
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Legend
  scale_fill_manual(name="", values=c("grey90", "darkgreen", "orange", "darkred", "navy", "purple")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g1

# Plot data
g2 <- ggplot(data, aes(x=da_mg/60, y=0.05, fill=type)) +
  geom_point(pch=21, size=4) +
  # Reference points
  geom_point(data=ref_pts, mapping=aes(x=value, y=0.05), inherit.aes = F, size=2, color="black", pch=16) +
  # Label LOAEL
  geom_segment(x=1, y=0.05, xend=1, yend=1, linetype="dotted", color="grey50") +
  annotate(geom="text", x=1, y=0.95, label="Lowest observed adverse effects level  (LOAEL)", hjust=-0.03, vjust=0, size=2.5) +
  # Label NOAEL
  geom_segment(x=0.33, y=0.05, xend=0.33, yend=0.7, linetype="dotted", color="grey50") +
  annotate(geom="text", x=0.33, y=0.7, label="No observed adverse effects level  (NOAEL)", hjust=-0.03, vjust=0, size=2.5) +
  # Label NOAEL
  geom_segment(x=0.1, y=0.05, xend=0.1, yend=0.4, linetype="dotted", color="grey50") +
  annotate(geom="text", x=0.1, y=0.4, label="Acute reference dose (ARfD)", hjust=-0.03, vjust=0, size=2.5) +
  # Labels
  labs(x="Domoic acid per\nkilogram of body weight (mg/kg)", y="", tag="B") +
  # Limits
  lims(y=c(0,1)) +
  # Legend
  scale_fill_manual(name="", values=c("grey90", "darkgreen", "orange", "navy", "purple")) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y=element_blank(),
        axis.line.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom")
g2

# # Export
# ggsave(g2, filename=file.path(datadir, "reference_doses.png"),
#        width=6.5, height=2.5, units="in", dpi=600)


# Merge plots
g <- gridExtra::grid.arrange(g1, g2,
                             layout_matrix=matrix(data=c(1,3,
                                                         2,2), ncol=2, byrow=T))

# Plots
ggsave(g, filename=file.path(plotdir, "Box1_action_threshold_origin.png"),
       width=6.5, height=4.5, units="in", dpi=600)


