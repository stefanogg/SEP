# Script to generate a timeline for within-host research projects

# Library
library(tidyverse)
library(magrittr)

# Generate a table
time <- seq(0,36,6)
label <- c(
  `1.5` = "Nasal swab",
  `1` = "Wound biopsy"
)
timeline_dat <- tibble(
  time = rep(time,2),
  # label = "Nasal swab\nInguinal swab\nWound biopsy"
  label = rep(label, 7),
  y = as.numeric(names(label))
) 

# Plot
p <- timeline_dat %>%
  ggplot(
    mapping = aes(
      x = time,
      y = y,
      label = label
    )
  ) +
  geom_text() +
  geom_segment(
    mapping = aes(x = time, xend = time, y = .75, yend = .25),
    arrow = arrow(angle = 20, type = "closed", length = unit(0.15, "inches"))
  ) +
  scale_x_continuous(breaks = time) +
  scale_y_continuous(limits = c(0,2)) +
  coord_cartesian(xlim = c(-1, 37)) +
  labs(x = "months", y = "") +
  cowplot::theme_cowplot() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p
# save to disk
file <- "figures/timeline.pdf"
pdf(file, width = 7, height = 3, useDingbats = F)
p
dev.off()
