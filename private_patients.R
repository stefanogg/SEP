# Summary of private patients monthly invoices

# Library
library(tidyverse)
library(magrittr)
library(readxl)

# Free memory space
rm(list = ls())

# Upload data
dat <- read_excel("data/Private_patients.xlsx")

# Summarise
private_df <- dat %>%
  filter(Superviseur == "SG") %>%
  filter(!is.na(X__2))

private_summary <- private_df %>%
  mutate(month = lubridate::month(`Envoi à la préfacturation`)) %>%
  group_by(month) %>%
  summarise(sum = sum(`Total Fr.`))


# mean
mean <- mean(private_summary$sum)
mean
# projection over 14 months
projection <- 14*mean
# variable part (22%)
variable <- .22 * projection

