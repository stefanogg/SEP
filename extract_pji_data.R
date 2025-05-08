# Extract PJI numbers from the ortho dpt audit spreadsheet

# Library
library(tidyverse)
library(readxl)
library(janitor)
rm(list = ls())
setwd(paste0(here::here()))
getwd()

# Raw data
data <- read_xlsx("~/Library/CloudStorage/OneDrive-TheRoyalMelbourneHospital/Fracture_related_infections/Database/Infections.xlsx")

# Fix formatting issues
df_audit <- data %>%
  janitor::remove_empty() %>%
  janitor::clean_names()

