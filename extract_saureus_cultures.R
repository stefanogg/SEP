# Script to extract data on saureus cultures during 2017 

# Library
# install.packages("tidyverse")
library(tidyverse)
library(magrittr)

# Free memory space
rm(list = ls())

# Upload data 
file <- "data/2016-STAAUR_sg_date.csv"
saureus_dat <- read_csv(file,
                        local = locale(encoding = "latin1"))
unique(saureus_dat$Prel.)

# Extract number of positive blood cultures and positive osteoarticular samples
# How many episodes with positive blood culture
n_sab <- saureus_dat %>%
  filter(str_detect(`Prel.`, "HEMOCULTURE")) %>%
  .$NIP %>%
  n_distinct()

saureus_blood_bone_joint <- saureus_dat %>%
  select(NIP, `Prel.`, `Orig.`, `Dt.recept.`) %>%
  filter(`Orig.` %in% c("SSC", "SOA") | str_detect(`Prel.`, "HEMOCULTURE")) %>%
  mutate(sample_type = case_when(
    `Orig.` == "SOA" ~ "Bone/joint",
    `Orig.` %in% c("SSC", "SSP") ~ "Blood"
  )) %>%
  mutate(sample_date = lubridate::ymd_hms(`Dt.recept.`)) %>%
  group_by(NIP, sample_type) %>%
  summarise(samples = toString(`Prel.`),
            first_positive = min(sample_date),
            last_positive = max(sample_date),
            sample_delay = last_positive - first_positive)
saureus_summary <- saureus_blood_bone_joint %>%
  ungroup() %>%
  count(sample_type)
saureus_blood_and_joint <- saureus_blood_bone_joint %>%
  group_by(NIP) %>%
  filter(any(sample_type == "Blood") & any(sample_type == "Bone/joint"))

# Import lists of "at risk patients"
file <- "data/diabetics_dialysis.txt"
diab_dial_dat <- read_lines(file, skip = 1) %>%
  as.numeric()
diab_dial_df <- tibble(
  NIP = diab_dial_dat,
  disease = "Diabetes + Dialysis"
) 
saureus_diab <- diab_dial_df %>%
  left_join(saureus_blood_bone_joint)

# Categorise persistent and relapsing bacteremias
saureus_persister_type <- saureus_blood_bone_joint %>%
  mutate(persister_type = case_when(
    sample_delay >= 3 & sample_delay < 15 ~ "Persistent",
    sample_delay >= 15 ~ "Relapsing"
  ))

# Dataframe with blood culture isolates and other isolates from the same patient
sab_dat <- saureus_dat %>%
  group_by(NIP) %>%
  filter(
    any(
      str_detect(
        `Prel.`, 
        "HEMOCULTURE"
        )
      )
    ) %>%
  mutate(sample_date = lubridate::ymd_hms(`Dt.recept.`)) 
sab_other_samples <- sab_dat %>%
  group_by(NIP) %>%
  summarise(bc_date = toString(sample_date[str_detect(`Prel.`, "HEMOCULTURE")]),
            other_samples = toString(`Prel.`[!str_detect(`Prel.`, "HEMOCULTURE")]),
            other_codes = toString(unique(`Orig.`)))
# exclude prosthetic joints and osteomyelitis
sab_foci <- sab_other_samples %>%
  filter(str_detect(other_samples, "ARTICULAIRE") |
           str_detect(other_samples, "ABCES") & str_detect(other_codes, "POU")) %>%
  filter(!str_detect(other_samples, "PROTHESE|PT"))
