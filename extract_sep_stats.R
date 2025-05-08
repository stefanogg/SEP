# Script to (tentatively) extract statistics on consultation activity in septic 
# surgery from the infamous Excel spreadsheet

# Library
library(tidyverse)
library(magrittr)
library(readxl)
# install.packages("writexl")
library(writexl)
# install.packages("visdat")
# install.packages("xlsx")
# install.packages("rJava")
# library(rJava)
# library(xlsx)

# Free memory space
rm(list = ls())

# Import the infamous table
file <- "data/Tableau_Chirurgie_Septique_pour_statistiques.xlsx"
col_types <- rep("text", 10)
col_types <- c(
  "text",
  "date",
  "date",
  col_types
) 

dat <- read_excel(
  path = "data/Tableau_Chirurgie_Septique_pour_statistiques.xlsx",
  col_types = col_types
)

dat <- read_excel(file,
                  col_types = col_types) %>%
  rename(
    patient = X__1,
    date_1 = X__2,
    date_2 = `Responsabilité / localisation`
  ) %>%
  mutate(
    year_1 = date_1,
    year_2 = date_2
  ) %>%
  mutate_at(
    .vars = vars(year_1, year_2),
    .funs = funs(as.numeric(lubridate::year(.)))
  ) %>%
  mutate_at(
    .vars = vars(year_1, year_2),
    .funs = funs( if_else(. < 2000, NA_real_, .))
  ) %>%
  mutate(
    date_1 = if_else( 
      is.na(year_1),
      NA_character_,
      as.character(date_1)
      ),
    date_2 = if_else( 
      is.na(year_2),
      NA_character_,
      as.character(date_2)
    )
  )
# Add id variable and clean
# check for missing patient id
source("~/Documents/MDU/R/Functions/complete_digits.R")
dat %>%
  filter(is.na(patient)) # 58 rows



dat <- dat %>%
  filter(!is.na(patient)) 
id <- dat %>%
  mutate(id = str_c(
    "SEP-",
    as.character(row_number())
    ))%>%
  .$id 
id <- sapply(id, complete_digits, n = 4)
dat <- dat %>%
  mutate(id = id)

# Vector with antibiotic data
antibiotics <- dat$`Antibiothérapie actuelle`
pattern <- "\\d+\\.(\\d{2})"
antibiotics_month <- str_match(antibiotics, pattern)
# Vector with diagnoses data
diagnoses <- dat$`Problèmes principaux`
pattern_4d <- "201\\d{1}"
diagnoses_year4d <- str_extract_all(diagnoses, pattern_4d)
diagnoses_year4d <- sapply(diagnoses_year4d, max)
pattern_2d <- "\\d{2}\\.\\d{2}\\.(1\\d{1})\\D"
diagnoses_year2d <- str_match_all(diagnoses, pattern_2d)
# functions
extract_year4d <- function(string){
  list <- str_extract_all(string, pattern = pattern_4d)
  vector <- sapply(list, max)
  return(vector)
}
extract_year2d <- function(string){
  list <- str_match_all(string, pattern = pattern_2d)
  # extract match
  match <- lapply(list, function(x){
    x <- x[,2]
    return(x)
  })
  vector <- sapply(match, max)
  return(vector)
}
extract_year2d(diagnoses)

# functions
# date pattern
date_pattern <- "(201\\d{1})|\\d{2}\\.\\d{2}\\.(1\\d{1})|\\d{2}/\\d{2}/(1\\d{1})"
string <- "10.11"

extract_date <- function(string, output){
  
  match <- str_match_all(string, date_pattern)
  
  if ( output == "4d" ){
    out <- sapply(match, function(x){
      x <- max(x[,2], na.rm = T)
      return(x)
    })
  }
  
  if ( output == "2d" ){
    out <- sapply(match, function(x){
      x_dot <- max(x[,3], na.rm = T)
      x_slash <- max(x[,4], na.rm = T)
      x <- max(x_dot, x_slash, na.rm = T)
      return(x)
    })
  }
  
  if ( output == "all" ){
    out <- match
  }
  
  return(out)
}
strings <- dat$`Antibiothérapie actuelle`[1:100]
extract_date(strings, output = "all")

# Dataframe of dates. Reliable dates: date_1 and date_2 and dates that
# can be extracted from the antibiotics column
year_dat <- dat %>%
  select(id,
         patient,
         date_1,
         year_1,
         date_2,
         year_2,
         diagnoses = `Problèmes principaux`,
         antibiotics = `Antibiothérapie actuelle`,
         clinical_course = Evolution) %>%
  mutate(
    antibiotics_cat = str_c(antibiotics, clinical_course),
    antibiotics_cat = if_else(
    str_detect(
      str_to_lower(antibiotics_cat),
      "augmentin|vanco|rimactan|rifamp|cipro|levo|peni|tavanic|clinda|dalac|amoxicill|floxa"
      ),
    antibiotics,
    NA_character_
  )) %>%
  mutate(
    year4d = extract_date(antibiotics_cat, output = "4d"),
    year2d = str_c("20", 
                   extract_date(antibiotics_cat, output = "2d")),
    year_final = pmax(year4d, year2d, na.rm = T)
    )



# Tag reliable dates
confirmed_dates_df <- year_dat %>%
  mutate(
    is_confirmed_date = !is.na(year_1) | !is.na(year_2),
    is_confirmed_year = !is.na(year_1) | !is.na(year_2) | !is.na(year_final)
  )
# stats
confirmed_dates_df %>%
  count(is_confirmed_date, is_confirmed_year) %>%
  mutate(p = n/sum(n))

# Final dataset that will be completed manually
sep_stats_to_complete <- confirmed_dates_df %>%
  mutate(date_final = case_when(
    is_confirmed_year & !is.na(date_1) ~ date_1,
    is_confirmed_year & !is.na(date_2) ~ date_2,
    is_confirmed_year & is.na(date_1) & is.na(date_2) ~ year_final
  ))
# save to disk
dir.create("dataframes")
sep_stats_to_complete %>%
  writexl::write_xlsx("dataframes/sep_stats_to_complete.xlsx")
# randomly split NA date in two
df <- sep_stats_to_complete %>%
  filter(!is_confirmed_year) %>%
  select(id, patient, diagnoses, antibiotics, clinical_course, date_final)
set.seed(44)
sep_stats_to_complete_1 <- df %>%
  sample_frac(.5)
sep_stats_to_complete_2 <- df %>%
  anti_join(sep_stats_to_complete_1, by = "id")
# save to disk
sep_stats_to_complete_1 %>%
  write_xlsx("dataframes/sep_stats_to_complete_1.xlsx")
sep_stats_to_complete_2 %>%
  write_xlsx("dataframes/sep_stats_to_complete_2.xlsx")


# # Graphical representation
# p <- year_dat %>%
#   mutate(rank = row_number()) %>%
#   ggplot(aes(x = rank,
#              y = year_final)) +
#   geom_point() +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank())
# p
# # Stats
# year_dat %>%
#   count(year_final)

# Reimport data and count missing values ----
col_types <- c(
  rep("text", 5),
  "date"
)
data_1 <- read_excel("data/Sep_stats_completed_Aisha.xlsx", 
                     col_types = col_types) %>%
  select(id, date_final)
n_missing_1 <- length(which(is.na(data_1$date_final)))
col_types <- c(
  col_types,
  "text"
)
data_2 <- read_excel("data/Sep_stats_completed_Eva.xlsx",
                    col_types = col_types) %>%
  select(id, date_final)
n_missing_2 <- length(which(is.na(data_2$date_final)))
data_merged <- bind_rows(
  data_1,
  data_2
)


# Final dataset
non_missing_stats <- sep_stats_to_complete %>%
  filter(is_confirmed_year) %>%
  mutate(date_final = as.Date(date_final))
missing_stats <- sep_stats_to_complete %>%
  filter(!is_confirmed_year) %>%
  select(-date_final)
# ad missing dates
missing_stats_completed <- missing_stats %>%
  left_join(data_merged) %>%
  mutate(date_final = as.Date(date_final)) 
# merge
sep_stats_completed <- bind_rows(
  non_missing_stats,
  missing_stats_completed
) %>%
  # new year_final
  mutate(year_final = if_else(
    is.na(year_final),
    lubridate::year(date_final),
    as.numeric(year_final)
  ))
visdat::vis_dat(sep_stats_completed)
# count missing year
length(which(is.na(sep_stats_completed$year_final)))
# first stats
# projection for 2018
# stats after 6 months
stats <- list(
  n_months = 7,
  n_consultations = 326
)
projection_2018 <- as.integer(stats$n_consultations/stats$n_months*12)
# official stats 
stats_soarian <- tibble(
  year_final = 2014:2017,
  n = c(98, 244, 338, 315),
  source = "Soarian"
)
df <- sep_stats_completed %>%
  filter(year_final %in% 2010:2017) %>%
  count(year_final) %>%
  # add soarian data
  mutate(source = "Excel table") %>%
  full_join(stats_soarian) %>%
  add_row(year_final = 2018, n = projection_2018, source = "Soarian") %>%
  mutate(projection = year_final == 2018) 
# duplicate_row <- tibble(
#   year_final = 2017,
#   n = df$n[which(df$year_final == 2017)],
#   projection = T
# )
# df <- bind_rows(df, duplicate_row)
p <- df %>%
  ggplot(
    mapping = aes(x = year_final, y = n)
  ) +
  geom_line(aes(linetype = source)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 800)) +
  scale_x_continuous(breaks = seq(2010, 2018, 1)) +
  theme_bw() 
p

p <- p +
  labs(x = "\nAnnée",
       y = "Premières consultations\n") +
  scale_linetype(name = "", labels = c("Tableau Excel", "Consultations Soarian"))
p
# Export table with data
tabl <- df %>%
  select(-projection) %>%
  spread(source, n)

# Number of patients
# extract IPP
sep_stats_patients <- sep_stats_completed %>%
  mutate(ipp = str_extract(
    patient,
    "\\d+"
  ))

