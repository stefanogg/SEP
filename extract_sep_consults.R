# Analyse Soarian consultations

# Library
library(tidyverse)
library(lubridate)
library(magrittr)

# Free memory space
rm(list = ls())

# Import data
# list files
# files <- list.files("data/") %>%
#   str_subset("ADISSA")
files <- "ADISSA-1113_20181112_1000.csv"
sep_dat <- bind_rows(lapply(files, function(x){
  path <- str_c("data/", x)
  file <- read_csv(path)
  return(file)
}))



# Hosp in traumatology and orthopaedics
two_hosp <- sep_dat %>%
  group_by(IPP) %>%
  filter(any(SERVICE_RESPONSABLE == "TRAH") & any(SERVICE_RESPONSABLE == "OTPH"))

# Explore data
# number of consults per day
sep_df <- sep_dat %>%
  mutate(
    date = lubridate::dmy_hms(DT_INTERVENTION)
  ) %>%
  mutate(type = if_else(
    str_detect(AVANCEMENT,"Premi"), "Première consultation", AVANCEMENT
  ))
p <- sep_df %>%
  ggplot(
    mapping = aes(
      x = lubridate::date(date)
    )
  ) +
  geom_bar() +
  facet_wrap(~lubridate::year(date))
p
# number of consults per service
p <- sep_df %>%
  ggplot(aes(fct_infreq(SERVICE_RESPONSABLE))) +
  geom_bar() +
  coord_flip()
p
# number of consults per supervisor
p <- sep_df %>%
  ggplot(aes(fct_infreq(SUIVI_SUPERVISION))) +
  geom_bar() +
  coord_flip()
p
# number of consults per type
p <- sep_df %>%
  ggplot(aes(type)) +
  geom_bar() +
  coord_flip()
p
# number of consults per month by type
plot_df <- sep_df %>%
  # mutate(month = str_c(lubridate::month(date, T), 
  #                      " ",
  #                      lubridate::year(date))) %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  count(month, type) 
p <- plot_df %>%
  replace_na(type = "Consultation de suivi") %>%
  ggplot(aes(month, n)) +
  geom_line(aes(linetype = type))
p
# Stats
# type
sep_stats <- sep_df %>%
  group_by(IPP) %>%
  mutate(no_first = 
           all(!str_detect(type, "Première consultation"))
  ) %>%
  mutate(type_new = if_else(
    no_first & date == min(date), "Première consultation", type
  )) %>%
  ungroup() %>%
  count(type)
sep_stats

# service
# group labels
otr <- c(
  "TRAH",
  "OTPH",
  "DALB",
  "DALC",
  "TRA1",
  "ONMH",
  "OTP1"
)
cpr <- c(
  "CPRH",
  "CPR1"
)
cpm <- c(
  "CPMH",
  "CPM1"
)

sep_stats <- sep_df %>%
  mutate(service = case_when(
    SERVICE_RESPONSABLE %in% otr ~ "OTR",
    SERVICE_RESPONSABLE %in% cpr ~ "CPR",
    SERVICE_RESPONSABLE %in% cpm ~ "CPM",
    TRUE ~ SERVICE_RESPONSABLE
  )) %>%
  count(service)
sep_stats %>%
  arrange(desc(n))
# summary per patient
sep_patients <- sep_df %>%
  group_by(IPP) %>%
  summarise(n_consults = n(),
            first_consult = min(date),
            type = toString(type))
# summary per hospitalisation
sep_hospitalisation <- sep_df %>%
  group_by(N_SEJOUR) %>%
  summarise(n_consults = n(),
            first_consult = min(date))
# outpatient consultations
outpatient_dat <- read_csv("data/MIN-STAT.csv") %>%
  rename(IPP = Ipp)
outpatient_IPP <- outpatient_dat %>%
  select(IPP) %>%
  unique() %>%
  pull()
# intersect data
sep_outpatient <- sep_df %>%
  filter(IPP %in% outpatient_IPP)


# November 2019: need to check whether the consultations were done during working hours
# Total number of consultations
year <- 2017
month <- 10
day <- 1
start <- make_date(year, month, day)
str(start)
year <- 2018
month <- 9
day <- 30
end <- make_date(year, month, day)
str(end)
stats_interval <- interval(start, end)
stats_interval
df <- sep_df %>%
  filter(date %within% stats_interval)


df_on_call <- sep_df %>%
  mutate(week_day = wday(date, label = TRUE),
         hour = hour(date))
# plot
df_on_call %>%
  ggplot(aes(week_day)) +
  geom_bar() +
  labs(x = "\nDay of the week", y = "Number of consultations\n") +
  theme_bw()
ggsave("figures/week_day.pdf", width = 8, height = 6)
df_on_call %>%
  ggplot(aes(as.factor(hour))) +
  geom_bar() +
  labs(x = "\nHour of the day", y = "Number of consultations\n") +
  theme_bw()
ggsave("figures/hour.pdf", width = 8, height = 6)

# stats
# number of consultations during weekend
df_on_call %>%
  filter(week_day %in% c("Sun","Sat")) %>%
  nrow()
# number of consultations outside working hours (between 20:00 and 8:00)
df_on_call %>%
  filter(hour %in% c(0:7, 20:23)) %>%
  nrow()
# number of consultations outside working hours (between 18:00 and 8:00)
df_on_call %>%
  filter(hour %in% c(0:7, 18:23)) %>%
  nrow()
