# Plot MIC for for rifampicin
# Data from: Albano et al, AAC 2019. DOI: https://doi.org/10.1128/aac.00959-19
# BJI 2024, Gold Coast

# Library
library(tidyverse)
rm(list = ls())
setwd(paste0(here::here()))
getwd()

# Create MIC table: S. aureus 
level <- c("≤0.001", "0.002", "0.004", "0.008", "0.016", "0.03", "0.06", "0.125", "0.25", "0.5", "1", "2", "4", "8", "≥16")
rifampicin_mic <- c("1 (0.9)", "4 (4.5)", "5 (9)", "21 (27.9)", "47 (69.4)", "27 (94.6)", "4 (98.2)", NA, NA, "1 (99)", NA, NA, "1 (100)", NA, NA)
rifampicin_mic <- as.integer(str_remove(rifampicin, " .*"))
rifampicin_mbic <- c(NA, NA, "4 (3.6)", "81 (76.6)", "24 (98.2)", NA, NA, "1 (99)", NA, NA, "1 (100)", NA, NA, NA, NA)
rifampicin_mbbc <- c(NA, NA, NA, "1 (0.9)", NA, "1 (1.8)", NA, "1 (2.7)", NA, NA, NA, "6 (8.1)", "36 (40.5)", "50 (85.6)", "16 (100)")


df_mic <- tibble(level = as_factor(level), rifampicin_mic, rifampicin_mbic, rifampicin_mbbc) %>%
  mutate(across(.cols = starts_with("rif"),
                .fns = ~as.integer(str_remove(., " .*"))))

df_mic_long <- df_mic %>%
  pivot_longer(starts_with("rif"), names_to = "parameter", values_to = "n") %>%
  mutate(parameter = fct_rev(str_to_upper(str_remove(parameter, "rifampicin_"))))

df_mic_long %>%
  ggplot(aes(x = level, y = value, fill = parameter)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_bw()
df_list <- list()
df_list$`S. aureus` <- df_mic_long

# Create MIC table: S. epidermidis
level <- c("≤0.001", "0.002", "0.004", "0.008", "0.016", "0.03", "0.06", "0.125", "0.25", "0.5", "1", "2", "4", "8", "≥16")
rifampicin_mic <- c("2 (2.4)", "12 (17)", "28 (51.2)", "35 (93.9)", "4 (98.8)", NA, NA, NA, NA, "1 (100)", NA, NA, NA, NA, NA)
rifampicin_mbic <- c(NA, "3 (3.6)", "8 (13.4)", "47 (70.7)", "19 (93.9)", "3 (97.5)", "1 (98.8)", NA, NA, NA, "1 (100)", NA, NA, NA, NA)
rifampicin_mbbc <- c(NA, "1 (1.2)", NA, "4 (6.2)", "5 (12.3)", "4 (17.3)", "3 (21)", "7 (29.6)", "5 (35.8)", NA, "7 (44.4)", "17 (65.4)", "17 (86.4)", "9 (97.5)", "2 (100)")


df_mic <- tibble(level = as_factor(level), rifampicin_mic, rifampicin_mbic, rifampicin_mbbc) %>%
  mutate(across(.cols = starts_with("rif"),
                .fns = ~as.integer(str_remove(., " .*"))))

df_mic_long <- df_mic %>%
  pivot_longer(starts_with("rif"), names_to = "parameter", values_to = "n") %>%
  mutate(parameter = fct_rev(str_to_upper(str_remove(parameter, "rifampicin_"))))

df_mic_long %>%
  ggplot(aes(x = level, y = n, fill = parameter)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_bw()
df_list$`S. epidermidis` <- df_mic_long

# Merge
df_mic_all <- bind_rows(df_list, .id = "species")
df_mic_all %>%
  ggplot(aes(x = level, y = n, fill = parameter)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~species, ncol = 1) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(x = "Concentration (mg/l)", y = "# isolates") +
  theme_bw() +
  theme(text = element_text(face = "bold"),
        strip.text = element_text(face = "bold.italic"),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/rifampicin_activity.pdf", width = 6, height = 4.5)
