
library(dplyr)
library(readr)
library(stringr)
library(openxlsx)

custom <- read.csv("minetap_custom_06.02.csv") %>%
  mutate(tsEvent = as.POSIXct(tsEvent, origin = "1970-01-01")) %>%
  arrange(idDevice, tsEvent)

cohorts <- custom %>%
  group_by(idDevice) %>%
  slice(1) %>%
  ungroup() %>%
  filter(eventName == "maxOpenBlock",
         params.value == "['1']") %>%
  select(idDevice, tsEvent)

first_cohort <- cohorts %>%
  filter(as.Date(tsEvent) < "2021-01-31")

second_cohort <- cohorts %>%
  filter(as.Date(tsEvent) >= "2021-01-31")

### первая когорта

game_progress_first_cohort <- data.frame()

for (i in seq(as.Date("2021-01-18"), by = "day", length.out = 13)){
  
  cohort_data <- first_cohort %>%
    filter(as.Date(tsEvent) == as.Date(i, origin = "1970-01-01"))
  
  game_progress_first_cohort <- rbind(game_progress_first_cohort,
                                      (custom %>%
                                         filter(idDevice %in% cohort_data$idDevice,
                                                eventName == "maxOpenBlock",
                                                as.Date(tsEvent) %in% seq(as.Date(i, origin = "1970-01-01"), by = "day", length.out = 2)) %>%
                                         distinct(idDevice, params.value) %>%
                                         count(params.value) %>%
                                         arrange(-n) %>%
                                         mutate(prop = round(n / nrow(cohort_data) * 100, 2))))
  
}

game_progress_first_cohort <- game_progress_first_cohort %>%
  group_by(params.value) %>%
  summarise(first_cohort_avg = round(mean(prop), 2)) %>%
  mutate(params.value = as.numeric(str_remove_all(params.value, "\\D+"))) %>% 
  arrange(params.value)

### вторая когорта

game_progress_second_cohort <- data.frame()

for (i in seq(as.Date("2021-01-31"), by = "day", length.out = 5)){
  
  cohort_data <- second_cohort %>%
    filter(as.Date(tsEvent) == as.Date(i, origin = "1970-01-01"))
  
  game_progress_second_cohort <- rbind(game_progress_second_cohort,
                                      (custom %>%
                                         filter(idDevice %in% cohort_data$idDevice,
                                                eventName == "maxOpenBlock",
                                                as.Date(tsEvent) %in% seq(as.Date(i, origin = "1970-01-01"), by = "day", length.out = 2)) %>%
                                         distinct(idDevice, params.value) %>%
                                         count(params.value) %>%
                                         arrange(-n) %>%
                                         mutate(prop = round(n / nrow(cohort_data) * 100, 2))))
  
}

game_progress_second_cohort <- game_progress_second_cohort %>%
  group_by(params.value) %>%
  summarise(second_cohort_avg = round(mean(prop), 2)) %>%
  mutate(params.value = as.numeric(str_remove_all(params.value, "\\D+"))) %>% 
  arrange(params.value)

cohort_game_progress <- full_join(game_progress_first_cohort,
                                  game_progress_second_cohort,
                                  by = "params.value") %>%
  mutate(diff = second_cohort_avg - first_cohort_avg)

write.xlsx(cohort_game_progress, "cohort_game_progress.xlsx")
