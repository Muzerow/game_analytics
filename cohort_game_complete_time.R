
library(dplyr)
library(readr)
library(stringr)
library(openxlsx)
library(optparse)

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"),
                     help = "Input file")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help = "Output directory")
parser <- add_option(parser, c("-s", "--fc_start"),
                     help = "First cohort start date")
parser <- add_option(parser, c("-e", "--fc_end"),
                     help = "First cohort end date")
parser <- add_option(parser, c("-c", "--fc_country"),
                     help = "First cohort country")
parser <- add_option(parser, c("-d", "--sc_start"),
                     help = "Second cohort start date")
parser <- add_option(parser, c("-r", "--sc_end"),
                     help = "Second cohort end date")
parser <- add_option(parser, c("-b", "--sc_country"),
                     help = "Second cohort country")
args <- parse_args(parser)

cohort_subset <- function(data, start, end, country){
  cohort <- data %>%
    filter(as.Date(tsInstall) >= start,
           as.Date(tsInstall) <= end)
  
  if (country != "All") {
    cohort <- cohort %>%
      filter(idCountryISOAlpha2 == country)
  }
  
  return(cohort)
}

event_time_diff <- function(data, cohort_users) {
  time_to_reach_block <- data.frame()
  
  for (i in cohort_users) {
    first_block <- data %>%
      filter(idDevice == i,
             params.value == "['1']") %>%
      select(idDevice, params.value, tsEvent) %>%
      rename(time_1 = tsEvent)
    
    last_block <- data %>%
      filter(idDevice == i,
             params.value == "['29']") %>%
      select(idDevice, params.value, tsEvent) %>%
      rename(time_29 = tsEvent)
    
    user_block <- inner_join(first_block, last_block, by = "idDevice")
    
    time_to_reach_block <- rbind(time_to_reach_block, user_block)
  }
  
  time_to_reach_block <- time_to_reach_block %>%
    mutate(diff = difftime(time_29, time_1, units = "hours")) %>%
    arrange(diff)
  
  time_to_reach_block <- data.frame(metric = c("mean_time_diff", "median_time_diff"),
                                    value = c(mean(time_to_reach_block$diff), median(time_to_reach_block$diff)))
  
  return(time_to_reach_block)
}


main <- function(args){
  base <- sub(".csv", "", basename(args$infile))
  
  data <- read.csv("minetap_28.02.csv") %>%
    mutate(tsInstall = as.POSIXct(tsInstall, origin = "1970-01-01"),
           tsEvent = as.POSIXct(tsEvent, origin = "1970-01-01")) %>%
    arrange(idDevice, tsEvent) %>%
    select(idCountryISOAlpha2, tsInstall, idDevice, eventName, params.value, tsEvent) %>%
    filter(eventName == "maxOpenBlock",
           params.value %in% c("['1']", "['29']"))
  
  cohorts <- data %>%
    select(idCountryISOAlpha2, idDevice, tsInstall) %>%
    distinct(idCountryISOAlpha2, idDevice, tsInstall)
  
  first_cohort <- cohort_subset(data = cohorts,
                                start = args$fc_start,
                                end = args$fc_end,
                                country = args$fc_country)
  
  second_cohort <- cohort_subset(data = cohorts,
                                 start = args$sc_start,
                                 end = args$sc_end,
                                 country = args$sc_country)
  
  data$duplicate <- duplicated(data[,c("idDevice", "params.value")])
  
  data <- data %>%
    filter(duplicate == FALSE)
  
  event_time_diff_first_cohort <- event_time_diff(data = data,
                                                  cohort_users = first_cohort$idDevice) %>%
    rename(value_first_cohort = value)
  
  event_time_diff_second_cohort <- event_time_diff(data = data,
                                                   cohort_users = second_cohort$idDevice) %>%
    rename(value_second_cohort = value)
  
  game_complete_time <- full_join(event_time_diff_first_cohort,
                                  event_time_diff_second_cohort,
                                  by = "metric") %>%
    mutate(diff = value_second_cohort - value_first_cohort)
  
  write.xlsx(game_complete_time, paste(args$outdir, "/", base, ".game_complete_time.xlsx", sep = "", collapse = ""))
}

main(args)

