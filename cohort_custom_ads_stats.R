library(dplyr)
library(readr)
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
parser <- add_option(parser, c("-v", "--interval"),
                     help = "Event interval")
args <- parse_args(parser)

cohort_subset <- function(data, start, end){
  cohort <- data %>%
    filter(as.Date(tsInstall) >= start,
           as.Date(tsInstall) <= end)
  
  return(cohort)
}

cohort_ads_stats <- function(data, cohort_data, start, end, interval){
  num_events_cohort_data <- data.frame()
  num_unique_events_cohort_data <- data.frame()
  
  start_end_diff <- as.integer(difftime(as.Date(end), as.Date(start), units = "days"))
  
  for (i in seq(as.Date(start), by = "day", length.out = start_end_diff + 1)){
    coh_data <- cohort_data %>%
      filter(as.Date(tsInstall) == as.Date(i, origin = "1970-01-01"))
    
    cohort_users_num <- nrow(coh_data)
    
    new_data <- data %>%
      filter(idDevice %in% coh_data$idDevice,
             eventName %in% c("openPopup", "showAdsClicked", "showAdsFinished"),
             params.value %in% c("['boost']", "['BoostRewardedVideo']"),
             as.Date(tsEvent) %in% seq(as.Date(i, origin = "1970-01-01"), by = "day",
                                       length.out = as.numeric(interval)))
    
    # кол-во событий
    num_events <- new_data %>%
      count(eventName) %>%
      mutate(events_per_user = n / cohort_users_num)
    
    # кол-во уникальных событий
    num_unique_events <- new_data %>%
      group_by(eventName) %>%
      count(idDevice) %>%
      ungroup() %>%
      count(eventName) %>%
      mutate(users_made_event = n / cohort_users_num)
    
    num_events_cohort_data <- rbind(num_events_cohort_data, num_events)
    num_unique_events_cohort_data <- rbind(num_unique_events_cohort_data, num_unique_events)
  }
  
  num_events_cohort_data <- num_events_cohort_data %>%
    group_by(eventName) %>%
    summarise(mean_num_events = mean(n),
              mean_events_per_user = mean(events_per_user))
  
  num_unique_events_cohort_data <- num_unique_events_cohort_data %>%
    group_by(eventName) %>%
    summarise(mean_num_unique_events = mean(n),
              mean_users_made_event = mean(users_made_event))
  
  ad_stats_cohort <- num_events_cohort_data %>%
    full_join(num_unique_events_cohort_data, by = "eventName")
  
  return(ad_stats_cohort)
}

main <- function(args){
  base <- sub(".csv", "", basename(args$infile))
  
  data <- read.csv(args$infile) %>%
    mutate(tsInstall = as.POSIXct(tsInstall, origin = "1970-01-01"),
           tsEvent = as.POSIXct(tsEvent, origin = "1970-01-01")) %>%
    arrange(idDevice, tsEvent)
  
  cohorts <- data %>%
    select(idDevice, tsInstall) %>%
    distinct(idDevice, tsInstall)
  
  first_cohort <- cohort_subset(data = cohorts,
                                start = args$fc_start,
                                end = args$fc_end)
  
  ads_stats_first_cohort <- cohort_ads_stats(data = data,
                                             cohort_data = first_cohort,
                                             start = args$fc_start,
                                             end = args$fc_end,
                                             interval = args$interval)
  

  ads_stats_first_cohort %>%
    write.xlsx(paste(args$outdir, "/", base, ".cohort_ads_stats.xlsx", sep = "", collapse = ""))
}

main(args)
