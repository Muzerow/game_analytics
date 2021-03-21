library(dplyr)
library(readr)
library(stringr)
library(lubridate)
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
parser <- add_option(parser, c("-v", "--interval"),
                     help = "Event interval")
args <- parse_args(parser)

# Subsetting cohort data

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

# 

cohort_popup <- function(data, cohort_data, start, end, interval){
  open_popup_cohort <- data.frame()
  
  start_end_diff <- as.integer(difftime(as.Date(end), as.Date(start), units = "days"))
  
  for (i in seq(as.Date(start), by = "day", length.out = start_end_diff + 1)){
    
    coh_data <- cohort_data %>%
      filter(as.Date(tsInstall) == as.Date(i, origin = "1970-01-01"))
    
    new_data <- data %>%
      filter(idDevice %in% coh_data$idDevice,
             eventName == "openPopup") %>%
      mutate(install_interval = tsInstall + days(interval)) %>%
      filter((tsEvent <= install_interval) == T) %>%
      count(idDevice, params.value)
    
    open_popup_cohort <- rbind(open_popup_cohort,
                               (new_data %>%
                                  group_by(params.value) %>%
                                  summarise(cohort_mean = sum(n) / nrow(coh_data))))
  }
  
  open_popup_cohort <- open_popup_cohort %>%
    group_by(params.value) %>%
    summarise(cohort_avg = mean(cohort_mean))
  
  return(open_popup_cohort)
}

# main function to create a table

main <- function(args){
  base <- sub(".csv", "", basename(args$infile))
  
  data <- read.csv(args$infile) %>%
    mutate(tsInstall = as.POSIXct(tsInstall, origin = "1970-01-01"),
           tsEvent = as.POSIXct(tsEvent, origin = "1970-01-01")) %>%
    arrange(idDevice, tsEvent)
  
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
  
  open_popup_first_cohort <- cohort_popup(data = data,
                                          cohort_data = first_cohort,
                                          start = args$fc_start,
                                          end = args$fc_end,
                                          interval = args$interval) %>%
    rename(first_cohort_avg = cohort_avg)
  
  open_popup_second_cohort <- cohort_popup(data = data,
                                           cohort_data = second_cohort,
                                           start = args$sc_start,
                                           end = args$sc_end,
                                           interval = args$interval) %>%
    rename(second_cohort_avg = cohort_avg)
  
  open_popup_cohorts <- full_join(open_popup_first_cohort,
                                  open_popup_second_cohort,
                                  by = "params.value") %>%
    mutate(diff = second_cohort_avg - first_cohort_avg)
  
  write.xlsx(open_popup_cohorts, paste(args$outdir, "/", base, ".cohort_popup_stats.xlsx", sep = "", collapse = ""))
}

main(args)



