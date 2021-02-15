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
parser <- add_option(parser, c("-v", "--interval"),
                     help = "Event interval")
args <- parse_args(parser)

# Subsetting cohort data

cohort_subset <- function(data, start, end, country){
  cohort <- data %>%
    filter(as.Date(tsEvent) >= start,
           as.Date(tsEvent) <= end,
           idCountryISOAlpha2 == country)
  
  return(cohort)
}

# Function to compute avg game progress for first n-days in the game

cohort_progress <- function(data, cohort_data, start, end, interval){
  game_progress_cohort <- data.frame()
  
  start_end_diff <- as.integer(difftime(as.Date(end), as.Date(start), units = "days"))
  
  for (i in seq(as.Date(start), by = "day", length.out = start_end_diff + 1)){
    
    coh_data <- cohort_data %>%
      filter(as.Date(tsEvent) == as.Date(i, origin = "1970-01-01"))
    
    game_progress_cohort <- rbind(game_progress_cohort,
                                  (data %>%
                                     filter(idDevice %in% coh_data$idDevice,
                                            eventName == "maxOpenBlock",
                                            as.Date(tsEvent) %in% seq(as.Date(i, origin = "1970-01-01"), by = "day",
                                                                      length.out = as.numeric(interval))) %>%
                                     distinct(idDevice, params.value) %>%
                                     count(params.value) %>%
                                     arrange(-n) %>%
                                     mutate(prop = round(n / nrow(coh_data) * 100, 2))))
  }
  
  game_progress_cohort <- game_progress_cohort %>%
    group_by(params.value) %>%
    summarise(cohort_avg = round(mean(prop), 2)) %>%
    mutate(params.value = as.numeric(str_remove_all(params.value, "\\D+"))) %>% 
    arrange(params.value)
  
  return(game_progress_cohort)
}

# main function to create a table

main <- function(args){
  base <- sub(".csv", "", basename(args$infile))
  
  data <- read.csv(args$infile) %>%
    mutate(tsEvent = as.POSIXct(tsEvent, origin = "1970-01-01")) %>%
    arrange(idDevice, tsEvent)
  
  cohorts <- data %>%
    group_by(idDevice) %>%
    slice(1) %>%
    ungroup() %>%
    filter(eventName == "maxOpenBlock",
           params.value == "['1']") %>%
    select(idCountryISOAlpha2, idDevice, tsEvent)
  
  first_cohort <- cohort_subset(data = cohorts,
                                start = args$fc_start,
                                end = args$fc_end,
                                country = args$fc_country)
  
  second_cohort <- cohort_subset(data = cohorts,
                                 start = args$sc_start,
                                 end = args$sc_end,
                                 country = args$sc_country)
  
  game_progress_first_cohort <- cohort_progress(data = data,
                                                cohort_data = first_cohort,
                                                start = args$fc_start,
                                                end = args$fc_end,
                                                interval = args$interval) %>%
    rename(first_cohort_avg = cohort_avg)
  
  game_progress_second_cohort <- cohort_progress(data = data,
                                                 cohort_data = second_cohort,
                                                 start = args$sc_start,
                                                 end = args$sc_end,
                                                 interval = args$interval) %>%
    rename(second_cohort_avg = cohort_avg)
  
  cohort_game_progress <- full_join(game_progress_first_cohort,
                                    game_progress_second_cohort,
                                    by = "params.value") %>%
    mutate(diff = second_cohort_avg - first_cohort_avg)
  
  write.xlsx(cohort_game_progress, paste(args$outdir, "/", base, ".cohort_game_progress.xlsx", sep = "", collapse = ""))
}  


main(args)




