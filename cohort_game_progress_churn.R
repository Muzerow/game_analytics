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
parser <- add_option(parser, c("-w", "--days_churn"),
                     help = "Number of days inactive to be churn")
parser <- add_option(parser, c("-a", "--churn_date"),
                     help = "Date from which days churn are calculated")
args <- parse_args(parser)

# Subsetting cohorts

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

# Calculating how many users reached the block

progress_stats <- function(data, cohort_data) {
  block_reached <- data %>%
    filter(idDevice %in% cohort_data$idDevice,
           eventName == "maxOpenBlock") %>%
    distinct(idDevice, params.value) %>%
    count(params.value) %>%
    mutate(prop = n / nrow(cohort_data))
  
  return(block_reached)
}

# Main function

main <- function(args){
  base <- sub(".csv", "", basename(args$infile))
  
  data <- read.csv(args$infile) %>%
    mutate(tsInstall = as.POSIXct(tsInstall, origin = "1970-01-01"),
           tsEvent = as.POSIXct(tsEvent, origin = "1970-01-01")) %>%
    arrange(idDevice, tsEvent)
  
  cohorts <- data %>%
    group_by(idDevice) %>%
    mutate(last_event = as.Date(last(tsEvent))) %>%
    distinct(idCountryISOAlpha2, tsInstall, idDevice, last_event) %>%
    filter(difftime(as.Date(args$churn_date), last_event, units = "days") > as.numeric(args$days_churn)) %>%
    select(idCountryISOAlpha2, tsInstall, idDevice)
  
  first_cohort <- cohort_subset(data = cohorts,
                                start = args$fc_start,
                                end = args$fc_end,
                                country = args$fc_country)
  
  second_cohort <- cohort_subset(data = cohorts,
                                 start = args$sc_start,
                                 end = args$sc_end,
                                 country = args$sc_country)
  
  block_reached_first_cohort <- progress_stats(data = data,
                                               cohort_data = first_cohort) %>%
    rename(n_first_cohort = n,
           prop_first_cohort = prop)
  
  block_reached_second_cohort <- progress_stats(data = data,
                                                cohort_data = second_cohort) %>%
    rename(n_second_cohort = n,
           prop_second_cohort = prop)
  
  progress_count <- full_join(block_reached_first_cohort,
                              block_reached_second_cohort,
                              by = "params.value") %>%
    mutate(params.value = as.numeric(str_remove_all(params.value, "\\D+"))) %>% 
    arrange(params.value) %>%
    rename(block_num = params.value)
  
  write.xlsx(progress_count, paste(args$outdir, "/", base, ".cohort_progress_count.xlsx", sep = "", collapse = ""))
}

main(args)




