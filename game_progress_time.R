library(data.table)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
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
parser <- add_option(parser, c("-m", "--mode"),
                     help = "Whether to calculate time difference between events or events and install timestamp")
parser <- add_option(parser, c("-u", "--units"),
                     help = "Units to calculate time difference in")
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

event_time_diff <- function(data, cohort_users, mode, units) {
  data <- data %>%
    filter(idDevice %in% cohort_users) %>%
    mutate(params.value = as.numeric(str_remove_all(params.value, "\\D+"))) %>%
    rename(`00` = tsInstall) %>% 
    spread(params.value, tsEvent) %>%
    select(-duplicate, -eventName, -idCountryISOAlpha2, -idDevice)
  
  new_col_names <- c()
  
  for (i in colnames(data)) {
    if (str_length(i) == 1) {
      i <- str_c("0", i, sep = "")
    }
    
    new_col_names <- c(new_col_names, i)
  }
  
  colnames(data) <- new_col_names
  
  data <- data %>%
    select(sort(current_vars()))
  
  for (i in colnames(select(data, -`00`))) {
    data <- data %>%
      mutate(!!paste0("diff",i) := difftime(!!sym(i),
                                            if (mode == "ie") {
                                              `00`
                                            } else {
                                              !!sym(ifelse(as.numeric(i) - 1 < 10,
                                                           paste0("0", as.character(as.numeric(i) - 1)),
                                                           as.character(as.numeric(i) - 1)))
                                            },
                                            units = units))
  }
  
  data <- data %>%
    select(starts_with("diff"))
  
  block_time <- data.frame(block = character(),
                           mean_time = POSIXct(),
                           median_time = POSIXct())
  
  for (i in colnames(data)) {
    block_stats <- data %>%
      select(starts_with(i)) %>%
      rename(block = !!sym(i))
    
    block_table <- data.frame(block = i,
                              mean_time = mean(block_stats$block, na.rm = T),
                              median_time = median(block_stats$block, na.rm = T))
    
    block_time <- rbind(block_time,
                        block_table)
  }
  
  block_time <- block_time %>%
    mutate(block = as.numeric(str_remove(block, "diff")))
  
  return(block_time)
}


main <- function(args){
  base <- sub(".csv", "", basename(args$infile))
  
  data <- fread(args$infile) %>%
    mutate(tsInstall = as.POSIXct(tsInstall, origin = "1970-01-01"),
           tsEvent = as.POSIXct(tsEvent, origin = "1970-01-01")) %>%
    arrange(idDevice, tsEvent) %>%
    select(idCountryISOAlpha2, tsInstall, idDevice, eventName, params.value, tsEvent) %>%
    filter(eventName == "maxOpenBlock")
  
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
                                                  cohort_users = first_cohort$idDevice,
                                                  mode = args$mode,
                                                  units = args$units) %>%
    rename(mean_time_first_cohort = mean_time,
           median_time_first_cohort = median_time)
  
  event_time_diff_second_cohort <- event_time_diff(data = data,
                                                   cohort_users = second_cohort$idDevice,
                                                   mode = args$mode,
                                                   units = args$units) %>%
    rename(mean_time_second_cohort = mean_time,
           median_time_second_cohort = median_time)
  
  game_complete_time <- full_join(event_time_diff_first_cohort,
                                  event_time_diff_second_cohort,
                                  by = "block") %>%
    mutate(mean_diff = mean_time_second_cohort - mean_time_first_cohort,
           median_diff = median_time_second_cohort - median_time_first_cohort)
  
  write.xlsx(game_complete_time, paste(args$outdir, "/", base, ".game_complete_time.xlsx", sep = "", collapse = ""))
}

main(args)

