library(dplyr)
library(openxlsx)
library(xlsx)
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
parser <- add_option(parser, c("-d", "--day_churn"),
                     help = "Days with no activity to be treated as churned")
args <- parse_args(parser)

day_churned <- function(data, cohort_start, cohort_end, days_churn) {
  day_churned_users <- data %>%
    filter(as.Date(tsInstall) >= cohort_start,
           as.Date(tsInstall) <= cohort_end) %>%
    group_by(idDevice) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    mutate(day_churned = as.numeric(difftime(as.Date(tsEvent), as.Date(tsInstall), units = "days"))) %>%
    filter(difftime(Sys.Date(), as.Date(tsEvent), units = "days") > days_churn) %>%
    select(idDevice, day_churned)
}

main <- function(args) {
  base <- sub(".csv", "", basename(args$infile))
  
  data <- read.csv(args$infile) %>%
    mutate(tsInstall = as.POSIXct(tsInstall, origin = "1970-01-01"),
           tsEvent = as.POSIXct(tsEvent, origin = "1970-01-01")) %>%
    arrange(idDevice, tsEvent)
  
  day_churned_users <- day_churned(data = data,
                                   cohort_start = as.Date(args$fc_start),
                                   cohort_end = as.Date(args$fc_end),
                                   days_churn = as.numeric(args$day_churn))
  
  churn_structure <- createWorkbook()
  sheet <- createSheet(churn_structure, sheetName = "Day Churned")
  
  day_churned_count <- day_churned_users %>%
    count(day_churned) %>%
    mutate(prop = n / nrow(day_churned_users)) %>%
    arrange(day_churned)
  
  write.xlsx(day_churned_count, paste0(args$outdir, "/", base, ".test.xlsx"))
  
  addDataFrame(day_churned_count, sheet)
  
  for (i in day_churned_count[1:15,]$day_churned) {
    sheet <- createSheet(churn_structure, sheetName = paste("Day", i, sep = " "))

    last_event_count <- data %>%
      filter(idDevice %in% filter(day_churned_users, day_churned == i)$idDevice) %>%
      group_by(idDevice) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      count(eventName, params.name, params.value) %>%
      mutate(prop = n / sum(n)) %>%
      arrange(-n)

    addDataFrame(last_event_count, sheet)
  }
  
  saveWorkbook(churn_structure, file = paste0(args$outdir, "/", base, ".churn_structure.xlsx"))
}

main(args)


