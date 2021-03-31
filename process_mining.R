library(bupaR)
library(data.table)
library(dplyr)
library(stringr)
library(pmap)
library(zoo)
library(openxlsx)
library(optparse)

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"),
                     help = "Input file")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help = "Output directory")
parser <- add_option(parser, c("-c", "--cohort_date"),
                     help = "Cohort date")
parser <- add_option(parser, c("-f", "--first_event"),
                     help = "From what event to create process map")
parser <- add_option(parser, c("-s", "--second_event"),
                     help = "To what event to create process map")
args <- parse_args(parser)

create_eventlog <- function(data) {
  eventlog <- data %>%
    filter(eventName %in% c("chestItemAdd", "chestItemSold", "maxOpenBlock", "openPopup", "showAdsClicked")) %>% 
    mutate(event = ifelse(eventName %in% c("chestItemAdd", "chestItemSold"),
                          paste(eventName, params.name, sep = " "),
                          paste(eventName, params.value, sep = " "))) %>%
    mutate(event = str_remove_all(gsub("\\[|\\]", "", event), "'")) %>%
    select(timestamp = tsEvent,
           case_id = idDevice,
           activity = event) %>%
    mutate(case_id = as.character(case_id)) %>%
    na.omit()
  
  return(eventlog)
}

main <- function(args) {
  data <- fread(args$infile) %>%
    mutate(tsInstall = as.POSIXct(tsInstall, origin = "1970-01-01"),
           tsEvent = as.POSIXct(tsEvent, origin = "1970-01-01")) %>%
    arrange(idDevice, tsEvent) %>%
    mutate(gameStage = str_remove_all(gsub("\\[|\\]", "", na.locf(ifelse(eventName == "maxOpenBlock",
                                                                                    params.value,
                                                                                    NA))), "'")) %>%
    
    filter(as.Date(tsInstall) == args$cohort_date,
           as.numeric(gameStage) %in% seq(from = args$first_event, to = args$second_event))
  
  eventlog <- create_eventlog(data)
  
  process_mining_map <- create_pmap(eventlog)
  
  process_mining_map %>%
    render_pmap_shiny(title = paste0("Process map between ", args$first_event, " and ", args$second_event, " blocks ",
                                     "(cohort ", args$cohort_date, ")"),
                      nodes_prune_percentage = 0.25,
                      edges_prune_percentage = 0.75)
}

main(args)

