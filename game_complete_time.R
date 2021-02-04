
library(dplyr)
library(readr)
library(stringr)
library(optparse)

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"),
                     help = "Input file")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help = "Output directory")
args <- parse_args(parser)


main <- function(args){
  base <- sub(".csv", "", basename(args$infile))
  
  data <- read.csv(args$infile) %>%
    mutate(tsEvent = as.POSIXct(tsEvent, origin = "1970-01-01")) %>%
    arrange(idDevice, tsEvent) %>%
    select(idDevice, eventName, params.value, tsEvent) %>%
    filter(eventName == "maxOpenBlock",
           params.value %in% c("['1']", "['29']"))
  
  data$duplicate <- duplicated(data[,c("idDevice", "params.value")])
  
  data <- data %>%
    filter(duplicate == FALSE)
  
  time_to_reach_block <- data.frame()
  
  for (i in unique(data$idDevice)) {
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
  
  write.csv(time_to_reach_block, paste(args$outdir, "/", base, ".game_complete_time.csv", sep = "", collapse = ""))
}

main(args)

