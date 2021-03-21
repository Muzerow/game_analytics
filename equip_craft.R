library(dplyr)
library(readr)
library(stringr)
library(openxlsx)
library(optparse)
library(tibble)
library(tidyr)

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
parser <- add_option(parser, c("-t", "--item_type"),
                     help = "Type of item to calculate")
parser <- add_option(parser, c("-x", "--block_reached"),
                     help = "Count proportion on users reached block")
parser <- add_option(parser, c("-y", "--button_crafted"),
                     help = "Button to calculate statsistics for")
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

item_create <- function(data, cohort_data, item, button) {
  coh_data <- data %>%
    filter(idDevice %in% cohort_data$idDevice,
           (str_detect(params.name, paste(item, "Helmet")) == T | str_detect(params.name, paste(item, "Chestplate")) == T |
              str_detect(params.name, paste(item, "Leggings")) == T | str_detect(params.name, paste(item, "Boots")) == T |
              str_detect(params.name, paste(item, "Pickaxe")) == T | str_detect(params.name, paste(item, "Sword")) == T |
              str_detect(params.name, button) == T)) %>%
    distinct(idDevice, params.name) %>%
    count(idDevice, params.name) %>%
    spread(params.name, n) %>%
    select(-idDevice) %>%
    mutate_all(funs(replace_na(.,0))) %>%
    mutate(total = rowSums(.)) %>%
    mutate(one_item = ifelse(total - !!sym(paste0("['",button,"']")) >= 1, 1, 0),
           sword_and_pickaxe = ifelse(!!sym(paste0("['",item," ","Sword","']")) == 1 &
                                        !!sym(paste0("['",item," ","Pickaxe","']")) == 1 &
                                        total - !!sym(paste0("['",button,"']")) == 2, 1, 0),
           pickaxe = ifelse(!!sym(paste0("['",item," ","Pickaxe","']")) == 1 &
                              total - !!sym(paste0("['",button,"']")) == 1, 1, 0),
           sword = ifelse(!!sym(paste0("['",item," ","Sword","']")) == 1 &
                            total - !!sym(paste0("['",button,"']")) == 1, 1, 0),
           one_equip = ifelse((total - !!sym(paste0("['",item," ","Pickaxe","']")) -
                                 !!sym(paste0("['",item," ","Sword","']")) - !!sym(paste0("['",button,"']"))) == 1, 1, 0),
           two_equip = ifelse((total - !!sym(paste0("['",item," ","Pickaxe","']")) -
                                 !!sym(paste0("['",item," ","Sword","']")) - !!sym(paste0("['",button,"']"))) == 2, 1, 0),
           three_equip = ifelse((total - !!sym(paste0("['",item," ","Pickaxe","']")) -
                                   !!sym(paste0("['",item," ","Sword","']")) - !!sym(paste0("['",button,"']"))) == 3, 1, 0),
           full_equip = ifelse((total - !!sym(paste0("['",item," ","Pickaxe","']")) -
                                  !!sym(paste0("['",item," ","Sword","']")) - !!sym(paste0("['",button,"']"))) == 4, 1, 0),
           full_equip_and_tools = ifelse(total - !!sym(paste0("['",button,"']")) == 6, 1, 0),
           button = !!sym(paste0("['",button,"']"))) %>%
    select(-!!sym(paste0("['",item," ","Pickaxe","']")), -!!sym(paste0("['",item," ","Sword","']")),
           -!!sym(paste0("['",item," ","Helmet","']")), -!!sym(paste0("['",item," ","Chestplate","']")),
           -!!sym(paste0("['",item," ","Leggings","']")), -!!sym(paste0("['",item," ","Boots","']")),
           -!!sym(paste0("['",button,"']")), -total)
  
  item_cohort_data <- as.data.frame(colSums(coh_data)) %>%
    mutate(prop = `colSums(coh_data)` / nrow(cohort_data)) %>%
    rownames_to_column()
  
  return(item_cohort_data)
}

main <- function(args){
  base <- sub(".csv", "", basename(args$infile))
  
  data <- read.csv(args$infile) %>%
    mutate(tsInstall = as.POSIXct(tsInstall, origin = "1970-01-01"),
           tsEvent = as.POSIXct(tsEvent, origin = "1970-01-01")) %>%
    arrange(idDevice, tsEvent)
  
  cohorts <- data %>%
    select(idCountryISOAlpha2, idDevice, tsInstall, eventName, params.value) %>%
    distinct(idCountryISOAlpha2, idDevice, tsInstall, eventName, params.value)
  
  if (args$block_reached != "None") {
    cohorts <- cohorts %>%
      filter(eventName == "maxOpenBlock",
             params.value == paste0("['",as.character(args$block_reached),"']"))
  }
  
  cohorts <- cohorts %>%
    distinct(idCountryISOAlpha2, idDevice, tsInstall)
  
  first_cohort <- cohort_subset(data = cohorts,
                                start = args$fc_start,
                                end = args$fc_end,
                                country = args$fc_country)
  
  second_cohort <- cohort_subset(data = cohorts,
                                 start = args$sc_start,
                                 end = args$sc_end,
                                 country = args$sc_country)
  
  item_create_first_cohort <- item_create(data = data,
                                          cohort_data = first_cohort,
                                          item = args$item_type,
                                          button = args$button_crafted) %>%
    rename(n_first_cohort = `colSums(coh_data)`,
           prop_first_cohort = prop)
  
  item_create_second_cohort <- item_create(data = data,
                                           cohort_data = second_cohort,
                                           item = args$item_type,
                                           button = args$button_crafted) %>%
    rename(n_second_cohort = `colSums(coh_data)`,
           prop_second_cohort = prop)
  
  item_stats <- full_join(item_create_first_cohort,
                          item_create_second_cohort,
                          by = "rowname") %>%
    rename(items_made = rowname)
  
  item_stats %>%
    write.xlsx(paste(args$outdir, "/", base, ".equip_craft.xlsx", sep = "", collapse = ""))
}

main(args)

