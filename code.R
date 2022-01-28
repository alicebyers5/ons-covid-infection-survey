### Extract ONS Covid Infection Survey trend data

### 0 - Load packages ----

library(xml2)
library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(lubridate)
library(openxlsx)
library(tidyr)
library(magrittr)
library(here)


### 1 - Get links and dates for all available files ----

get_links <- function(webpage){

  x <- read_html(webpage)

  y <-
    html_attr(html_nodes(x, "a"), "href") %>%
    extract(str_detect(., ".xlsx"))

  paste0("https://www.ons.gov.uk", y)

}

webpage <- paste0(
  "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/",
  "conditionsanddiseases/datasets/covid19infectionsurveyscotland/",
  2021:2022
)

files <- c(get_links(webpage[1]), get_links(webpage[2]))

# Remove file published on 24-12-2021 - tab 1f missing
files %<>% extract(!str_detect(., "24122021"))


### 2 - Read files and extract relevant data ----

extract_data <- function(url){

  date <- url %>% str_extract("\\d{8}") %>% ymd()

  message("Running ", date)

  # Check sheet 1f exists
  x <- loadWorkbook(url)
  if(!"1f" %in% sheets(x)){stop("Sheet 1f doesn't exist")}

  var_names <- c("geography_code", "country", "local_authority_areas",
                 "perc_positive", "perc_lower_95_ci", "perc_upper_95_ci",
                 "ratio_positive", "ratio_lower_95_ci", "ratio_upper_95_ci")

  # Read sheet 1f from workbook
  wb <- readWorkbook(url, sheet = "1f")

  # Check whether data has been updated in this publication
  updated <- wb %>%
    filter(str_starts(Contents,
                      "The data in this table has not been updated")) %>%
    nrow()

  if(updated > 0){
    message("No updated data to extract.")
  }else{

    wb %>%
      filter(str_starts(Contents, "J06")) %>%
      set_names(var_names) %>%
      mutate_at(vars(starts_with("perc")), as.numeric) %>%
      mutate(pub_date = date,
             reporting_period = wb$Contents[3]) %>%
      separate(reporting_period,
               into = c("start_date", "end_date"),
               sep = " to ") %>%
      mutate_at(vars(start_date:end_date), ~ dmy(.)) %>%
      select(contains("date"), everything())

  }
}

trend <- map_dfr(files, extract_data) %>% arrange(pub_date)


### 3 - Save data file ----

write.xlsx(trend, here("ons-cis-trend.xlsx"))


### END OF SCRIPT ###