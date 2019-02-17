library(magrittr)
library(rvest)
library(tidyverse)
library(stringr)

setwd("C:\\Users\\pistachio\\Projects\\web-scrape\\fhwa")
url_stem <- "https://www.fhwa.dot.gov/"
web_text <- read_html(paste0(url_stem, "policyinformation/travel_monitoring/tvt.cfm"))

url_monthly <- web_text %>%
  html_nodes(xpath = '//*[@class="tvt"]/table') %>%  # select all tables under div[@class="tvt"]
  html_nodes("a") %>% html_attr("href")  # grab url references
#url_monthly 

# remove xls and pdf url references 
url_monthly <- url_monthly[!str_detect(url_monthly, "(\\.)xls|pdf")]

# grab only year month portion for url pasting
url_monthly <- str_extract(url_monthly, "[0-9]{2}[a-z]{3}tvt/")
url_monthly <- url_monthly[!is.na(url_monthly)]

# url stem changes for links 11jul and earlier
url_change1_index <- str_which(url_monthly, "11jul")
url_monthly[1:(url_change1_index-1)] <- paste0("policyinformation/travel_monitoring/", url_monthly[1:(url_change1_index-1)])
url_monthly[url_change1_index:length(url_monthly)] <- paste0("ohim/tvtw/", url_monthly[url_change1_index:length(url_monthly)])

# .cfm changes to .htm for links 08jan and earlier
url_change2_index <- str_which(url_monthly, "08jan")

# page 6 does not exist for 2003 and earlier
url_change3_index <- str_which(url_monthly, "03dec")

# create empty table
df_all <- data.frame(
  source = character(),
  state = character(),
  current_stations = numeric(),
  current_miles = numeric(),
  prior_stations = numeric(),
  prior_miles = numeric()
)
  
for (i in 1:(url_change3_index-1)) {
  
  month_i = url_monthly[i]
  
  if (i < url_change2_index) {
    url_i <- paste0(url_stem, month_i, "page6.cfm")
    pg <- read_html(url_i) 
    tbl <- pg %>% html_node(xpath = '//table[contains(@class,"datatable")]') %>% html_table(fill= TRUE)
  } else if (i == url_change2_index) {
    url_i <- paste0(url_stem, month_i, "page6.htm")
    pg <- read_html(url_i) 
    tbl <- pg %>% html_node(xpath = '/html/body/table[2]') %>% html_table(fill= TRUE)
  } else {
    url_i <- paste0(url_stem, month_i, "page6.htm")
    pg <- read_html(url_i) 
    tbl <- pg %>% html_node(xpath = '//table[contains(@id,"Table1")]') %>% html_table(fill= TRUE)
  }

  tbl_clean <- tbl[, c(1:3,6:7)]  # select curr and prior month [number of stations] and [vehicle miles]
  names(tbl_clean) <- c("state", "current_stations", "current_miles", "prior_stations", "prior_miles")
  
  # replace anything that is not a letter or space with a space
  tbl_clean$state <- gsub("[^[:alpha:] ]", " ", tbl_clean$state) 
  
  # remove anything that is not a digit (need to remove commas)
  tbl_clean$current_stations <- as.numeric(gsub("[^[:digit:]]", "", tbl_clean$current_stations))
  tbl_clean$current_miles <- as.numeric(gsub("[^[:digit:]]", "", tbl_clean$current_miles))
  tbl_clean$prior_stations <- as.numeric(gsub("[^[:digit:]]", "", tbl_clean$prior_stations))
  tbl_clean$prior_miles <- as.numeric(gsub("[^[:digit:]]", "", tbl_clean$prior_miles))
  
  # remove subtotal and total rows
  tbl_clean <- tbl_clean %>% 
    filter(!is.na(current_miles) & !is.na(prior_miles),
           !str_detect(tolower(state), "total"),
           !str_detect(tolower(state), "region"),
           str_detect(tolower(state), "[a-z]")) %>%
    mutate(source = str_extract(month_i, "[0-9]{2}[a-z]{3}tvt"))  # year month tvt
  
  # append data
  df_all <- bind_rows(df_all, tbl_clean)
  
  # print progress
  print(paste0("Pulled in: ", month_i))
  flush.console()
}

write_csv(df_all, "tvt_all_estimated_roads_by_state.csv")
