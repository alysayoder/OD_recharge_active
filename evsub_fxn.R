#function needs to query the event_pre dataframe to get the beginning and end of each event 
#subset discharge data to the length of each flooding event
library(lubridate)
library(dplyr)
library(here)
evsub <- function(d_start, d_end) {
  #get data
  mb <- read.csv(here::here("data", "mich_bar_cms_all.csv"), stringsAsFactors = FALSE)
  mb$Date <- as.POSIXct(mb$Date, format = "%m/%d/%Y %H:%M")
  mb %>% 
    select(Date, MiBarCMS) %>%
    filter(Date >= as.POSIXct(d_start) & Date <= as.POSIXct(d_end))
}