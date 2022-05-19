library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)


#' @export
current_aq <- function(city, country){
  
  #geocoding
  geocode_base <- 'http://api.openweathermap.org/geo/1.0/direct?q='
  geocode_limit <- '&limit=1&appid='
  geocode_appid <- '03782ca206139ca19d564d33c2813127'
  
  geocode_url <- paste0(
    geocode_base,
    city,
    ",",
    country,
    geocode_limit,
    geocode_appid
  )
  
  geocode_raw <- GET(geocode_url)
  geocode_char <- rawToChar(geocode_raw$content)
  geocode_dat <- fromJSON(geocode_char)
  
  #print(geocode_dat[1 , c(3,4)])

  #current airquality
  base <- 'http://api.openweathermap.org/data/2.5/'
  which <- 'air_pollution?'
  lat <- 'lat='
  lat_number <- geocode_dat[1,3]
  lon <- '&lon='
  lon_number <- geocode_dat[1,4]
  appid <- '&appid=03782ca206139ca19d564d33c2813127'
  
  url <- paste0(base,
                which,
                lat,
                lat_number,
                lon,
                lon_number,
                appid
  )
  
  current_aq_raw <- GET(url)
  current_aq_char <- rawToChar(current_aq_raw$content)
  current_aq <- fromJSON(current_aq_char)
  
  #converting to data frame
  current_aqi_df <- current_aq[['list']][['main']]
  current_aq_comp_df <- current_aq[['list']][['components']]
  
  print(current_aqi_df)
  print(current_aq_comp_df)
  
  }


#' @export
history_aq <- function(city, country){
  
  #testing the function
  city <- "Amsterdam"
  country <- "NL"
  
  #geocoding
  geocode_base <- 'http://api.openweathermap.org/geo/1.0/direct?q='
  geocode_limit <- '&limit=1&appid='
  geocode_appid <- '03782ca206139ca19d564d33c2813127'
  
  geocode_url <- paste0(
    geocode_base,
    city,
    ",",
    country,
    geocode_limit,
    geocode_appid
  )
  
  geocode_raw <- GET(geocode_url)
  geocode_char <- rawToChar(geocode_raw$content)
  geocode_dat <- fromJSON(geocode_char)
  
  lat_number <- geocode_dat[1,3]
  lon_number <- geocode_dat[1,4]
  
  #get time stamps
  current_time <- Sys.time()
  current_time_unix <- as.numeric(as.POSIXct(current_time))
  current_time_unix_r <- round(current_time_unix,0)
  
  
  two_weeks_back <- Sys.time() - weeks(2)
  two_weeks_back_unix <- as.numeric(as.POSIXct(two_weeks_back))
  two_weeks_back_unix_r <- round(two_weeks_back_unix, 0)
  

  #get history aq data
  hist_aq_url_1 <- "http://api.openweathermap.org/data/2.5/air_pollution/history?lat="
  lat_number 
  hist_aq_url_2 <- "&lon="
  lon_number
  hist_aq_url_3 <- "&start="
  hist_aq_url_4 <- "&end="
  hist_aq_url_5 <- "&appid="
  appid <- '03782ca206139ca19d564d33c2813127'
  
  hist_aq_url <- paste0(hist_aq_url_1,
                        lat_number,
                        hist_aq_url_2,
                        lon_number,
                        hist_aq_url_3,
                        two_weeks_back_unix_r,
                        hist_aq_url_4,
                        current_time_unix_r,
                        hist_aq_url_5,
                        appid)
  
  hist_aq_raw <- GET(hist_aq_url)
  hist_aq_char <- rawToChar(hist_aq_raw$content)
  hist_aq_dat <- fromJSON(hist_aq_char)
  
  #converting to data frame
  hist_aqi_df <- hist_aq_dat[['list']][['main']]
  hist_comp_df <- hist_aq_dat[['list']][['components']]
  
  #plotting
  hist_comp_df$time <- seq(1:nrow(hist_comp_df))
  
  hist_comp_df %>% 
    ggplot(aes(x = time, y = co)) +
    geom_line()
}





