library(httr)
library(jsonlite)

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
  
  geocode_dat[1 , c(3,4)]

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
  
  dat <- GET(url)
  dat_1 <- rawToChar(dat$content)
  dat_2 <- fromJSON(dat_1)
  dat_2$list[[1]]
}



