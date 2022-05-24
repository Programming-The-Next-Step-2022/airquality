#' @import httr
#' @import jsonlite
#' @import ggplot2
#' @import dplyr
#' @import lubridate
NULL

#' Geocoding
#'
#' This function gives the coordinates of a given city. It used for further functions which require coordinates instead of city names.
#'
#' @param city City (in quotation marks)
#' @param country Country Code (according to ISO 3166; e.g.: NL for Netherlands; in quotation marks)
#'
#' @return Coordinates (latitude, longitude)
#'
#' @examples geocoding("Amsterdam", "NL")
#'
#' @details It is important to enter the city and the country.
#'
#' @export
geocoding <- function(city = "Amsterdam", country = "NL"){

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
  return(geocode_dat <- fromJSON(geocode_char))

}

#' Current Airquality Data
#'
#' With this function you can get the current air quality data of any town.
#'
#' @param city City (in quotation marks)
#' @param country Country Code (according to ISO 3166; e.g.: NL for Netherlands; in quotation marks)
#'
#' @return The airquality index (aqi) and the subcomponents (co, no, no2, o3, so2, pm2_5, pm10, nh3) in a list object.
#'
#' @examples current_aq_list("Amsterdam", "NL")
#'
#' @details The airquality index ranges from 1 = Good to 5 = Very Poor.
#'
#' @export
current_aq_list <- function(city = "Amsterdam", country = "NL"){

  coordinates <- geocoding(city, country)

  #current airquality
  base <- 'http://api.openweathermap.org/data/2.5/'
  which <- 'air_pollution?'
  lat <- 'lat='
  lat_number <- coordinates[1,3]
  lon <- '&lon='
  lon_number <- coordinates[1,4]
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

  return(current_aq)
}

#' Two Week Airquality Data
#'
#' With this function you can get the air quality data of any town for the last two weeks.
#'
#' @param city City (in quotation marks)
#' @param country Country Code (according to ISO 3166; e.g.: NL for Netherlands; in quotation marks)
#'
#' @return The airquality index (aqi), the subcomponents (co, no, no2, o3, so2, pm2_5, pm10, nh3), and the time stamps in a list object.
#'
#' @examples history_aq_list("Amsterdam", "NL")
#'
#' @details The airquality index ranges from 1 = Good to 5 = Very Poor.
#'
#' @export
history_aq_list <- function(city = "Amsterdam", country = "NL"){

  coordinates <- geocoding(city, country)

  #get time stamps
  current_time <- Sys.time()
  current_time_unix <- as.numeric(as.POSIXct(current_time))
  current_time_unix_r <- round(current_time_unix,0)


  two_weeks_back <- Sys.time() - weeks(2)
  two_weeks_back_unix <- as.numeric(as.POSIXct(two_weeks_back))
  two_weeks_back_unix_r <- round(two_weeks_back_unix, 0)


  #get history aq data
  hist_aq_url_1 <- "http://api.openweathermap.org/data/2.5/air_pollution/history?lat="
  hist_aq_url_2 <- "&lon="
  hist_aq_url_3 <- "&start="
  hist_aq_url_4 <- "&end="
  hist_aq_url_5 <- "&appid="
  appid <- '03782ca206139ca19d564d33c2813127'

  hist_aq_url <- paste0(hist_aq_url_1,
                        coordinates[1,3],
                        hist_aq_url_2,
                        coordinates[1,4],
                        hist_aq_url_3,
                        two_weeks_back_unix_r,
                        hist_aq_url_4,
                        current_time_unix_r,
                        hist_aq_url_5,
                        appid)

  hist_aq_raw <- GET(hist_aq_url)
  hist_aq_char <- rawToChar(hist_aq_raw$content)
  hist_aq_dat <- fromJSON(hist_aq_char)

  return(hist_aq_dat)

}

#' Airquality Subcomponents Plot (past two weeks)
#'
#' With this function you can plot each of the subcomponents for the airquality of the last two weeks.
#'
#' @param city City (in quotation marks)
#' @param country Country Code (according to ISO 3166; e.g.: NL for Netherlands; in quotation marks)
#' @param component The subcomponent you wish to plot
#'
#'
#' @return A plot of the airquality subcomponent over the past two weeks.
#'
#' @examples plot_comp_hist("Amsterdam", "NL", "co")
#'
#' @details
#'
#' @export
plot_comp_hist <- function(city = "Amsterdam", country = "NL", component = "co"){

  data <- history_aq_list(city, country)
  hist_comp_df <- data.frame(data[[2]][[2]], data[[2]][[3]])

  hist_comp_df$time <- as_datetime(hist_comp_df$data..2....3..)

  hist_comp_df %>%
    ggplot(aes(x = time, y = .data[["co"]])) +
    geom_line(color = "deepskyblue3") +

    scale_x_datetime(labels = scales::date_format("%Y-%m-%d "),
                     date_breaks = "24 hours") +
    ggtitle(paste0(component, " concentration in ", city, " over the past two weeks")) +
    xlab("Time") +
    ylab(paste0(component, " concentration")) +

    theme_classic() +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          plot.title = element_text(hjust = 0.5))

}

#' Airquality Index Plot (past two weeks)
#'
#' With this function you can plot  the airquality index of the last two weeks.
#'
#' @param city City (in quotation marks)
#' @param country Country Code (according to ISO 3166; e.g.: NL for Netherlands; in quotation marks)
#'
#'
#' @return A plot of the airquality index over the past two weeks.
#'
#' @examples plot_comp_hist("Amsterdam", "NL")
#'
#' @details
#'
#' @export
plot_aqi_hist <- function(city = "Amsterdam", country = "NL"){

  data <- history_aq_list(city, country)
  hist_aqi_df <- data.frame(data[[2]][[1]], data[[2]][[3]])

  hist_aqi_df$time <- as_datetime(hist_aqi_df$data..2....3..)


  hist_aqi_df %>%
    ggplot(aes(x = time, y = aqi)) +
    geom_line(color = "deepskyblue3") +
    scale_x_datetime(labels = scales::date_format("%Y-%m-%d "),
                     date_breaks = "24 hours") +
    ylim(1,5) +
    ggtitle(paste0("Airquality Index of ", city ," over the past two weeks")) +
    xlab("Time") +
    ylab("Airquality Index") +

    theme_classic() +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          plot.title = element_text(hjust = 0.5))
}






