#' @import httr
#' @import jsonlite
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @import scales
#' @import plotly
#' @import gt
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

  #api url
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
  return(geocode_dat)

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
#' @examples current_aq_df("Amsterdam", "NL")
#'
#' @details The airquality index ranges from 1 = Good to 5 = Very Poor.
#'
#' @export
current_aq_df <- function(city = "Amsterdam", country = "NL"){

  coordinates <- geocoding(city, country)

  #api url
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

  #get api data
  current_aq_raw <- GET(url)
  current_aq_char <- rawToChar(current_aq_raw$content)
  current_aq <- fromJSON(current_aq_char)

  #create data frame
  current_aq_convert <- as.data.frame(current_aq)
  current_aq_df <- as.data.frame(t(current_aq_convert))

  #arrange columns
  current_aq_df$Component <- rownames(current_aq_df)
  current_aq_df <- current_aq_df[-c(1,2,12), c(2,1)]
  rownames(current_aq_df) <- seq(1:nrow(current_aq_df))

  #renaming
  current_aq_df$Component <- c("AQI", "CO", "NO", "NO2", "O3",
                               "SO2", "PM2_5", "PM_10", "NH3")

  colnames(current_aq_df) <- c("Component", "Index / Concentration")

  return(current_aq_df)

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

  comp_hist_plot <- hist_comp_df %>%
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

  ggplotly(comp_hist_plot)
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
plot_aqi_hist <- function(city = "Amsterdam", country = "NL") {

  data <- history_aq_list(city, country)
  hist_aqi_df <- data.frame(data[[2]][[1]], data[[2]][[3]])

  #converting time
  hist_aqi_df$time <- as_datetime(hist_aqi_df$data..2....3..)
  hist_aqi_df$time <- as.Date(hist_aqi_df$time)


  #averaging data to day level and adding color column
  hist_aqi_df <- hist_aqi_df %>%
    mutate(time = floor_date(time, unit = "day")) %>%
    group_by(time) %>%
    summarize(aqi = mean(aqi)) %>%
    mutate(aqi = round(aqi,0))

  #plotting
  hist_aqi_df %>%
    ggplot(aes(x = time, y = aqi, fill = factor(aqi))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("1" = "chartreuse2",
                                 "2" = "gold",
                                 "3" =" orange",
                                 "4" = "orangered1",
                                 "5" = "red4"),
                      labels = c("1 | Good", "2 | Fair", "3 | Moderate", "4 | Poor", "5 | Very Poor")) +
    labs(fill = "Airquality Index") +
    scale_x_date(labels = scales::date_format("%Y-%m-%d"),
                 date_breaks = "1 day",
                 expand = c(0, 0)) +

    ggtitle(paste0("Airquality Index of ", city ," over the past two weeks")) +
    xlab("Time") +
    ylab("Airquality Index") +
    ylim(c(0, 5)) +

    theme_classic() +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          plot.title = element_text(hjust = 0.5))

}


#' Current Airquality Index of Top 10 Major European Cities
#'
#' This function produces a table with the current airquality index (AQI) of the top 10 major European cities.
#'
#'
#'
#' @examples current_aq_table
#'
#'
#' @export
current_aq_table <- function(){


ist_data <- current_aq_df("Istanbul", "TUR")
lon_data <- current_aq_df("London", "GBR")
ber_data <- current_aq_df("Berlin", "GER")
mad_data <- current_aq_df("Madrid", "ESP")
kyi_data <- current_aq_df("Kyiv", "UKR")
rom_data <- current_aq_df("Rome", "ITA")
buc_data <- current_aq_df("Bucharest", "ROM")
par_data <- current_aq_df("Paris", "FRA")
vie_data <- current_aq_df("Vienna", "AUS")
ham_data <- current_aq_df("Hamburg", "GER")


Cities <- c("Istanbul", "London", "Berlin", "Madrid", "Kyiv",
            "Rome", "Bucharest", "Paris", "Vienna", "Hamburg")

AQI <- c(ist_data[1,2], lon_data[1,2], ber_data[1,2], mad_data[1,2],
         kyi_data[1,2], rom_data[1,2], buc_data[1,2], par_data[1,2],
         vie_data[1,2], ham_data[1,2])


cap_data <- data.frame(Cities, AQI)


#coloring cells
gt(cap_data) %>%
  tab_header("Current Airquality Index (AQI) of Top 10 Major Europan Cities") %>%

  tab_style(
    style = list(
      cell_fill(color = "chartreuse2")
    ),
    locations = cells_body(
      columns = AQI,
      rows = AQI == 1)
  ) %>%

  tab_style(
    style = list(
      cell_fill(color = "gold")
    ),
    locations = cells_body(
      columns = AQI,
      rows = AQI == 2)
  ) %>%

  tab_style(
    style = list(
      cell_fill(color = "orange")
    ),
    locations = cells_body(
      columns = AQI,
      rows = AQI == 3)
  ) %>%

  tab_style(
    style = list(
      cell_fill(color = "orangered1")
    ),
    locations = cells_body(
      columns = AQI,
      rows = AQI == 4)
  ) %>%

  tab_style(
    style = list(
      cell_fill(color = "red4"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = AQI,
      rows = AQI == 5)
  )

}





