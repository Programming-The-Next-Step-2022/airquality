# Shiny App ---------------------------------------------------------------
library(shiny)

ui <- fluidPage(

  titlePanel("Air Quality"),

  sidebarLayout(

    sidebarPanel(

      textInput(inputId = "city_in",
                label = "City",
                width = 200

      ),

      textInput(inputId = "country_in",
                label = "Country",
                width = 200

      )
    ),

    mainPanel(
      plotOutput(outputId = "current_aq")
    )
  )
)


server <- function(input, output) {

  output$current_aq <- renderPlot({

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

    plot_aqi_hist(input$city_in, input$country_in)


  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
