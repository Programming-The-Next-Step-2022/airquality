# Shiny App ---------------------------------------------------------------
library(shiny)

ui <- fluidPage(

  titlePanel("Air Quality"),

  sidebarLayout(

    sidebarPanel(

      numericInput(inputId = "lat_in",
                   label = "Enter Latitude",
                   value = "",
                   width = 400

      ),

      numericInput(inputId = "long_in",
                   label = "Enter Longitude",
                   value = "",
                   width = 400

      )
    ),

    mainPanel(
      tableOutput(outputId = "aqi_out")
    )
  )
)

server <- function(input, output) {

  output$aqi_out <- renderTable({

    base <- 'http://api.openweathermap.org/data/2.5/'
    which <- 'air_pollution?'
    lat <- 'lat='
    lon <- '&lon='
    appid <- '&appid=03782ca206139ca19d564d33c2813127'

    url <- paste0(base,
                  which,
                  lat,
                  input$lat_in,
                  lon,
                  input$long_in,
                  appid
    )

    dat <- GET(url)
    dat_1 <- rawToChar(dat$content)
    dat_2 <- fromJSON(dat_1)
    dat_2$list[[1]]
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
