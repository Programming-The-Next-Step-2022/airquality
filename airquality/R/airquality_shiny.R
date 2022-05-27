# Shiny App ---------------------------------------------------------------
library(shiny)
library(airquality)
library(plotly)
library(gt)

ui <- fluidPage(

  tabsetPanel(


    tabPanel("Enter your city",
             textInput(inputId = "city_in",
                       label = "City",
                       width = 200
             ),
             textInput(inputId = "country_in",
                       label = "Country",
                       width = 200
             )
    ),

    tabPanel("Current AQ",
             gt_output(outputId = "current_aq_df")
    ),

    tabPanel("AQI History",
             plotOutput(outputId = "plot_aqi_hist",
                        width = 1200
             )
    ),

    tabPanel("AQ Subcomponent History",
             selectInput(inputId = "comp_in",
                         label = "Subcomponent",
                         choices = c("co", "no", "no2", "o3",
                                     "so2", "pm2_5", "pm10", "nh3" ),
                         width = 200),

             plotlyOutput(outputId = "plot_comp_hist",
                          width = 1200)
    ),

    tabPanel("Current AQI Europe",
             gt_output(outputId = "current_aq_table")
             ),

    tabPanel("Current Weather",
             gt_output(outputId = "current_weather")
             )


  )

)


server <- function(input, output) {

  output$current_aq_df <- render_gt({

    gt(current_aq_df(input$city_in, input$country_in))

  })

  output$plot_aqi_hist <- renderPlot({

    plot_aqi_hist(input$city_in, input$country_in)

  })

  output$plot_comp_hist <- renderPlotly({

    plot_comp_hist(input$city_in, input$country_in, input$comp_in)

  })

  output$current_aq_table <- render_gt({

    current_aq_table()

  })

  output$current_weather <- render_gt({

    current_weather()

  })



}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
