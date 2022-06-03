#' @import shiny
#' @import shinythemes
NULL

#' @export
airquality_app <- function(){

  #ui----
  ui <- fluidPage(theme = shinytheme("lumen"),
                  titlePanel("Airquality Widget"),
                  tabsetPanel(

                    #Home Tab ----
                    tabPanel(
                      title = "Home",
                      icon = icon("home"),

                      fluidRow(
                        br(),

                        column(h4("WHO 2022:"),
                               p("Air pollution is contamination of the indoor or outdoor environment by any chemical, physical or biological agent that modifies the natural characteristics of the atmosphere.Household combustion devices, motor vehicles, industrial facilities and forest fires are common sources of air pollution. Pollutants of major public health concern include particulate matter, carbon monoxide, ozone, nitrogen dioxide and sulfur dioxide."),
                               p("Outdoor and indoor air pollution cause respiratory and other diseases and are important sources of morbidity and mortality. WHO data show that almost all of the global population (99%) breathe air that exceeds WHO guideline limits and contains high levels of pollutants, with low- and middle-income countries suffering from the highest exposures."),
                               p("Air quality is closely linked to the earth's climate and ecosystems globally. Many of the drivers of air pollution (i.e. combustion of fossil fuels) are also sources of greenhouse gas emissions. Policies to reduce air pollution, therefore, offer a win-win strategy for both climate and health, lowering the burden of disease attributable to air pollution, as well as contributing to the near- and long-term mitigation of climate change."),
                               style="text-align:justify;color:black;background-color:
                             lightcyan;padding:15px;border-radius:10px",
                               width = 6),
                        column(img(src='https://images.unsplash.com/photo-1483825366482-1265f6ea9bc9?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=2070&q=80',
                                   width = "600",
                                   height = "320"),


                               width = 6

                        )

                      ),

                      fluidRow(
                        br(),
                        column(h4("This widget can:"),
                               tags$li("Retrieve the current airquality index and airquality subcomponents of any town worldwide"),
                               tags$li("Plot the airquality index and concentration of air quality components for the past two weeks of any town worldwide"),
                               tags$li("Retrieve and compare the airquality index of the ten biggest European cities"),
                               tags$li("Get the current weather data for any town worldwide"),
                               style="text-align:justify;color:black;background-color:
                             palegreen;padding:15px;border-radius:10px",
                               width = 6

                        )
                      )

                    ),

                    #Current Airquality Tab ----
                    tabPanel(
                      title = "Current Airquality",
                      icon = icon("info"),

                      sidebarLayout(

                        sidebarPanel(

                          fluidRow(
                            column(tags$li("Please enter a city and country of your choice using ISO 3166 country codes (e.g., NL for Netherlands)"),
                                   tags$li("All data on the right side will be updated accordingly"),
                                   style="text-align:justify;color:black;background-color:
                             white;padding:15px;border-radius:10px",
                                   width = 12
                            )
                          ),

                          textInput(inputId = "city_in",
                                    value = "Amsterdam",
                                    label = "City",
                                    width = 200),

                          textInput(inputId = "country_in",
                                    value = "NL",
                                    label = "Country",
                                    width = 200)
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel(
                              title = "Current Airquality Data",
                              icon = icon("table"),
                              tableOutput(outputId = "current_aq_df")
                            ),

                            tabPanel(
                              title = "Two Week Airquality",
                              icon = icon("chart-area"),
                              plotOutput(outputId = "plot_aqi_hist")
                            ),

                            tabPanel(
                              title = "Airquality Subcomponents",
                              icon = icon("chart-area"),
                              selectInput(inputId = "comp_in",
                                          label = "Subcomponent",
                                          choices = c("co", "no", "no2", "o3",
                                                      "so2", "pm2_5", "pm10", "nh3" ),
                                          width = 200),
                              plotlyOutput(outputId = "plot_comp_hist")
                            ),

                            tabPanel(
                              title = "Current Weather",
                              icon = icon("cloud"),
                              tableOutput(outputId = "current_weather")
                            )
                          )
                        ))),

                    #Europe Tab ----
                    tabPanel(
                      title = "Europe",
                      icon = icon("globe"),

                      tabsetPanel(
                        tabPanel(title = "Major Cities EUR",
                                 gt_output(outputId = "current_aq_table")
                        ),

                        tabPanel(title = "AQI Map EUR",
                                 plotOutput(outputId = "aqi_map",
                                            width = "100%",
                                            height = "600")
                        )

                      )
                    )

                  ))

  #server ----
  server <- function(input, output) {

    output$current_aq_df <- renderTable({

      current_aq_df(input$city_in, input$country_in)

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

    output$current_weather <- renderTable({

      current_weather(input$city_in, input$country_in)

    })

    output$aqi_map <- renderPlot({

      aqi_map()

    })


  }

  # Create Shiny app ----
  shinyApp(ui = ui, server = server)


}
