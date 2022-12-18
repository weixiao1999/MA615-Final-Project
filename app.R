library(shiny)
library(shinythemes)
library(geojsonio)
library(broom)
library(sf)
library(osmdata)
library(ggplot2)
library(dplyr)
library(ggnewscale)
library(readr)

stops <- read_csv("https://raw.githubusercontent.com/weixiao1999/MA615-Final-Project/main/stops.txt")
stops_T <- dplyr::filter(stops,stops$stop_id=="70001"|stop_id=="70002"|stop_id=="70004"|stop_id=="70006"|stop_id=="70010"|stop_id=="70012"|stop_id=="70014"|stop_id=="70016"|stop_id=="70018"|stop_id=="70020"|stop_id=="70022"|stop_id=="70022"|stop_id=="70024"|stop_id=="70026"|stop_id=="70028"|stop_id=="70030"|stop_id=="70032"|stop_id=="70034"|stop_id=="70036"|stop_id=="70278")
spdf_file <- geojson_read("https://raw.githubusercontent.com/weixiao1999/MA615-Final-Project/main/ZIP_Codes.geojson",what = "sp")
stats_df <- as.data.frame(spdf_file)
spdf_file <- tidy( spdf_file,region="ZIP5" )
stop_id <- stops_T$stop_id
bus_location <- dplyr::filter(stops,stop_id=="110"|stop_id=="67"|stop_id=="72"|stop_id=="75"|stop_id=="79"|stop_id=="187"|stop_id=="59"|stop_id=="62"|stop_id=="64")
stopbus_id <- bus_location$stop_id



ui <- 
  navbarPage("MBTA in Boston", collapsible = TRUE, inverse = TRUE, 
             theme = shinytheme("flatly"),
             tabPanel("Orange Line", plotOutput("plot1")),
           
             tabPanel("With Orange Line stop_id selection",
                      fluidPage(
                        selectInput("stop_id", "Choose Your Stop_id,
                                    the travel time from 70001 showed below", stop_id),
                        plotOutput("plot2"),
                        verbatimTextOutput("time")
                      )),
             tabPanel("Bus", plotOutput("plot3")),
             tabPanel("With Bus stop_id selection",
                      fluidPage(
                        selectInput("stopbus_id", "Choose Your Stop_id,
                                    the travel time from 110 showed below", stopbus_id),
                        plotOutput("plot4"),
                        verbatimTextOutput("time_bus")
                        )))
             
server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot() +
      geom_polygon(data=spdf_file,
                   aes(x=long,
                       y=lat,
                       group=group),
                   alpha=0,
                   color="black",
                   size=.5) +
      geom_point(data=stops_T,
                 aes(x=stop_lon,
                     y=stop_lat),
                 fill="orange",
                 alpha=.8,
                 size=2,
                 shape=22) +
      theme_void() +
      coord_map() +
      labs(title="Orange Line in Boston")
  })
output$plot2 <- renderPlot({
  ggplot() +
    geom_polygon(data=spdf_file,
                 aes(x=long,
                     y=lat,
                     group=group),
                 alpha=0,
                 color="black",
                 size=.5) +
    geom_point(data=filter(stops_T,stops_T$stop_id==input$stop_id),
               aes(x=stop_lon,
                   y=stop_lat),
               fill="orange",
               alpha=.8,
               size=4,
               shape=22) +
    geom_point(data=filter(stops_T,stops_T$stop_id=="70001"),
               aes(x=-71.11369,y=42.30052),
               fill="red",
               alpha=.8,
               size=4,
               shape=22)+
    theme_void() +
    coord_map() +
    labs(title="Orange Line in Boston")
})

output$time <- renderPrint({ 
  ifelse(input$stop_id=="70002",122.5537,
                           ifelse(input$stop_id=="70004",226.7342,
                                  ifelse(input$stop_id=="70006",354.9257,
                                         ifelse(input$stop_id=="70008",459.0807,
                                                ifelse(input$stop_id=="70010",566.1711,
                                                       ifelse(input$stop_id=="70012",717.5580,
                                                              ifelse(input$stop_id=="70014",902.7081,
                                                                     ifelse(input$stop_id=="70016",1142.2838,
                                                                            ifelse(input$stop_id=="70018",1237.7784,
                                                                                   ifelse(input$stop_id=="70020",1319.1821,
                                                                                          ifelse(input$stop_id=="70022",1401.6612,
                                                                                                 ifelse(input$stop_id=="70024",1543.6635,
                                                                                                        ifelse(input$stop_id=="70026",1649.4762,
                                                                                                               ifelse(input$stop_id=="70028",1789.3144,
                                                                                                                      ifelse(input$stop_id=="70030",1938.1214,
                                                                                                                             ifelse(input$stop_id=="70032",2175.2021,
                                                                                                                                    ifelse(input$stop_id=="70034",2435.8897,
                                                                                                                                           ifelse(input$stop_id=="70036",2573.7567,
                                                                                                                                                 ifelse(input$stop_id=="70278",2048.1622,0)))))))))))))))))))
 
})
output$plot3 <- renderPlot({
  ggplot() +
    geom_polygon(data=spdf_file,
                 aes(x=long,
                     y=lat,
                     group=group),
                 alpha=0,
                 color="black",
                 size=.5) +
    geom_point(data=bus_location,
               aes(x=stop_lon,
                   y=stop_lat),
               fill="blue",
               alpha=.8,
               size=2,
               shape=22) +
    theme_void() +
    coord_map() +
    labs(title="Bus in Boston")
})
output$plot4 <- renderPlot({
    ggplot() +
      geom_polygon(data=spdf_file,
                   aes(x=long,
                       y=lat,
                       group=group),
                   alpha=0,
                   color="black",
                   size=.5) +  
    geom_point(data=filter(bus_location,bus_location$stop_id==input$stopbus_id),
               aes(x=stop_lon,
                   y=stop_lat),
               fill="blue",
               alpha=.8,
               size=4,
               shape=22) +
    geom_point(data=filter(bus_location,bus_location$stop_id=="110"),
               aes(x=-71.11812,y=42.37326),
               fill="purple",
               alpha=.8,
               size=4,
               shape=22)+
    theme_void() +
    coord_map() +
    labs(title="Bus in Boston")
  })
output$time_bus <- renderPrint({ 
  ifelse(input$stopbus_id=="67",240,
         ifelse(input$stopbus_id=="72",420,
                ifelse(input$stopbus_id=="75",600,
                       ifelse(input$stopbus_id=="79",840,
                              ifelse(input$stopbus_id=="187",960,
                                     ifelse(input$stopbus_id=="59",1200,
                                            ifelse(input$stopbus_id=="62",1440,
                                                   ifelse(input$stopbus_id=="64",2700,0))))))))
                                                          
})
}

shinyApp(ui = ui, server = server)