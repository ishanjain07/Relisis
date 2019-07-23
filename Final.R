library(shiny)
library(shinydashboard)
library(leaflet)
library(jsonlite)
library(tidyverse)
library(leaflet.extras)
library(lubridate)
library(cluster)
library(factoextra)

ui <- dashboardPage(skin = "red",
                    title = "RELISIS",
                    dashboardHeader(title = "RELISIS", titleWidth = 350),
                    
                    dashboardSidebar(width = 350,
                                     
                                     tags$div(
                                       tags$blockquote(tags$h3("WebApp to Determine Safe Zones and Stay Safe From Disaster Prone Areas")),
                                       tags$br(),
                                       tags$h4("-> It will help in deciding the position of the relief centres to be scattered across the city using the cluster formulation based algorithms."),
                                       tags$br(),
                                       tags$h4("-> Provide an approximation of the food and utility estimates and the general population of the individual nodes and how it is suitable to each individual."),
                                       tags$br(),
                                       tags$p("Upload the JSON file "),
                                       style = "padding: 10px; font-size:15px;"
                                       
                                     ),
                                     
                                     tags$hr(),
                                     
                                     # Input: Select a file ----
                                     fileInput("file1", "Upload JSON forma File",
                                               multiple = FALSE,
                                               accept = ".json",
                                               placeholder = "Max file size 100Mb"),
                                     
                                     tags$hr()
                                     
                    ),
                    
                    # Main panel for displaying outputs ----
                    dashboardBody(
                      
                      tags$head(tags$style("#myMap{height:90vh !important;}")),
                      
                      leafletOutput("myMap"),
                      print(tags$br()),
                      tags$div(
                        fluidRow(
                          column(12,
                                 dataTableOutput('dttable')
                          )),        
                        print(tags$br()),
                        plotOutput("latvslon"),
                        print(tags$br()),
                        print(tags$br()),
                        plotOutput("kmeans_mapping6"),
                        print(tags$br()),
                        print(tags$br()),
                        plotOutput("kmeans_2"),
                        print(tags$br()),
                        print(tags$br()),
                        plotOutput("kmeans_3"),
                        print(tags$br()),
                        print(tags$br()),
                        plotOutput("kmeans_4"),
                        print(tags$br()),
                        print(tags$br()),
                        plotOutput("optimal_wss"),
                        print(tags$br()),
                        print(tags$br()),
                        plotOutput("optimal_silhouette"),
                        print(tags$br()),
                        print(tags$br()),
                        plotOutput("optimal_gapstat"),
                        print(tags$br()),
                        print(tags$br()),
                        dataTableOutput('table')
                      )
                      
                    )
)


# Define server logic to read selected file ----
server <- function(input, output) {
  
  options(shiny.maxRequestSize = 100*1024^2)
  
  output$myMap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Default Maptile") %>% 
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Maptile") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Maptile") %>%
      setView(24, 27, zoom = 2) %>% 
      addLayersControl(
        baseGroups = c("Default Maptile", "Dark Maptile", "Satellite Maptile"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    
    withProgress(message = 'Please wait...',
                 value = 0/4, {
                   
                   req(input$file1)
                   
                   incProgress(1/4, detail = "reading data")
                   
                   locationdata <- fromJSON(input$file1$datapath, simplifyVector = TRUE, simplifyDataFrame = TRUE)
                   
                   filemy <- fromJSON("short.json")
                   system.time(filemy)
                   myloc = filemy$locations
                   
                   newIcons <- iconList(
                     stand = makeIcon("stand.png", "stand.png", 36, 36),
                     drive = makeIcon("drive.png", "drive.png", 36, 36)
                   )
                   
                   lati = myloc$latitudeE7 / 1e7
                   longi = myloc$longitudeE7 / 1e7
                   center1 = data.frame(longi[1],lati[1]) 
                   plot( x= longi, y=lati)
                   data1 = data.frame(longi,lati)
                   
                   incProgress(1/4, detail = "cleaning data")
                   
                   myData <- locationdata$locations %>% 
                     select(latitudeE7, longitudeE7, `timestampMs`, velocity) %>% 
                     mutate(lat = latitudeE7 / 1E7, lon = longitudeE7 / 1E7) %>% 
                     mutate(timestampMs = as.numeric(timestampMs)) %>%
                     mutate(Date = as.POSIXct(timestampMs/1000, origin="1970-01-01")) %>%
                     select(-latitudeE7, -longitudeE7) %>% 
                     mutate(image = case_when(
                       velocity > 10 ~ "drive",
                       TRUE ~ "stand"
                     )) %>% 
                     mutate(image = factor(image, levels = c("drive","stand")))
                   
                   
                   myloc$time = as.POSIXct(as.numeric(myloc$timestampMs)/1000, origin = "1970-01-01")
                   
                   
                   
                   incProgress(1/4, detail = "rendering map")
                   
                   leafletProxy("myMap", data = myData) %>%
                     fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>%  
                     addHeatmap(lng = ~lon, lat = ~lat, group = "HeatMap", blur = 20, max = 0.01, radius = 15) %>%
                     addMarkers(data = head(myData, 50000), ~lon, ~lat, icon = ~newIcons[image], clusterOptions = markerClusterOptions(), 
                                label = ~ format(Date, format = "%H:%M %d-%b-%Y"), group = "Points") %>% 
                     addLayersControl(
                       baseGroups = c("Default Maptile", "Dark Maptile", "Satellite Maptile"),
                       overlayGroups = c("HeatMap", "Points"),
                       options = layersControlOptions(collapsed = FALSE)
                     )
                   
                   incProgress(1/4)
                   
                 })
  })
  
  
  
  # 
  # observe({
  #   
  # })
  # 
  
  
  
  output$dttable <- renderTable(data.frame(lati,longi),striped = FALSE, hover = FALSE, bordered = TRUE)
  
  
  
  
  # output$ziptable <- DT::renderDataTable({
  #     Action = paste('<a class="go-map" href="" data-lat="', lati, '" data-long="', longi, '"><i class="fa fa-crosshairs"></i></a>', sep="")
  #   action <- DT::dataTableAjax(session, df)
  #   
  #   DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  # })
  
  
  
  
  
  output$latvslon <- renderPlot({
    plot( x= longi, y=lati)    
  })
  
  output$kmeans_mapping6 <- renderPlot({
    
    kmeans_data_Cal = kmeans(data1,centers = 6)
    plot(kmeans_data_Cal$cluster)
  })
  
  output$kmeans_2 <- renderPlot({
    
    kmeans_clu2 = kmeans(data1,centers = 2)
    fviz_cluster(kmeans_clu2, data = data1) + ggtitle("k = 2")
  })
  
  output$kmeans_3 <- renderPlot({
    kmeans_clu3 = kmeans(data1,centers = 3)
    fviz_cluster(kmeans_clu3, data = data1) + ggtitle("k = 3")
  })
  
  output$kmeans_4 <- renderPlot({
    kmeans_clu4 = kmeans(data1,centers = 4)
    fviz_cluster(kmeans_clu4, data = data1) + ggtitle("k = 4")
  })
  
  output$optimal_wss <- renderPlot({
    fviz_nbclust(data1, kmeans, method = "wss") + ggtitle("Using WSS")
  })
  
  output$optimal_silhouette <- renderPlot({
    fviz_nbclust(data1, kmeans, method = "silhouette") + ggtitle("Using Silhouette")
  })
  
  output$optimal_gapstat <- renderPlot({
    fviz_nbclust(data1, kmeans, method = "gap_stat") + ggtitle("Using Gap_stat")
  })
  
  output$dttable <- renderTable(data.frame(lati,longi),striped = FALSE, hover = FALSE, bordered = TRUE)
  
  name=rep(c("John","Doe"),435)
  dataxyz=data.frame(longi,lati,myloc$time,name)
  output$table <- renderDataTable(dataxyz)
  
  
  
}

shinyApp(ui, server)