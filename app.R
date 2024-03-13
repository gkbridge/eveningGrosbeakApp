library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(tibble)
library(tibbletime)
library(lubridate)
library(shinythemes)
library(DT)
library(htmlwidgets)

# Winter roosting, birds that are found together in summer, do they stay together in the winter
# Allow for more than one bird tracking (groups), pick one bird and see which ones are connected to it
# Network analysis for connections between birds (connection if they are in same site at same time)

eg_df <- readRDS("df_eg.RDS")
## Fix data to be able to get unique birds
eg_df <- eg_df %>%
  mutate(motusTagDepID = paste(motusTagID, tagDeployID, sep = "."))
eg_df <- eg_df %>%
  relocate(motusTagDepID, .after = motusTagID)

adks <- c(7219, 9588, 7762, 7763, 9693, 7270, 6124, 7238, 8937)
eg_adks <- eg_df %>%
  filter(eg_df$recvDeployID %in% adks)

## Deployment site data manipulation
first_deployment_df <- eg_df %>%
  arrange(., tsCorrected)
first_deployment_df <- first_deployment_df %>%
  group_by(motusTagDepID) %>%
  slice(1)

DATA <- first_deployment_df

# Define UI
ui <- fluidPage(
    
    theme = shinytheme("cosmo"),

    # Application title
    titlePanel("Evening Grosbeak"),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Motus",
                 tags$h5(HTML("This is an interactive shiny app to portray motus data for evening grosbeaks."))),
        tabPanel("Data",
                 # downloadButton('downloadAll',"Download the data"),
                 dataTableOutput('data')),
        tabPanel("Deployment Sites",
                 tags$h5(HTML("Below is an interactive map that shows where all evening grosbeak motus-tracked birds were deployed.")),
                 leafletOutput('map'), #, sidebarPanel to select time frame and then filter the data down below
                 verbatimTextOutput("Click_text"),
                 tableOutput("Click_table"),
                 plotOutput("Click_plot")
        ),
        tabPanel("Track one bird",
                 tags$h5(HTML("Choose one bird, using the motusTagDepID from the data table and track its whereabouts.")),
                 sidebarPanel(textInput('uniqueID', label = h5("Enter unique bird ID"), value = "73291.44424")),
                 sidebarPanel(dateInput('startDate', label = h5("Choose a start date"))),
                 sidebarPanel(dateInput('endDate', label = h5("Choose an end date"))),
                 leafletOutput('onebird'),
                 dataTableOutput('oneBirdTable')) #,
      #   tabPanel("Toy dataset",
      #            leafletOutput('toy')),
      #             verbatimTextOutput("Click_text"),
      #             tableOutput("Click_table"),
      #             plotOutput("Click_plot")
      )
    )

    
)

# Define server logic required to draw a histogram
server = function(input, output, session) {
    # added session to function parameters to make reactive
  
    # Data manipulation
    selectedData <- reactive({
      # track one bird
      start_date <- as.Date(input$startDate)
      end_date <- as.Date(input$endDate)
      
      bird <- eg_df %>%
        filter(motusTagDepID == input$uniqueID)
      bird <- bird %>%
        filter(ts >= start_date, ts <= end_date)
      bird
    })
    
    # Data tab - EG data table
    output$data <- DT::renderDataTable({
      eg_df
    })
    output$downloadAll <- downloadHandler(
      filename = function(){"df_eg.RDS"}, 
      content = function(fname){
        write.csv(thedata(), fname)
      }
    )
    
    # ADK data
    output$adkData <- DT::renderDataTable({
      eg_adks
    })
    
    # Deployment site map (leaflet)
    # output$deployments <- renderLeaflet({
    #   popupInfo = paste("Unique ID = ", first_deployment_df$motusTagDepID,
    #                     "Deployment site = ", first_deployment_df$recvDeployName)
    #   
    #   leaflet() %>% 
    #     addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
    #     setView(lat = 15, lng = 0, zoom = 1.5) %>% 
    #     addCircleMarkers(data = first_deployment_df,
    #                      lng = ~tagDepLon,
    #                      lat = ~tagDepLat,
    #                      color = "red",
    #                      clusterOptions = markerClusterOptions(),
    #                      popup = popupInfo)
    # 
    # })
    
    ## INTERACTIVE DEPLOYMENT MAP ##
    # reactive data
    mapDATA<-reactive({
      DATA
    })
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
        setView(lat = 15, lng = 0, zoom = 1.5)
    })
    
    # Render circle markers
    observe({ 
      map <- leafletProxy("map", data = mapDATA())
      map %>% clearMarkers()
      if (!is.null(mapDATA())) {
        map %>% 
          addCircleMarkers(lat = ~tagDepLat, 
                           lng = ~tagDepLon, 
                           layerId = ~motusTagDepID, 
                           clusterOptions =  markerClusterOptions()
          )
      }
    })
    
    output$Click_text<-renderText({
      click <- input$map_marker_click
      if (!is.null(click))
        paste0("Would you like to summarize station ", click$id, " ?")
    })
    
    
    output$Click_table<-renderTable({
      click <- input$map_marker_click
      if (!is.null(click)){
        bird <- DATA[which(DATA$motusTagDepID == click$id), ]
        bird
      }
    })
    
    # Show popup on click
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      text<-paste("Latitude ", click$lat, "Longtitude ", click$lng)
      
      map <- leafletProxy("map")
      map %>% clearPopups() %>%
        addPopups(click$lng, click$lat, text)
    })
    
    ## END INTERACTIVE DEPLOYMENT CODE ##
    
    # Track one bird
    output$onebird <- renderLeaflet({
      test_bird = selectedData()
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
        setView(lat = 15, lng = 0, zoom = 1.5) %>% 
        addCircleMarkers(data = test_bird,
                         lng = ~recvDeployLon,
                         lat = ~recvDeployLat,
                         color = "red",
                         clusterOptions = markerClusterOptions()) %>%
        addPolylines(data = test_bird,
                     lng = ~recvDeployLon,
                     lat = ~recvDeployLat,
                     color ="blue",
                     weight = 1)
    })
    
    # Fluid Pages
    # output$onebird <- DT::renderDataTable({
    #   one = selectedData()
    #   one
    # })
    
    
    # output$toy <- renderLeaflet({
    #   leaflet() %>% 
    #     addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
    #     setView(lat = 15, lng = 0, zoom = 1.5) %>% 
    #     addCircleMarkers(data = toys,
    #                      lng = ~longitude,
    #                      lat = ~latitude,
    #                      color = "red",
    #                      clusterOptions = markerClusterOptions()) %>%
    #     # Add click event handler
    #     on('click', function(e) {
    #       # Get information from clicked marker
    #       clicked_data <- toys[toys$latitude == e$latlng$lat & toys$longitude == e$latlng$lng,]
    #       
    #       # Check if a marker was clicked
    #       if (nrow(clicked_data) > 0) {
    #         # Create popup content
    #         popup_content <- paste("<b>", clicked_data$name, "</b><br>",
    #                                "Latitude:", round(clicked_data$latitude, 4), "<br>",
    #                                "Longitude:", round(clicked_data$longitude, 4))
    #         
    #         # Open popup at clicked location
    #         L.popup(popup_content)
    #         .setLatLng(e$latlng)
    #         .openOn(this)
    #       }
    #     })
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
  