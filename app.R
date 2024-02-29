library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(tibble)
library(tibbletime)
library(motus)
library(lubridate)
library(RSQLite)
library(shinythemes)
library(DT)
library(htmlwidgets)

# # TOY DATA SET
# toys <- data.frame(
#   name = c("First", "Second", "Third"),
#   latitude = c(42.36, 41.89, 42.11),
#   longitude = c(-71.06, -71.41, -72.68)
# )
# # Add state abbreviations for the northeast US locations
# toys$state <- c("MA", "RI", "CT")

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
                 downloadButton('downloadAll',"Download the data"),
                 dataTableOutput('data')),
        tabPanel("Adirondack Deployments",
                 dataTableOutput('adkData')),
        tabPanel("Deployment Sites",
                 tags$h5(HTML("Below is an interactive map that shows where all evening grosbeak motus-tracked birds were deployed.")),
                 leafletOutput('deployments') #, sidebarPanel to select time frame and then filter the data down below
        ),
        tabPanel("Track one bird",
                 tags$h5(HTML("Choose one bird, using the motusTagDepID from the data table and track its whereabouts.")),
                 sidebarPanel(textInput('uniqueID', label = h5("Enter unique bird ID"), value = "73291.44424")),
                 sidebarPanel(dateInput('startDate', label = h5("Choose a start date"))),
                 sidebarPanel(dateInput('endDate', label = h5("Choose an end date"))),
                 leafletOutput('onebird')) #,
        # tabPanel("Toy dataset",
        #          leafletOutput('toy'))
      )
    )

    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
    
    ## eventReactive ? - possibly for clicks/kind of like reactive wrapper
    ## try around with toy sample, to see if you can get redirecting/event handling to work
    
    # Deployment site map (leaflet)
    output$deployments <- renderLeaflet({
      popupInfo = paste("Unique ID = ", first_deployment_df$motusTagDepID,
                        "Deployment site = ", first_deployment_df$recvDeployName)
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
        setView(lat = 15, lng = 0, zoom = 1.5) %>% 
        addCircleMarkers(data = first_deployment_df,
                         lng = ~tagDepLon,
                         lat = ~tagDepLat,
                         color = "red",
                         clusterOptions = markerClusterOptions(),
                         popup = popupInfo)
      # observe({
      #   click<-input$map_marker_click
      #   if(is.null(click))
      #     return()
      #   text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
      #   text2<-paste("You've selected point ", click$id)
      #   map$clearPopups()
      #   map$showPopup( click$lat, click$lng, text)
      #   output$Click_text<-renderText({
      #     text2
      #   })
      #   
      # })
      #
    })
    
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
  