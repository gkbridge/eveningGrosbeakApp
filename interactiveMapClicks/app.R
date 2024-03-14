library(shiny)
library(leaflet)
library(dplyr)
library(shinythemes)
library(DT)
library(rsconnect)
# rsconnect::deployApp('path/to/your/app')

## DATA PREP
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

## SHINY APP
shinyApp(
  ui = fluidPage(
    
    theme = shinytheme("cosmo"),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Deployments",
                 fluidRow(
                   leafletOutput("map", "100%", 400))),
        tabPanel("Table",
                 fluidRow(
                   verbatimTextOutput("Click_text"),
                   tabPanel("table", tableOutput("Click_table")),
                   leafletOutput("Click_plot"))),
        # tabPanel("Track one bird",
        #          tags$h5(HTML("Choose one bird, using the motusTagDepID from the data table and track its whereabouts.")),
        #          sidebarPanel(textInput('uniqueID', label = h5("Enter unique bird ID"), value = "73291.44424")),
        #          sidebarPanel(dateInput('startDate', label = h5("Choose a start date"))),
        #          sidebarPanel(dateInput('endDate', label = h5("Choose an end date"))),
        #          leafletOutput('onebird'),
        #          dataTableOutput('oneBirdTable')),
        tabPanel("Motus",
                 fluidRow(
                   tags$h5(HTML("This is an interactive shiny app to portray motus data for evening grosbeaks."))
                 )),
        tabPanel("All Evening Grosbeak Data",
                 dataTableOutput('data')),
        tabPanel("Adirondack Deployments",
                 dataTableOutput('adkData')),
      )
    )
  ),
  
  
  server = function(input, output, session){
    
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
        paste0("Would you like to summarize bird ", click$id, " ?")
    })
    
    
    output$Click_table<-renderTable({
      click <- input$map_marker_click
      if (!is.null(click)){
        bird <- DATA[which(DATA$motusTagDepID == click$id), ]
        bird
      }
    })
    
    output$Click_plot <- renderLeaflet({
      # add pop ups for this table as well, work as well with being able to use reactive data here as well to limit time frame
      
      click <- input$map_marker_click
      if (!is.null(click)){
        bird <- DATA[which(DATA$motusTagDepID == click$id), ]

        # Go through full data set now to check for this bird with matching id
        one_bird <- eg_df %>%
          filter(eg_df$motusTagDepID == bird$motusTagDepID)
        # browser()
        
        firstDep <- one_bird %>%
          slice(1)
        
        # trim these two datasets together to connect, rename lat and lon so they have the same variable name
        # group_by at location so bird only shows up once at each place (group by lat, lon)
        # mutate, so that arrival date and end date at each location are consistent (look like summarize statements, but use mutate instead so that it is always the same value)
        # slice 1
        # Put the date in the pop-up
        # Select any other variables needed to put in pop-up
        # image in pop-up of evening grosbeak ? custom markers 

        leaflet() %>%
          addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
          setView(lat = 15, lng = 0, zoom = 1.5) %>%
          addCircleMarkers(data = firstDep,
                           lng = ~tagDepLon,
                           lat = ~tagDepLat,
                           color = "green") %>%
          addCircleMarkers(data = one_bird,
                           lng = ~recvDeployLon,
                           lat = ~recvDeployLat,
                           color = "red",
                           clusterOptions = markerClusterOptions()) %>%
          addPolylines(data = one_bird,
                       lng = ~recvDeployLon,
                       lat = ~recvDeployLat,
                       color ="blue",
                       weight = 1)

      }
    })
    
    
    # Show popup on click
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      text<-paste("ID", click$id, "<br>Lattitude ", round(click$lat, 2), "<br>Longtitude ", round(click$lng,2))
      
      map <- leafletProxy("map")
      map %>% clearPopups() %>%
        addPopups(click$lng, click$lat, text)
    })
    ## END INTERACTIVE MAP OUTPUT
    
    
    # Data tab - EG data table
    output$data <- DT::renderDataTable({
      eg_df
    })
    
    # ADK data
    output$adkData <- DT::renderDataTable({
      eg_adks
    })
    
    
  })
