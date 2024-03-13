library(shiny)
library(leaflet)
library(dplyr)
library(shinythemes)
library(DT)

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
                   plotOutput("Click_plot"))),
        tabPanel("Motus",
                 fluidRow(
                   tags$h5(HTML("This is an interactive shiny app to portray motus data for evening grosbeaks."))
                 )),
        tabPanel("All Evening Grosbeak Data",
                 dataTableOutput('data')),
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
        paste0("Would you like to summarize station ", click$id, " ?")
    })
    
    
    output$Click_table<-renderTable({
      click <- input$map_marker_click
      if (!is.null(click)){
        bird <- DATA[which(DATA$motusTagDepID == click$id), ]
        bird
      }
    })
    
    # output$Click_plot <- renderPlot({
    #   click <- input$map_marker_click
    #   if (!is.null(click)){
    #     bird <- DATA[which(DATA$ids == click$id), ]
    #     
    #     plot(bird[1:2])
    #   }
    # })
    
    
    # Show popup on click
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      text<-paste("Lattitude ", click$lat, "<br>Longtitude ", click$lng)
      
      map <- leafletProxy("map")
      map %>% clearPopups() %>%
        addPopups(click$lng, click$lat, text)
    })
    ## END INTERACTIVE MAP OUTPUT
    
    # Data tab - EG data table
    output$data <- DT::renderDataTable({
      eg_df
    })
    
    
  })
