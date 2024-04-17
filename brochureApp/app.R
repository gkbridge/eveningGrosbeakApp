library(shiny)
library(leaflet)
library(dplyr)
library(shinythemes)
library(DT)
library(bslib)
library(lubridate)

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

shinyApp(
  ui <- page_navbar(
    
    theme = shinytheme("simplex"),
    title = "Interactive Evening Grosbeak Platform",
    sidebar = sidebar(
      tags$body(HTML("<b>Interact with Evening Grosbeak data!</b><hr>
                  <br><b>Deployments:</b> Look at a map of deployment sites. Click to see which birds are deployed from each.<hr>
                     <br><b>Winter:</b> Look at a map of winter roosting sites. Click to see the routes of birds who roosted together.<hr>
                     <br><b>All Evening Grosbeak Data:</b> Look through evening grosbeak data.<hr>
                     <br><b>Adirondack Deployments:</b> Look through data for birds deployed from the Adirondack region.<hr>
                     <br><b>About:</b> Introduction to project and network/data source.<hr>")),
      padding = 5
    ),
    nav_panel(title = "Deployments and Tracking",
              tabsetPanel(
                tabPanel("Deployments",
                         tabsetPanel(
                           tabPanel("Map", 
                                    tags$h3(HTML("Map of each bird's first deployment")),
                                    tags$body(HTML("Click on a cluster and marker to get more information on the <em>Chosen bird</em> tab.<hr>")),
                                    leafletOutput("map", "100%", 400)),
                           tabPanel("Chosen bird", class = "p-x-2 p-y-2",
                                    tabsetPanel(
                                      tabPanel("Table", # class = "p-x-10", # does not work with padding
                                               fluidRow(
                                                 verbatimTextOutput("Click_text"),
                                                 tabPanel("table", tableOutput("Click_table")),
                                                 leafletOutput("Click_plot"),
                                                 ),
                                               ),
                                      tabPanel("Group",
                                               tags$body(HTML("These birds deployed from the same location as your chosen bird.<hr>")),
                                               fluidRow(
                                                 leafletOutput("Click_group")
                                               ))
                                    ))
                         )),
              )),
    nav_panel(title = "Winter Roosting",
              tabsetPanel(
                layout_columns(
                  card(card_header(HTML("<b>Winter Roosting Locations</b><hr>")), tags$body(HTML("Blue markers are winter roosting locations. <br>Circle/cluster markers are individual roosting birds.
                                                                                                 <br><em>Choose a year to pick one season of roosting. Default is all years.</em>"))),
                  sidebarPanel(
                    numericInput("year", label = h6("Choose year"),
                                 value = NULL, min = 2018, max = 2025),
                    width = 5
                  )
                  # col_widths = c(20, 10)
                ),  
                tabPanel("Map",
                         fluidRow(
                           leafletOutput("winter")
                         )),
                tabPanel("Roosting Group")
              )),
    nav_panel(title = "About",
              tabsetPanel(
                tabPanel("Abstract", textOutput('about')),
                tabPanel("Data", dataTableOutput('data')),
                tabPanel("Adirondack Deployments", dataTableOutput('adkData'))
              ))
    
  ),
  
  server = function(input, output, session) {
    
    output$about <- renderText({
      paste(HTML("This project aims to analyze a large dataset on Evening Grosbeaks, a species within the finch family native to North America. The data comes from Motus, a network of radio receivers that are able to pick up transmissions from tagged birds. Motus has many large datasets of all types of bird species and through data cleaning in R, this project hones in on the data captured for evening grosbeaks. The main objective for this project was to develop a platform enabling users to observe and understand trends in their migration patterns. The R shiny app created for this showcases many features: an interactive map of deployment sites, individual bird tracking, group bird tracking (by winter roosting location and deployment location), all evening grosbeak data used, and all evening grosbeak data used from the Adirondack region.
          "))
    })
    
    # Data tab - EG data table
    output$data <- DT::renderDataTable({
      eg_df
    })
    
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
        paste0("Summary of bird ", click$id)
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
        
        # sort one_bird by timestamp to ensure you are getting very first pinpoint
        one_bird <- one_bird %>%
          arrange(tsCorrected)
        firstDep <- one_bird %>%
          slice(1)
        
        # firstDep uses deployment coordinates to mark
        # one_bird uses receiver coordinates to mark
        one_bird <- one_bird[-1, ] # take out deployment site (initial point and point of firstDep)
        # # rename lat and long for each data set
        firstDep <- firstDep %>%
          rename(
            lat = tagDepLat,
            lon = tagDepLon,
          )
        one_bird <- one_bird %>%
          rename(
            lat = recvDeployLat,
            lon = recvDeployLon
          )
        # join the data sets (vertically) (first row should be deploymnent site)
        # make copies of datasets with desired data
        firstDepCopy <- firstDep %>%
          select(motusTagDepID, lat, lon, tsCorrected, ts)
        one_birdCopy <- one_bird %>%
          select(motusTagDepID, lat, lon, tsCorrected, ts)
        full_bird <- bind_rows(firstDepCopy, one_birdCopy)
        
        new_full_bird <- full_bird %>%
          group_by(lat, lon) %>%
          slice(1)
        
        popupInfo = paste("Lat = ", new_full_bird$lat, ", Lon = ", new_full_bird$lon)
        
        
        leaflet() %>%
          addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
          setView(lat = 15, lng = 0, zoom = 1.5) %>%
          addAwesomeMarkers(data = firstDep,
                            lng = ~lon,
                            lat = ~lat) %>%
          addCircleMarkers(data = new_full_bird,
                           lng = ~lon,
                           lat = ~lat,
                           color = "red",
                           popup = popupInfo,
                           clusterOptions = markerClusterOptions()) %>%
          addPolylines(data = new_full_bird,
                       lng = ~lon,
                       lat = ~lat,
                       color ="blue",
                       weight = 1)
        
      }
    })
    
    # Grab the bird and the birds around it to see if they take the same route
    output$Click_group <- renderLeaflet({
      click <- input$map_marker_click
      if (!is.null(click)){
        bird <- DATA[which(DATA$motusTagDepID == click$id), ]
        
        # filter through deployment sites map to get the group of birds that would have been around it
        group_on_dep <- first_deployment_df %>%
          filter(tagDepLat == bird$tagDepLat && tagDepLon == bird$tagDepLon)
        # browser()
        
        group_birds <- eg_df %>%
          filter(motusTagDepID %in% group_on_dep$motusTagDepID) # need to filter per bird, one hit per location
        
        bird_destination_freq <- group_birds %>%
          group_by(motusTagDepID, recvDeployLat, recvDeployLon) %>%
          summarise(freq = round(sqrt(n())))
        
        leaflet() %>%
          addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
          setView(lat = 15, lng = 0, zoom = 1.5) %>%
          addCircleMarkers(data = group_birds,
                           lng = ~recvDeployLon,
                           lat = ~recvDeployLat,
                           color = "red",
                           clusterOptions = markerClusterOptions()) %>%
          addPolylines(data = bird_destination_freq,
                       lng = ~recvDeployLon,
                       lat = ~recvDeployLat,
                       color = "blue", # want to color by bird
                       weight = ~freq)
        
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
    
    output$winter <- renderLeaflet({
      # browser()
      winter <- eg_df %>%
        filter(month(ts) %in% c(1, 2, 12)) %>%
        group_by(motusTagDepID) %>% # don't need the same bird popping up over and over
        slice(1)
      
      winterLocations <- winter %>%
        group_by(recvDeployName) %>%
        slice(1)
      
      popupInfo1 = paste("Unique ID = ", winter$motusTagDepID,
                         "Time = ", winter$ts)
      popupInfo2 = paste("Location = ", winterLocations$recvDeployName)
      
      winterSpots <- leaflet() %>% 
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
        setView(lat = 15, lng = 0, zoom = 1.5) %>% 
        addCircleMarkers(data = winter,
                         lng = ~recvDeployLon,
                         lat = ~recvDeployLat,
                         color = "red",
                         clusterOptions = markerClusterOptions(),
                         popup = popupInfo1) %>%
        addAwesomeMarkers(data = winterLocations,
                          lng = ~recvDeployLon,
                          lat = ~recvDeployLat,
                          popup = popupInfo2)
      winterSpots
      
    })
    
    # ADK data
    output$adkData <- DT::renderDataTable({
      eg_adks
    })
    
  }
)
