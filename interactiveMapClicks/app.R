library(shiny)
library(leaflet)
library(dplyr)
library(shinythemes)
library(DT)
library(bslib)
library(lubridate)

# try to restructure with new app (banner panel, check out different application layouts)
# look into a multipage app
# Google shiny layouts/templates

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
    
    theme = shinytheme("simplex"),
    
    titlePanel("Evening Grosbeak Interactive Platform"),
    
    sidebarPanel(
      tags$body(HTML("<b>Interact with Evening Grosbeak data!</b><hr>
                  <br><b>Deployments:</b> Look at a map of deployment sites. Click to see which birds are deployed from each.<hr>
                     <br><b>Winter:</b> Look at a map of winter roosting sites. Click to see the routes of birds who roosted together.<hr>
                     <br><b>All Evening Grosbeak Data:</b> Look through evening grosbeak data.<hr>
                     <br><b>Adirondack Deployments:</b> Look through data for birds deployed from the Adirondack region.<hr>
                     <br><b>About:</b> Introduction to project and network/data source.<hr>")),
      width = 3
    ),
  
    mainPanel(
      tabsetPanel(
        tabPanel("Deployments",
                 tabsetPanel(
                   tabPanel("Map", 
                            tags$h3(HTML("Map of each bird's first deployment")),
                            tags$body(HTML("Click on a cluster and marker to get more information on the <em>Chosen bird</em> tab.<hr>")),
                            leafletOutput("map", "100%", 400)),
                   tabPanel("Chosen bird",
                            tabsetPanel(
                              tabPanel("Table",
                                       fluidRow(
                                         verbatimTextOutput("Click_text"),
                                         tabPanel("table", tableOutput("Click_table")),
                                         leafletOutput("Click_plot"))),
                              tabPanel("Group",
                                       tags$body(HTML("These birds deployed from the same location as your chosen bird.<hr>")),
                                       fluidRow(
                                         leafletOutput("Click_group")
                                       ))
                            )
                   )
                 )
        ),
        # tabPanel("Track one bird",
        #          tags$h5(HTML("Choose one bird, using the motusTagDepID from the data table and track its whereabouts.")),
        #          sidebarPanel(textInput('uniqueID', label = h5("Enter unique bird ID"), value = "73291.44424")),
        #          sidebarPanel(dateInput('startDate', label = h5("Choose a start date"))),
        #          sidebarPanel(dateInput('endDate', label = h5("Choose an end date"))),
        #          leafletOutput('onebird'),
        #          dataTableOutput('oneBirdTable')),
        tabPanel("Winter",
                 tabsetPanel(
                   sidebarPanel(
                     numericInput("year", label = h6("Choose year"),
                                  value = NULL, min = 2018, max = 2025)
                   ),
                   tabPanel("Map",
                            fluidRow(
                              leafletOutput("winter")
                            )),
                   tabPanel("Roosting Group")
                   ),
                 ),
        tabPanel("All Evening Grosbeak Data",
                 dataTableOutput('data')),
        tabPanel("Adirondack Deployments",
                 dataTableOutput('adkData')),
        tabPanel("About",
                 fluidRow(
                   tags$h5(HTML("This is an interactive shiny app to portray motus data for evening grosbeaks."))
                 )),
      ),
      width = 9
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
        
        # 4/4
        # quick translate it and submit it in english
        # flush out the app (headers, nice-looking, nested tabs, background themes, introduction to motus/birds)
        # weight the lines
        # for loop with the thickness/weight of the line
        # based on frequency, grouped by destination
        # subet for the different results in the destinations
        # figure out how big the subset
        # that feeds into the weight as you add the polylines for those birds that are going to that desination
        
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
    
    
    # Data tab - EG data table
    output$data <- DT::renderDataTable({
      eg_df
    })
    
    # ADK data
    output$adkData <- DT::renderDataTable({
      eg_adks
    })
    
    
  })
