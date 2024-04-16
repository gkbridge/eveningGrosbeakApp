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
  ui = fluidPage(
    theme = shinytheme("simplex"),
    
    navbarPage(
      "Evening Grosbeak Interactive Platform",
      sidebarPanel(
        tags$body(HTML("<b>Interact with Evening Grosbeak data!</b><hr>
                  <br><b>Deployments:</b> Look at a map of deployment sites. Click to see which birds are deployed from each.<hr>
                     <br><b>Winter:</b> Look at a map of winter roosting sites. Click to see the routes of birds who roosted together.<hr>
                     <br><b>All Evening Grosbeak Data:</b> Look through evening grosbeak data.<hr>
                     <br><b>Adirondack Deployments:</b> Look through data for birds deployed from the Adirondack region.<hr>
                     <br><b>About:</b> Introduction to project and network/data source.<hr>")),
        width = 3
      ),
      tabPanel("Deployments and Tracking",
               tabsetPanel(
                 tabPanel("Map", 
                          tags$h3(HTML("Map of each bird's first deployment")),
                          tags$body(HTML("Click on a cluster and marker to get more information on the <em>Chosen bird</em> tab.<hr>")),
                          leafletOutput("map", "100%", 400))
                 # tabPanel("Chosen bird",
                 #          tabsetPanel(
                 #            tabPanel("Table",
                 #                     fluidRow(
                 #                       verbatimTextOutput("Click_text"),
                 #                       tabPanel("table", tableOutput("Click_table")),
                 #                       leafletOutput("Click_plot"))),
                 #            tabPanel("Group",
                 #                     tags$body(HTML("These birds deployed from the same location as your chosen bird.<hr>")),
                 #                     fluidRow(
                 #                       leafletOutput("Click_group")
                 #                     ))
                 #          )
                 # )
               )),
      tabPanel("About",
               tabsetPanel(
                 tabPanel("Background"),
                 tabPanel("All Data",
                          dataTableOutput('data'))
               )),
    )
    
  ),
  
  server = function(input, output, session) {
    
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
    
    # Data tab - EG data table
    output$data <- DT::renderDataTable({
      eg_df
    })
    
    
  }
)
