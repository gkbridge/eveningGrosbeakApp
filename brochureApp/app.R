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

### Create UI and server functions for each page
# UI for Page 1
# Home
ui_page1 <- function() {
  # UI logic for Page 1
  fluidPage(
    mainPanel(
      tabsetPanel(
        tabPanel("All Evening Grosbeak Data",
                 dataTableOutput('data')),
        tabPanel("Adirondack Data")
      )
    )
  )
}

# Server logic for Page 1
server_page1 <- function(input, output, session) {
  # Simple test data frame
  test_df <- data.frame(col1 = rnorm(10), col2 = rbinom(10, 1, 0.5))

  # Data tab - EG data table
  output$data <- DT::renderDataTable({
    test_df
  })
}

# # UI for Page 2
# ui_page2 <- function() {
#   # UI logic for Page 2
# }
# 
# # Server logic for Page 2
# server_page2 <- function(input, output, session) {
#   # Server logic for Page 2
# }
# 
### Overall app structure
ui <- shinyUI(
  fluidPage(
    navbarPage(
      "Evening Grosbeak Interactive Platform",
      tabPanel("Home", ui_page1())#,
      #tabPanel("Page 2", ui_page2())
    )
  )
)
# 
server <- function(input, output, session) {
  # Server logic for the overall app
  # You can add common server logic here if needed
  callModule(server_page1, "page1")
  #callModule(server_page2, "page2")
}

### Run the app
shinyApp(ui = ui, server = server)
