library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(tibble)
library(tibbletime)
library(motus)
library(lubridate)
library(RSQLite)
library(htmltools)

# setwd("/Users/gracebridge/repos/evening_grosbeak")

# GB start with EG data
# Ran 1 - R code package installs but do not run again, do not want to update R or any packages that would mess with code
# Ran 2 - modified to filter for Evening Grosbeak
# use static RDS file of saved Evening Grosbeak table - do not keep loading and working with motus (can do periodical updates to data)
eg_df <- readRDS("df_eg.RDS")
head (eg_df)

eg_df <- eg_df %>%
  mutate(motusTagDepID = paste(motusTagID, tagDeployID, sep = "."))
eg_df <- eg_df %>%
  relocate(motusTagDepID, .after = motusTagID)

selection <- eg_df %>%
  select(motusTagDepID, recvDeployLat, recvDeployLon, ts, tsCorrected, recvDeployName) %>%
  group_by(recvDeployName) %>%
  slice(1)

# eg_df <- eg_df %>%
#   collect()%>%
#   as.data.frame()%>%
#   mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

grouped_eg <- eg_df %>%
  mutate(motusTagDepID = paste(motusTagID, tagDeployID, sep = ".")) %>%
  group_by(motusTagDepID) %>%
  summarize(n_recv = n_distinct(recvDeployID))

test_bird <- eg_df %>%
  mutate(motusTagDepID = paste(motusTagID, tagDeployID, sep = ".")) %>%
  filter(motusTagDepID == 73326.44459) %>%
  group_by(recvDeployID) %>%
  slice(1)

# get birds from Adirondack region (active receiver locations) "recvDeployID"
# ID 7219 SUNY Potsdam - Bowman Hall (St. Lawrence)
# ID 9588 Upper and Lower Lakers WMA (St. Lawrence)
# ID 7762 Missisquoi Bay (Northeast VT)
# ID 7763 Shelburne Farms (Burlington VT)
# ID 9693 Dead Creek (Middlebury VT)
# ID 7270 Buckner2019 (Castleton VT - west of Lake George)
# ID 6124 Strawberry Fields Preserve (Amsertdam NY)
# ID 7238 Utica Zoo (Utica NY)
# ID 8937 Murcrest Farms (South of Carthage NY)
adks <- c(7219, 9588, 7762, 7763, 9693, 7270, 6124, 7238, 8937)

eg_adks <- eg_df %>%
  filter(eg_df$recvDeployID %in% adks)
head(eg_adks)

## another option would be setting up limits for longitude and latitude to decide on a region

# filter to get one bird
# In case there are multiple deployments for a tag, mutate so you are only looking at one bird (unique deployment of tag)
eg_adks <- eg_adks %>%
  mutate(motusTagDepID = paste(motusTagID, tagDeployID, sep = "."))
eg_adks <- eg_adks %>%
  relocate(motusTagDepID, .after = motusTagID)
# now look at motusTagDepID instead of motusTagID in case a tag was used on multiple birds
# choose one bird from data set - first one is motusTagDepID = 46991.31768
one_bird <- eg_adks %>%
  filter(motusTagDepID == 	
           73291.44424)
head(one_bird)

# map North America plot of one bird migration (using leaflet)
dat <- read.csv('https://static.lib.virginia.edu/statlab/materials/data/state_electricity_data_2018.csv')
head(dat)
m <- leaflet()
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels)  %>% 
  setView(lng = -96.25, lat = 39.50, zoom = 4)
m

mBird <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  setView(lat = 15, lng = 0, zoom = 1.5) %>% 
  # addCircleMarkers(data = test_bird,
   #          lng = ~tagDepLon,
   #          lat = ~tagDepLat,
    #         color = "blue") %>%
  addCircleMarkers(data = test_bird,
                   lng = ~recvDeployLon,
                   lat = ~recvDeployLat,
                   color = "red",
                   clusterOptions = markerClusterOptions())
mBird


# For meeting 2/8
# prep the data
# take each bird - filter it down to its first instance at a location
# plot all of those on a map
# do MarkerClusters
# figure out how to pull the bird info from each point (if you click on the point, there should be a pop-up)
# want to be able to then subset your map into that one bird that you clicked on, make sure finding a unique location
# user is interacting with the map to choose a bird
# **Build this first**Given a bird/location, then do the flying picture (its route, might be a static map, like ggmap or some other static map)
# Could have choice for time period of route

# If all above works, animate



# Given a bird/location, map out its route (might just be a static map)
## filter to get one bird (out of all evening grosbeaks, not just adks)
## using test_bird from above
### so prep the data so that it is in chronological order
head(test_bird)
test_bird <- test_bird %>%
  arrange(., tsCorrected) # using tsCorrected because it is a double
# trim the data down to one time period
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-02-01")
test_bird <- test_bird %>%
  filter(ts >= start_date, ts <= end_date)

# All birds with one point for their starting deployment site
first_deployment_df <- eg_df %>%
  arrange(., tsCorrected)
first_deployment_df <- first_deployment_df %>%
  group_by(motusTagDepID) %>%
  slice(1)
deployment_starts_map <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  setView(lat = 15, lng = 0, zoom = 1.5) %>% 
  addCircleMarkers(data = first_deployment_df,
                   lng = ~tagDepLon,
                   lat = ~tagDepLat,
                   color = "red",
                   clusterOptions = markerClusterOptions(),
                   popup = htmlEscape(~motusTagDepId))
deployment_starts_map
# For shiny app: register clicks
# https://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny
# then plot route of the bird clicked
## plot the route (one bird)
routeBird <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  setView(lat = 15, lng = 0, zoom = 1.5) %>% 
  # addCircleMarkers(data = test_bird,
  #          lng = ~tagDepLon,
  #          lat = ~tagDepLat,
  #         color = "blue") %>%
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
routeBird
# focus on producing correct data table (limit how many rows are displayed at a time, but could also download whole dataset)
# dt package (different from data table package) (download buttons and can control how it is displayed)

## mapbox - static map






# pick one bird and map out its travel path
# bring it down to at most 2 observations at each receiving site (when it was first there and when it left there)
# Can start with when they got there to trim it down
# when slicing, arrange by date/time before you slice
# or do slice_min of the date after the group_by

# How long is it staying in a site before it moves on? 
# focus on deployment sites on a map, making it for the reseacher, not the watcher

# Maybe: Make like a timeline animation? Watch it go through its migration... maybe not leaflets, non-interactive map structure, gg animation mapping it around
# should be able to click on a bird and see which bird it is and focus on that one

# set up filtering options that would work in future for toggling in shiny app
# by region, by deployment site, by individual bird, by device, by signal, by time stamp





# For after spring break
# trim these two datasets together to connect, rename lat and lon so they have the same variable name
# group_by at location so bird only shows up once at each place (group by lat, lon)
# mutate, so that arrival date and end date at each location are consistent (look like summarize statements, but use mutate instead so that it is always the same value)
# slice 1
# Put the date in the pop-up
# Select any other variables needed to put in pop-up
# image in pop-up of evening grosbeak ? custom markers 

# use one_bird as used in shiny app
firstDep <- one_bird %>%
  slice(1)
# take out this observation from one_bird data set
one_bird <- one_bird[-1, ]

# rename lat and long for each data set
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
firstDepCopy <- firstDep %>%
  select(motusTagDepID, lat, lon, tsCorrected, ts)
one_birdCopy <- one_bird %>%
  select(motusTagDepID, lat, lon, tsCorrected, ts)
full_bird <- bind_rows(firstDepCopy, one_birdCopy)
full_bird

# # Clean up - make sure we don't have multiple points for the bird just sitting in one location
# prev_lat <- full_bird$lat[1]
# prev_lon <- full_bird$lon[1]
# 
# for (i in 2:nrow(full_bird)) {
#   current_lat <- full_bird$lat[i]
#   current_lon <- full_bird$lon[i]
#   
#   if (!is.na(current_lat) && !is.na(current_lon) && !is.na(prev_lat) && !is.na(prev_lon)) {
#     if (current_lat == prev_lat && current_lon == prev_lon) {
#       # if it matches the previous coordinates, delete it
#       full_bird <- full_bird[-i, ]
#     } else {
#       prev_lat <- current_lat
#       prev_lon <- current_lon
#     }
#   }
#   
# }

# filter by lat, lon and get all the birds on the click (all the birds that were deployed from the same spot) (subset winter roost group dataset)
# with that pack of bird ids, plot where they went after winter (color coded by bird)
# choose a year, find where the birds are together in the winter (google when winter starts for them)
# nested tabs

# full_bird is data of the bird (deployment and receivements)

# 1. Clean up one bird map
# make new data set that only includes select data from one bird from full_bird with unique timing spots
new_full_bird <- full_bird %>%
  group_by(lat, lon) %>%
  slice(1)


# 2. Group by winter roosters
# start with whole data set, and filter down to the winter months to see where they are staying
winter <- eg_df %>%
  filter(month(ts) %in% c(1, 2, 12)) %>% # dec, jan, feb
  group_by(motusTagDepID, recvDeployLat, recvDeployLon) %>% # just one winter spot for each bird
  slice(1)

winterLocations <- winter %>%
  group_by(recvDeployName) %>%
  slice(1)

# 3. Work on nested tabs

