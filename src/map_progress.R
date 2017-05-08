####################
# Map - Interactive 
# Veronica Lee
####################

library(devtools)
devtools::install_github("rstudio/leaflet")

# Get Nevada's map coordinates
-------------------------------------
  
library(maps)
library(leaflet)
mapnv = map("state", region = c("Nevada"), fill = TRUE, plot = FALSE)

# Get Nevada's county-level shape files from U.S. census department
-------------------------------------
library(tigris)

counties_NV <- counties(state="Nevada")
list_counties(state="Nevada")
LV <- counties_NV@data[counties_NV@data$GEOID == '32003',] # 32= STATEFP of Nevada; 003 = County FP of Clark County 

#  Load a vector map of Clark boroughs
#Las Vegas is the city located within Clark County of the state of Nevada. 
#LV (city) - Clark (county) - NV (state)

library(rgdal)
setwd("C:/Users/Veronica/Desktop/Spring 2017/Data Visualization 4063/final")

#load Nevada County Level Shape File  
NV_cb <- readOGR("./shape","cb_2016_32_tract_500k", verbose = FALSE) # County level file from cencus.gov 
NV_z <- readOGR("./shape/ZillowNeighborhoods-NV","ZillowNeighborhoods-NV", verbose = FALSE) #Ziller Neighoborhoods Data of Nevada

#Subset a Las Vegas map by neighborhood-level from above
-------------------------------------
LV_z <- subset(NV_z, NV_z$City %in% c("Las Vegas"))   
#Subset a Vegas map by neighborhood-level from above
two_z <- subset(LV_z, LV_z$Name %in% c("The Strip", "Downtown"))
two_z@data$r_count <- ifelse(two_z@data$Name == "The Strip", 818, 340) 
two_z@data$avg_rating <- ifelse(two_z@data$Name == "The Strip", '3.32/5.00', '3.65/5.00') 

#make LV map by neighborhood level - add neighborhood name
-------------------------------------
p <- leaflet(LV_z) %>% setView(-115.14,36.16,10) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
             popup = paste("<b>Neighborhood:</b>",LV_z$Name, "<br/>"),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
              
# Highlight two selected neighborhoods : The Strip & Downtown on the LV map 

p <- p %>% addPolygons(data = two_z, color = "#c41200", weight = 1, smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.3,   
                  popup = paste("<b>Neighborhood:</b>",two_z$Name, "<br/>",
                                "<b>Restaurant Count:</b>",two_z$r_count,"<br/>",
                                "<b>Avg. Rating:</b>",two_z$avg_rating,"<br/>"),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))


# Base map, highlighting two neighborhoods for comparison 
p 

# Text label two selected neighborhoods : The Strip & Downtown on the LV map 
#p %>% addMarkers(
#  lng = -115.14, lat = 36.17,
#  label = "The Strip",
#  labelOptions = labelOptions(noHide = T, textOnly = TRUE, 
#                              style = list(
#                                "color" = "blue",
#                                "font-size" = "10px"))) %>% 
#  addMarkers(
#    lng = -115.14, lat = 36.15,
#    label = "Downtown",
#    labelOptions = labelOptions(noHide = T, textOnly = TRUE, 
#                                style = list(
#                                  "color" = "blue",
#                                  "font-size" = "10px")))

# Load Las Vegas Business Data Frame
lv_b <- readr::read_csv("./LV_bis.csv")

# Add restaurant scatter as circles on a map 
library(RColorBrewer)
pal = colorFactor("RdYlGn", domain = lv_b$stars)
color_rating <- pal(lv_b$stars)

p2 <- p %>% setView(-115.14,36.16,10) %>% addTiles() %>% 
  addCircles(data = lv_b, lng = ~longitude, lat = ~latitude, col = color_rating, 
                 popup = paste("<b>Neighborhood:</b>",lv_b$neighborhood, "<br/>",
                               "<b>Name:</b>",lv_b$name, "<br/>",
                               "<b>Rating:</b>",lv_b$rate_range,"<br/>",
                               "<b>Price:</b>",lv_b$price_range,"<br/>")) 

p2 %>% # Adding Legend 
 addLegend("bottomright",
                       pal = colorFactor("RdYlGn", domain = lv_b$stars), values = ~lv_b$stars,
                       title = "Restaurant Rate in Las Vegas", opacity = 0.5)


# Clustering restaurants in a LV map 
m <- leaflet(lv_b) %>% setView(-115.14,36.16,10) %>% addTiles() %>%
   addCircleMarkers(data = lv_b, lng = ~longitude, lat = ~latitude, color=color_rating,
                        popup = paste("<b>Neighborhood:</b>",lv_b$neighborhood, "<br/>",
                                      "<b>Name:</b>",lv_b$name, "<br/>",
                                      "<b>Rating:</b>",lv_b$rate_range,"<br/>",
                                      "<b>Price:</b>",lv_b$price_range,"<br/>"),
                          clusterOptions = markerClusterOptions())  

# Add neighborhood polygons on a LV map with clustering of restaurants 

m <- m %>% 
  addPolygons(data = LV_z, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              popup = paste("<b>Neighborhood:</b>",LV_z$Name, "<br/>"),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = F)) %>%
  addPolygons(data = two_z, color = "#c41200", weight = 1, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.3,   
              popup = paste("<b>Neighborhood:</b>",two_z$Name, "<br/>",
                            "<b>Restaurant Count:</b>",two_z$r_count,"<br/>",
                            "<b>Avg. Rating:</b>",two_z$avg_rating,"<br/>"),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = FALSE)) 
# Add legend to the map 
m %>% addLegend("bottomright",
            pal = colorFactor("RdYlGn", domain = lv_b$stars), values = ~lv_b$stars,
            title = "Restaurant Rate in Las Vegas", opacity = 0.5)


View(lv_b)

library(rgdal)
library(sp)


addMarkers(
  clusterOptions = markerClusterOptions()
)  

# Create "overall edu" variable equivalent to college or higher degree pop % to make five variables aksed by a question
edu_long_cty <- edu_long_cty %>% mutate(overall_edu=coll)
# Subsetting the data for New York State 
edu_long_ny <- edu_long_cty %>% filter(state=="NY") 
head(edu_long_ny)
str(edu_long_ny)
edu_long_ny %>% group_by(state) %>% tally() # total 62 counterparties in the data 
names(edu_long_ny)

#Add edu_long_ny data to shapefile 
combined <- counties_NY@data %>% left_join(edu_long_ny, by = "GEOID")
counties_NY@data <- combined 
names(counties_NY@data)
head(counties_NY@data)

View(LV_cb@data)
# Create a map widget of New York State by county level 
m <- leaflet(data = LV) %>% 
  addTiles() %>%
#  setView(lat= 36.4303079, lng=-82.98566, zoom=5)%>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

# Adding Legend 
m <- m %>% addLegend("bottomleft",
                     pal = colorNumeric("RdYlGn", counties_NY$coll), values = ~coll,
                     title = "% Education Level<br> by County in NY", opacity = 0.5)


# Show/Hide layers
m <- m  %>% 
  # Base groups = Background layer
  #addTiles(group = "OpenStreetMap") %>%
  #addProviderTiles(providers$Stamen.Toner, group = "Toner")  %>%
  #addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  # Data Layers
  # First Data Layer: overall edu
  addPolygons(group="Overall Education Level",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, 
              color = ~colorNumeric("RdYlGn", overall_edu)(overall_edu),
              popup = paste(#"State:",counties_NY$state, "<br/>",
                "County:",counties_NY$area_name,"<br/>",
                "Overall Education:",round(counties_NY$overall_edu,1),"%","<br/>"#,
                #"less than HS:",round(counties_NY$lessHS,1),"%","<br/>",
                #"High School:",round(counties_NY$HS,1),"%","<br/>",
                #"Some College:",round(counties_NY$somecoll,1),"%","<br/>",
                #"College or Higher:",round(counties_NY$coll,1),"%","<br/>"
              )) %>%
  # Second Data Layer: less than HS
  addPolygons(group="Less than HS",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, 
              color = ~colorNumeric("RdYlGn", lessHS)(lessHS),
              popup = paste(#"State:",counties_NY$state, "<br/>",
                "County:",counties_NY$area_name,"<br/>",
                # "Overall Education:",round(counties_NY$overall_edu,1),"%","<br/>",
                "less than HS:",round(counties_NY$lessHS,1),"%","<br/>"#,
                #"High School:",round(counties_NY$HS,1),"%","<br/>",
                #"Some College:",round(counties_NY$somecoll,1),"%","<br/>",
                #"College or Higher:",round(counties_NY$coll,1),"%","<br/>"
              )) %>% 
  # Third Data Layer: High School
  addPolygons(group="High School",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, 
              color = ~colorNumeric("RdYlGn", HS)(HS),
              popup = paste(#"State:",counties_NY$state, "<br/>",
                "County:",counties_NY$area_name,"<br/>",
                # "Overall Education:",round(counties_NY$overall_edu,1),"%","<br/>",
                #"less than HS:",round(counties_NY$lessHS,1),"%","<br/>"#,
                "High School:",round(counties_NY$HS,1),"%","<br/>"#,
                #"Some College:",round(counties_NY$somecoll,1),"%","<br/>",
                #"College or Higher:",round(counties_NY$coll,1),"%","<br/>"
              )) %>% 
  # Fouth Data Layer: High School
  addPolygons(group="Some College",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, 
              color = ~colorNumeric("RdYlGn", somecoll)(somecoll),
              popup = paste(#"State:",counties_NY$state, "<br/>",
                "County:",counties_NY$area_name,"<br/>",
                # "Overall Education:",round(counties_NY$overall_edu,1),"%","<br/>",
                #"less than HS:",round(counties_NY$lessHS,1),"%","<br/>"#,
                #"High School:",round(counties_NY$HS,1),"%","<br/>"#,
                "Some College:",round(counties_NY$somecoll,1),"%","<br/>"#,
                #"College or Higher:",round(counties_NY$coll,1),"%","<br/>"
              )) %>% 
  # Fifth Data Layer: College and Higher
  addPolygons(group="College and Higher",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, 
              color = ~colorNumeric("RdYlGn", coll)(coll),
              popup = paste(#"State:",counties_NY$state, "<br/>",
                "County:",counties_NY$area_name,"<br/>",
                # "Overall Education:",round(counties_NY$overall_edu,1),"%","<br/>",
                #"less than HS:",round(counties_NY$lessHS,1),"%","<br/>"#,
                #"High School:",round(counties_NY$HS,1),"%","<br/>"#,
                #"Some College:",round(counties_NY$somecoll,1),"%","<br/>"#,
                "College and Higher:",round(counties_NY$coll,1),"%","<br/>"
              )) %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Toner", "Toner Lite"),
    overlayGroups = c("Overall Education Level","Less than HS","High School","Some College","College and Higher"),
    options = layersControlOptions(collapsed = TRUE) )


# Simple addition of a minimap
#m %>% addMiniMap()
# Some more controls
m <-m %>% addMiniMap(tiles = providers$Esri.WorldStreetMap,toggleDisplay = TRUE)

# Locate me button
m <- m %>%
  addEasyButton(easyButton(
    icon="fa‐globe", title="Zoom to Entire World Map",
    onClick=JS("function(btn, map){ map.setZoom(1.5); }"))) %>%
  addEasyButton(easyButton(
    icon="fa‐crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
m