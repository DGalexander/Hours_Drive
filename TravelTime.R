## Set WD
setwd("~/R Projects/TravelTime")

## Load packages
library("GISTools")
library("rgeos")
library("rgdal")
library("maptools")
library("spatstat")
library("raster")

## Load PDNP Layer .shp
PDNP <- readShapePoly("~/R Projects/TravelTime/Boundary/PDNP.shp")
# Take a look
plot(PDNP)

## Create a 1km Buffer
PDNP.buffer <- gBuffer(PDNP, width = 1000)
# Take a look
plot(PDNP.buffer, add =T, border = "green")

## Load OS GB Roads Data (NN = 100km) Clipped and combined the NN for the PDNP)
roads_SD <- readShapeLines("~/R Projects/TravelTime/oproad_essh_gb/SD_CLIP.shp")
roads_SE <- readShapeLines("~/R Projects/TravelTime/oproad_essh_gb/SE_CLIP.shp")
roads_SJ <- readShapeLines("~/R Projects/TravelTime/oproad_essh_gb/SJ_CLIP.shp")
roads_SK <- readShapeLines("~/R Projects/TravelTime/oproad_essh_gb/SK_CLIP.shp")

#### SHOULD WRITE A FUNCTION FOR THIS ####

#### Roads SD
## Logical = A Roads
roads.ARoads <- (roads_SD$class == "A Road")
## Subset the SLDF A roads
roads.ARoads <- roads_SD[roads.ARoads,]
## Spatial Clip to PDNP Buffer
PDNP.Aroads_SD <- gIntersection(PDNP.buffer, roads.ARoads, byid = TRUE)

#### Roads SE
## Logical = A Roads
roads.ARoads <- (roads_SE$class == "A Road")
## Subset the SLDF A roads
roads.ARoads <- roads_SE[roads.ARoads,]
## Spatial Clip to PDNP Buffer
PDNP.Aroads_SE <- gIntersection(PDNP.buffer, roads.ARoads, byid = TRUE)

#### Roads SJ
## Logical = A Roads
roads.ARoads <- (roads_SJ$class == "A Road")
## Subset the SLDF A roads
roads.ARoads <- roads_SJ[roads.ARoads,]
## Spatial Clip to PDNP Buffer
PDNP.Aroads_SJ <- gIntersection(PDNP.buffer, roads.ARoads, byid = TRUE)

#### Roads SK
## Logical = A Roads
roads.ARoads <- (roads_SK$class == "A Road")
## Subset the SLDF A roads
roads.ARoads <- roads_SK[roads.ARoads,]
## Spatial Clip to PDNP Buffer
PDNP.Aroads_SK <- gIntersection(PDNP.buffer, roads.ARoads, byid = TRUE)

#### ####

## Remove the roads data as it is >30mb ;)
rm(roads_SD, roads_SE, roads_SJ, roads_SK)

## Merge Data
PDNP.Aroads <- rbind(PDNP.Aroads_SD, PDNP.Aroads_SE, PDNP.Aroads_SJ, PDNP.Aroads_SK)
# Take a look
plot(PDNP.Aroads, add = T)

## Remove more roads data 
rm(PDNP.Aroads_SD, PDNP.Aroads_SE, PDNP.Aroads_SJ, PDNP.Aroads_SK, roads.ARoads)


## Find Points where A roads intersect the PDNP Boundary

PDNP.Poly <- as(PDNP, "owin")
PDNP.Aroads.Line <- as(PDNP.Aroads, "psp")
PDNP.Poly.edges <- edges(PDNP.Poly)
xPoints <- crossing.psp(PDNP.Aroads.Line, PDNP.Poly.edges)
xPoints <- xPoints[c(1:58)]

## Plot the access points to view their locations on a map.

plot(xPoints, add = TRUE, col = "red", pch = 20, cex = 3)

## Now we convert these access points from X Y location to Latitude and Longitude; 
## the format used to query Route360 API engine

# Define Spatial reference http://spatialreference.org/ref/epsg/osgb-1936-british-national-grid/proj4/
library(proj4)
xPoints <- as.data.frame(xPoints)
proj4string <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
## proj4string <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
## Transform data
pj <- project(xPoints, proj4string, inverse=TRUE)
latlon <- data.frame(lat=pj$y, lon=pj$x)
print(latlon)

# Convert to Spatial Polygons 
library("geojsonio")
Catchment <- geojson_read("drive_hour.geojson", method = "local", what = "sp")
# buffer by an extra 1km (r360 free plan only 200m)
Catchment <- gBuffer(Catchment, width = 1000)
# Change the projection
Catchment <- spTransform(Catchment, proj4string)
plot(Catchment)
plot(PDNP, add = T, col = "green")


#### Get the External Data ####
IMD_2015 <-geojson_read("https://opendata.arcgis.com/datasets/ee0226d33b62409ba2f2b0f99404190a_0.geojson", what = "sp")
IMD_2015 <- spTransform(IMD_2015, proj4string)

# Subset with the hours drive cos it's massive :)
IMD.HoursDrive <- gIntersection(Catchment, IMD_2015)
# Import as .shp
IMD.HoursDrive <- shapefile("IMD.HoursDrive.shp")


# Check what it looks like
plot(IMD.HoursDrive, add = T, col = "green")


#### Lets Map ####
library(leaflet)

## Set the data to numeric
IMD.HoursDrive$'IMDDec5' <- as.numeric(IMD.HoursDrive$'IMDDec5')

## Create the color bin
pal <- colorBin("RdYlBu", domain = IMD.HoursDrive$'IMDDec5', n=10)

## Create a Popup
LSOA_popup <- paste0("<strong>LSOA: </strong>",
                     IMD.HoursDrive$'LSOA11NM',
                     "<br><strong>IMD Rank (where 1 is most deprived): </strong>",
                     IMD.HoursDrive$'IMDRank5',
                     "<br><strong>IMD Decile (% Most Deprived): </strong>",
                     IMD.HoursDrive$IMDDec5)

mymap <- leaflet(IMD.HoursDrive) %>%
  setView(lng = -1.7, lat = 53.33, zoom = 9) %>% addTiles() %>%
  addPolygons(stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, 
              fillColor = ~pal(IMD.HoursDrive$IMDDec5), popup = LSOA_popup) %>%
  addLegend("bottomright", pal = pal, values = ~'IMD Decile',
            title = "Decile (10=Least Deprived)",
            opacity = 1)

library(htmlwidgets)
saveWidget(widget = mymap, file = 'HoursDriveDeprivation.html')
