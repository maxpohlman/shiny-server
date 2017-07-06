##Growth Center Land Use by Max Pohlman

## This script creates the map located at https://maxpohlman.shinyapps.io/mapproj/ (Visualizing Land Usage tab).
## All the data is downloaded within this script.

#Required packages
library(sf)
library(leaflet)
library(htmltools)
library(RColorBrewer)
library(dplyr)

#Downloads the data
if(!dir.exists("growthdata")){dir.create("growthdata")}
download.file(url = 'http://www.rigis.org/geodata/plan/rilc11d.zip',
              destfile = "growthdata/a.zip")
unzip(zipfile = "growthdata/a.zip",
      exdir = "growthdata")
lulc <- st_read("growthdata/rilc11d.shp")

download.file(url = 'http://www.rigis.org/geodata/plan/growth06.zip',
              destfile = "growthdata/a.zip")
unzip(zipfile = "growthdata/a.zip",
      exdir = "growthdata")
lulc <- st_read("growthdata/growth06.shp")

growthx <- st_transform(growth_cent,4326)
lulcx <- st_transform(lulc,4326)

#creates an index where each element is a set of multipolygons 1 mile from a growth center
op<-st_multipolygon()
for(i in 1:length(growthx$Cntr_Class)){
  op[[i]]<- growthx[i,] %>%
    st_buffer(1/69) %>%
    st_intersection(lulcx) %>%
    group_by(Descr_2011) %>%
    summarize(area = sum(Acres_2011))
  op[[i]]$name<-growthx[i,]$NAME
}

#Merges all the multipolygons into a giant multipolygon df
opp<-rbind(op[[1]],op[[2]])
for (i in 3:length(op))
{
  opp<-rbind(opp,op[[i]])
}

#Icons, colors, labels
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
factpal <- colorFactor(col_vector[1:37],growthx$Descr_2011)
icons <- awesomeIcons(
  icon = 'leaf',
  iconColor = '#4eff3a',
  library = 'fa',
  markerColor = 'red'
                      )
labels <- sprintf(
  "<strong>%s</strong><br/>%g acres",
  gcp$Descr_2011, gcp$area
                  ) %>% lapply(htmltools::HTML)



#Plots!
q<-leaflet() %>% setView(lng = -71.5341141, lat = 41.7031914, zoom = 10) %>% addTiles() %>%
  addPolygons(data=gcp, stroke = T, color = "black", weight = 2, fillColor = ~factpal(Descr_2011), fillOpacity = .1, smoothFactor = .2,
              highlight = highlightOptions(   weight = 5,
                                              color = "#666",
                                              fillOpacity = .7,
                                              bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addAwesomeMarkers(data = growthx, icon = icons, popup = growthx$NAME)
q