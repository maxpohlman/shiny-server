library(shinythemes)
library(shiny)
library(sf)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shinyjs)
library(aws.s3)
library(markdown)
library(htmltools)
library(RColorBrewer)
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "")
s3load("markers.RData", bucket = "maxmarkerdata")
options(shiny.sanitize.errors = FALSE)


shinyApp( 
  
  ui =navbarPage( "Max's Map Projects", theme = shinytheme("cerulean"),
                  tabPanel("Visitor Locations",
                           sidebarPanel(
                             p('Welcome to my mapping/GIS shiny app! The above tabs are various small projects I am working on to teach 
                               myself various ways of analyzing and presenting spatial data in a user friendly and interactive way.'),
                             p("While you're here, why not prove it to the world? You can add markers to the map on the right by clicking
                               a location to get the Lat/Lng coordinates, then clicking the 'Add Location' button. You can also leave
                               your name and a message that appears when the marker is clicked."),
                             p("Feel free to add anything from your current location to your favorite pizza place to your most memorable
                               vacation spot, and everything inbetween!"),
                             radioButtons('base','Choose Basemap',c('Open Street Map' = "OpenStreetMap",
                                                                    'ESRI World Imagery' = 'Esri.WorldImagery',
                                                                    'Stamen Toner'='Stamen.Toner',
                                                                    'Earth at Night' = 'NASAGIBS.ViirsEarthAtNight2012',
                                                                    'ESRI Topographical Map' = 'Esri.WorldTopoMap')
                             )
                             ),
                           mainPanel(
                             p("Click and drag the map to pan locations and use the scroll wheel to zoom in and out."),
                             leafletOutput('l'),
                             p(),
                             fluidRow(
                               column(3,textInput('name',"Your Name",value = 'Anonymous')),
                               column(3,textInput('mess',"Your Message",value = 'Cool site!')),
                               br(),
                               br(),
                               column(5,textOutput('latlong'))
                             ),
                             
                             fluidRow(
                               column(2,actionButton('but', 'Add Location')),
                               br(),
                               column(2.5,span(textOutput('thanks'),style="color:blue"))
                             )
                             
                           )),
                  
                  
                  
                  
                  ###### GROWTH TAB #####
                  tabPanel("Visualizing Land Usage",
                           sidebarLayout(
                             sidebarPanel(
                               p('Rhode Island has designated areas within the state as "growth centers". These
                                 locations have been designated as prime areas for development due to having ',
                                 em('"a core of commercial and community services, residential development, and natural and built landmarks and boundaries that provide a sense of place." '),
                                 'You can find more about growth centers on',a(href="http://www.rigis.org/data/growth", "The RIGIS Website "), 'or '
                                 ,a(href="http://www.planning.ri.gov/documents/121/landuse2025.pdf", "The Rhode Island Land Use Plan.")),
                               p('This map plots the current classification of what the land within 1 mile of each growth center is being used for.
                                 Mousing over an area will show how much of that type of land exists near that specific growth center.'),
                               p('If you click on a marker, the table below will show you the distribution of land types around that particular growth center'),
                               p("If you're interested in replicating the map, the source code to create it can be found", a(href='https://github.com/maxpohlman/shiny-server/blob/master/Mapproj/Growth_Center_Map.r','here.')),
                               p(strong('The polygons take some time to load and reload upon changes, even after the updating bar disappears. Select an opacity to initialize the polygons.')),
                               actionButton('high',"Plot the polygons with high opacity"),
                               actionButton('low','Plot the Polygons with low opacity')),
                             mainPanel(
                               leafletOutput('gc'),
                               tableOutput('gctable')
                             )
                             )
                  ),
                  ###### Map TAB ######
                  tabPanel(HTML("GIS Mapping in R</a></li><li><a href=\"http://maxpohlman.com\">Back to my Website</a></li><li><a href=\"https://github.com/maxpohlman/shiny-server/blob/master/Mapproj/app.r\">View Source Code"),
                           fluidPage(
                             titlePanel("Creating Analytical Maps in R"),
                             includeMarkdown('az.md')
                           ))
                  ###### GEOM TAB ######
                  
                  
                  
  ),
  
  
  
  server = function(input,output,session){
    ############### DATA LOADING #################
    withProgress(message = 'Loading data', value = 1, {
      growth_cent <- st_read("ridata/growth06.shp")
      streams <- st_read("ridata/streams.shp")
      #muni <- st_read("ridata/muni97d.shp")
      lulc <- st_read("ridata/rilc11d.shp")
      #lulc$lu<-as.character(lulc$Descr_2011)
      #munix <- st_transform(muni,4326)
      growthx <- st_transform(growth_cent,4326)
      lulcx <- st_transform(lulc,4326)
      #growth_cent <- st_transform(growth_cent, 4326)
      #muni <- st_transform(muni, 4326)
      #streams <- st_transform(streams, 4326)
      #lulc <- st_transform(lulc,4326)
      #lu <- st_read("ridata/Land_Use_2025.shp")
      #road<- st_read('ridata/RIDOT_Roads_2016.shp')
      #busroutes <- st_read('ridata/RIPTA_Bus_Routes.shp')
      #pond <- st_read('ridata/Rhode_Island_Ponds_and_Lakes.shp')
      #census <- st_read('ridata/US_Census_2010_Summary_File_1_Indicators.shp')
      gcp<-st_read('ridata/growth_center_polygons.shp')
    })
    
    ############## INTRO STUFF #################
    
    output$l<-renderLeaflet({
      leaflet() %>% setView(lng = -79.0300287, lat = 41.8745644, zoom = 6) %>% addTiles() %>% 
        addMarkers(lat = mdata$latt, lng = mdata$lngg, label = mdata$labell, popup =mdata$popupp)
    })
    output$latlong<-renderText({'Click somewhere on the map!'})
    
    ## SELECTS BASEMAP
    observe({
      leafletProxy('l')%>% addProviderTiles(providers[[input$base]])
    })
    
    
    ## Gets/prints Lat/Long
    observeEvent(input$l_click,{
      click<<-input$l_click
      optxt<-paste("Lat: ", click$lat,",", "Long: ", click$lng)
      output$latlong<-renderText({optxt})
    })
    # Adds marker on click of button
    observeEvent(input$but,{
      
      tobind<-data.frame(latt=click$lat,lngg=click$lng,labell=input$name,popupp=input$mess, stringsAsFactors=FALSE)
      
      mdata<-rbind(mdata,tobind)
      s3save(mdata, bucket = "maxmarkerdata", object = "markers.RData")
      leafletProxy('l')%>%addMarkers(lat = tobind$latt, lng = tobind$lngg, label = tobind$labell, popup =tobind$popupp)
      output$thanks<-renderText({'Thank you for participating!'})
      
    })
    
    
    ############## Growth Stuff #################
    
    #Labels and colors and icons
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
    
    
    #plotting
    withProgress(message = "Updating maps", value = 1,{
      output$gc<-renderLeaflet({
        leaflet()%>% setView(lng = -71.5341141, lat = 41.7031914, zoom = 9) %>% addTiles() %>%
          addAwesomeMarkers(data = growthx, icon = icons, layerId = growthx$NAME, popup = growthx$NAME)
        
      })
    })
    
    #high res
    observeEvent(input$high,{
      withProgress(message = "Updating maps", value = 1,{
        leafletProxy('gc')  %>% clearShapes() %>%
          addPolygons(data=gcp, stroke = T, color = "black", weight = 2, fillColor = ~factpal(Descr_2011), fillOpacity = 1, smoothFactor = .2,
                      highlight = highlightOptions(   weight = 5,
                                                      color = "#666",
                                                      fillOpacity = .7,
                                                      bringToFront = TRUE),
                      label = labels,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
      })})
    #low res
    observeEvent(input$low,{
      withProgress(message = "Updating maps", value = 1,{
        leafletProxy('gc')  %>% clearShapes() %>%
          addPolygons(data=gcp, stroke = T, color = "black", weight = 2, fillColor = ~factpal(Descr_2011), fillOpacity = .2, smoothFactor = .2,
                      highlight = highlightOptions(   weight = 5,
                                                      color = "#666",
                                                      fillOpacity = .7,
                                                      bringToFront = TRUE),
                      label = labels,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
      })})
    
    #Table
    observeEvent(input$gc_marker_click,{
      townz<-input$gc_marker_click$id
      totable<-as.data.frame(gcp)
      totable<-subset(totable,name == townz, select = c('Descr_2011','area'))
      totable$Percentage<-totable$area/sum(totable$area)*100
      colnames(totable)<-c('Land Use','Area (Acres)','% of total land in the area')
      output$gctable<-renderTable({totable})  
      
    })
    
    
  }
)

