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
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "")
s3load("markers.RData", bucket = "maxmarkerdata")
options(shiny.sanitize.errors = FALSE)


shinyApp( 

  ui =navbarPage( "Max's Map Project", theme = shinytheme("cerulean"),
                  tabPanel("Locations",
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
                             sidebarPanel(p(strong('This map takes a couple seconds to load and reload upon changes. Select an opacity to initialize the polygons')),
                                          p('Rhode Island has designated areas within the state as "growth centers". These
                                            locations have been designated as prime areas for development due to having ',
                                            em('"a core of commercial and community services, residential development, and natural and built landmarks and boundaries that provide a sense of place." '),
                                            'You can find more about growth centers on',a(href="http://www.rigis.org/data/growth", "The RIGIS Website "), 'or '
                                            ,a(href="http://www.planning.ri.gov/documents/121/landuse2025.pdf", "The Rhode Island Land Use Plan.")),
                                          p('This map plots the current classification of what the land within 1 mile of each growth center is being used for.
                                            Mousing over an area will show how much of that type of land exists near that specific growth center.'),
                                          p('If you click on a marker, the table below will show you the distribution of land types used for that particular marker'),
                                          p("If you're interested in replicating the map, the source code to create it can be found", a(href='https://github.com/maxpohlman/shiny-server/blob/master/Mapproj/Growth_Center_Map.r','here.')),
                                          radioButtons('op', 'Choose opacity level for the polygons', c('High' = 'High','Low'='Low'), selected = character(0))),
                             mainPanel(
                               leafletOutput('gc'),
                               tableOutput('gctable')
                             )
                           )
                           ),
                  ###### Map TAB ######
                  tabPanel("GIS in R",
                           fluidPage(
                             titlePanel("Creating Analytical Maps in R"),
                             includeMarkdown('az.md')
                           )),
                  ###### GEOM TAB ######
                
                  tabPanel(HTML("geom_sf practice</a></li><li><a href=\"http://maxpohlman.com\">Back to my website</a></li><li><a href=\"https://github.com/maxpohlman/shiny-server/blob/master/Mapproj/app.r\">View source code"),
                                    
                                    sidebarLayout(
                                      sidebarPanel( shinyjs::useShinyjs(),
                                                    p("This was my very first project in teaching myself how to work with shapefiles in R.", strong("Some features are disabled for the whole state due to computing constraints."),
                                                      "Things are added in the order you select them. If something disappears, uncheck and re-check the boxes"),
                                                    selectInput("scale", label = "What do you want to plot", choices = c('Entire State',
                                                                                                                         'County',
                                                                                                                         'Municipality'),
                                                                selected = 'Entire State'),
                                                    selectInput("smallscale", label = "Specific County or Municipality", choices = 'Entire State',
                                                                selected = 'Entire State'),
                                                    span(strong(textOutput('statewarning')), style="color:red"),
                                                    checkboxInput("lu", label = "Show types of land use?", FALSE),
                                                    checkboxInput("rivers", label = "Show rivers?", FALSE),
                                                    checkboxInput("roads", label = "Show roads?", FALSE),
                                                    checkboxInput("pond", label = "Show ponds and lakes?", FALSE),
                                                    checkboxInput("busrt", label = "Show bus routes?", FALSE)
                                                    #selectInput("landuse", label = "Type of land use", choices = c('Clear map', sort(unique(lulc$lu))))
                                      ),
                                      #Adds plot area
                                      mainPanel(
                                        plotOutput('plot')
                                        
                                      ))
                                    
                                    
                           )
                             
                           ),
                             
   
  
  server = function(input,output,session){
    ############### DATA LOADING #################
    withProgress(message = 'Loading data', value = 1, {
      growth_cent <- st_read("ridata/growth06.shp")
      streams <- st_read("ridata/streams.shp")
      muni <- st_read("ridata/muni97d.shp")
      lulc <- st_read("ridata/rilc11d.shp")
      lulc$lu<-as.character(lulc$Descr_2011)
      munix <- st_transform(muni,4326)
      growthx <- st_transform(growth_cent,4326)
      lulcx <- st_transform(lulc,4326)
      growth_cent <- st_transform(growth_cent, 4326)
      muni <- st_transform(muni, 4326)
     streams <- st_transform(streams, 4326)
      lulc <- st_transform(lulc,4326)
      lu <- st_read("ridata/Land_Use_2025.shp")
      road<- st_read('ridata/RIDOT_Roads_2016.shp')
      busroutes <- st_read('ridata/RIPTA_Bus_Routes.shp')
      pond <- st_read('ridata/Rhode_Island_Ponds_and_Lakes.shp')
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
      factpal <- colorFactor(topo.colors(37),growthx$Descr_2011)
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
      output$gc<-renderLeaflet({
        leaflet()%>% setView(lng = -71.5341141, lat = 41.7031914, zoom = 9) %>% addTiles() 
          # addPolygons(data=gcp, stroke = T, color = "black", weight = 2, fillColor = ~factpal(Descr_2011), fillOpacity = 1, smoothFactor = .2,
          #             highlight = highlightOptions(   weight = 5,
          #                                             color = "#666",
          #                                             fillOpacity = .7,
          #                                             bringToFront = TRUE),
          #             label = labels,
          #             labelOptions = labelOptions(
          #               style = list("font-weight" = "normal", padding = "3px 8px"),
          #               textsize = "15px",
          #               direction = "auto")) %>%
          # addAwesomeMarkers(data = growthx, icon = icons, layerId = growthx$NAME, popup = growthx$NAME)
          # 
      })
      
       observe({ 
         if(!is.null(input$op)){
         if(input$op == 'High'){
           opac <-1

         }
         if(input$op == 'Low'){
           opac <-.1

         }
         withProgress(message = 'Rendering maps', value = 1, {
           
         leafletProxy('gc')  %>% clearShapes() %>%
           addPolygons(data=gcp, stroke = T, color = "black", weight = 2, fillColor = ~factpal(Descr_2011), fillOpacity = opac, smoothFactor = .2,
                       highlight = highlightOptions(   weight = 5,
                                                       color = "#666",
                                                       fillOpacity = .7,
                                                       bringToFront = TRUE),
                       label = labels,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px",
                         direction = "auto"))  %>%
              addAwesomeMarkers(data = growthx, icon = icons, layerId = growthx$NAME, popup = growthx$NAME)
         })
         }
         
       })
       
       #Table
       observeEvent(input$gc_marker_click,{
         townz<-input$gc_marker_click$id
         totable<-as.data.frame(gcp)
         totable<-subset(totable,name == townz, select = c('Descr_2011','area'))
         totable$Percentage<-totable$area/sum(totable$area)*100
         colnames(totable)<-c('Land Use','Area (Acres)','% of total land in the area')
         output$gctable<-renderTable({totable})  
         
       })
            
      
      
      
    #   
    #   
    #   
    # 
    # 
    # ############## GEOM STUFF #################
    # 
    # #################################
    # # Observes for map geometry     #
    # #################################

    observe({

      if (input$scale == 'Entire State'){
        updateSelectInput(session, "smallscale",
                          choices = 'Entire State')
        shinyjs::disable("lu")
        shinyjs::disable("roads")
        output$statewarning<-renderText({"WARNING: Plotting anything for the entire state will take extra time"})
      }
      if (input$scale == 'County'){
        updateSelectInput(session, "smallscale",
                          choices = sort(unique(muni$COUNTY)))
        output$statewarning<-renderText({""})
        shinyjs::enable("lu")
        shinyjs::enable("roads")
      }
      if (input$scale == 'Municipality'){
        updateSelectInput(session, "smallscale",
                          choices = sort(unique(muni$NAME)))
        output$statewarning<-renderText({""})
        shinyjs::enable("lu")
        shinyjs::enable("roads")
      }
          })

    #################################
    # Filters and plots boundaries  #
    #################################
    output$plot <- renderPlot({if (input$scale == 'Entire State'){
      boundaries<-muni
    }
    else{
      if(input$scale == 'County'){
        boundaries<-subset(muni,COUNTY == input$smallscale)
                                  }
      else{
        if(input$scale == 'Municipality'){
          boundaries<-subset(muni,NAME == input$smallscale)
                                          }
          }
        }
      p<- ggplot() +
           geom_sf(data = boundaries)


      #################################
      # Filters and plots Rivers and roads#
      #################################
      withProgress(message = 'Updating map', value = 1, {
      if(input$lu == TRUE){
        luu <- lu %>% st_intersection(boundaries)
        p<- p+geom_sf(data=luu, aes(fill = Map_Legend))
      }
      if(input$rivers == TRUE){
      buff <- streams %>% st_intersection(boundaries)
      p<- p + geom_sf(data=buff, col = 'Blue')
      }
      if(input$roads == TRUE){
      rroads <- road %>% st_intersection(boundaries)
      p<- p + geom_sf(data=rroads, col = 'bisque4')
      }
      if(input$busrt == TRUE){
        busrts<- busroutes %>% st_intersection(boundaries)
        p<- p+ geom_sf(data = busrts, aes(color = RT_NAME), size = 1.5)
      }
      if(input$pond == TRUE){
        pondss<- pond %>% st_intersection(boundaries)
        p<- p+ geom_sf(data = pondss, fill = 'dodgerblue2', color = 'dodgerblue2')
      }
      })
      #################################
      # Filters and plots Land use    #
      #################################
      #if(input$landuse != 'Clear map'){
       # lubuff <- st_intersection(lulc$geometry[lulc$lu == input$landuse], boundaries)
        #p <- p + plot(lubuff, col = 'beige', add = TRUE )
      #}



      p<-p + scale_fill_brewer(palette = 'Greens')
      print(p)
                            }, height = 800, width = 1000)
  }
)
  
