library(shinythemes)
library(shiny)
library(sf)
library(ggplot2)
library(dplyr)
library(httr)
library(leaflet)
library(shinyjs)
mdata<-readRDS('introdata/markers.rds')

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
                  
                  
                  
                  
###### GEOM TAB ######
                  tabPanel("geom_sf practice",
                           sidebarLayout(
                             sidebarPanel( shinyjs::useShinyjs(),
                               p("This is a personal mapping project I am doing to teach myself GIS.", strong("Some features are disabled for the whole state due to computing constraints."),
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
                               
                             ))),
                           tabPanel(HTML("Leaflet practice (NYI)</a></li><li><a href=\"http://maxpohlman.com\">Back to my website</a></li><li><a href=\"https://github.com/maxpohlman/shiny-server/blob/master/Mapproj/app.r\">View source code"),
                                    
                                    mainPanel(
                                      tableOutput('tabo')
                                      
                                    )
                                    
                                    
                                    
                           )
                             
                           ),
                             
   
  
  server = function(input,output,session){
    
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
        saveRDS(mdata,'introdata/markers.rds')
        leafletProxy('l')%>%addMarkers(lat = tobind$latt, lng = tobind$lngg, label = tobind$labell, popup =tobind$popupp)
        output$thanks<-renderText({'Thank you for participating!'})
      
    })
    

    
    ############## GEOM STUFF #################
    withProgress(message = 'Loading data', value = 1, {
      
      growth_cent <- st_read("ridata/growth06.shp")
      streams <- st_read("ridata/streams.shp")
      muni <- st_read("ridata/muni97d.shp")
      lulc <- st_read("ridata/rilc11d.shp")
      lulc$lu<-as.character(lulc$Descr_2011)
      growth_cent <- st_transform(growth_cent, 4326)
      muni <- st_transform(muni, 4326)
      streams <- st_transform(streams, 4326)
      lulc <- st_transform(lulc,4326)
      lu <- st_read("ridata/Land_Use_2025.shp")
      road<- st_read('ridata/RIDOT_Roads_2016.shp')
      busroutes <- st_read('ridata/RIPTA_Bus_Routes.shp')
      pond <- st_read('ridata/Rhode_Island_Ponds_and_Lakes.shp')
      census <- st_read('ridata/US_Census_2010_Summary_File_1_Indicators.shp')
    })
    ################################# 
    # Observes for map geometry     #
    #################################
    
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
  
