library(shinythemes)
library(shiny)
library(sf)
library(ggplot2)
library(dplyr)

shinyApp(
  ui =navbarPage( "Max's Map Project", theme = shinytheme("cerulean"),
                  tabPanel("Where things are",
                           sidebarLayout(
                             sidebarPanel(
                               p("This is a personal mapping project I am doing to teach myself GIS.", strong("Some features take some time to update on the map, especially if you do the whole state!"),
                                 "Things are added in the order you select them. If something disappears, uncheck and re-check the boxes"),
                               selectInput("scale", label = "What do you want to plot", choices = c('Entire State',
                                                                                     'County',
                                                                                     'Municipality'),
                                           selected = 'Entire State'),
                               selectInput("smallscale", label = "Specific County or Municipality", choices = 'Entire State',
                                           selected = 'Entire State'),
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
                           tabPanel(HTML("Demographic Maps (NYI)</a></li><li><a href=\"http://maxpohlman.com\">Back to my website</a></li><li><a href=\"https://github.com/maxpohlman/shiny-server/blob/master/mydata/app.r\">View source code"),
                                    
                                    mainPanel(
                                      tableOutput('tabo')
                                      
                                    )
                                    
                                    
                                    
                           )
                             
                           ),
                             
   
  
  server = function(input,output,session){
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
    
    ################################# 
    # Observes for map geometry     #
    #################################
    
    observe({
      if (input$scale == 'Entire State'){
        updateSelectInput(session, "smallscale",
                          choices = 'Entire State')
      }
      if (input$scale == 'County'){
        updateSelectInput(session, "smallscale",
                          choices = sort(unique(muni$COUNTY)))
      }
      if (input$scale == 'Municipality'){
        updateSelectInput(session, "smallscale",
                          choices = sort(unique(muni$NAME)))

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
  
