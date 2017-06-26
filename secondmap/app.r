library(shinythemes)
library(shiny)
library(rgdal)
library(ggplot2)
library(dplyr)
library(raster)
library(sp)
options(shiny.sanitize.errors = FALSE)

#streams <- st_read("ridata/streams.shp")
#muni<-readRDS(file = 'ridata/muni.rds')

#streams <- st_transform(streams, 4326)
#lu <- st_read("ridata/Land_Use_2025.shp")
#road<- st_read('ridata/RIDOT_Roads_2016.shp')
#busroutes <- st_read('ridata/RIPTA_Bus_Routes.shp')
#pond <- st_read('ridata/Rhode_Island_Ponds_and_Lakes.shp')
#census <- st_read('ridata/US_Census_2010_Summary_File_1_Indicators.shp')

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
    muni <- readOGR('ridata','muni97d')
    output$plot<-renderPlot({
      p<-plot(muni)
      print(p)
    })
  }
)
  
