library(sf)
library(shiny)
library(spData)
library(leaflet)
library(tidyverse)
library(shinyjs)
library(tmaptools)
library(shinythemes)


# Define UI for application that draws a histogram
ui <- navbarPage( "Bird Migration Map", theme = shinytheme("cerulean"),
                  tabPanel(HTML("Map</a></li><li><a href=\"http://maxpohlman.com\">Back to my Website</a></li><li><a href=\"https://github.com/maxpohlman/shiny-server/blob/master/Mapproj/app.r\">View Source Code"),        
  fluidPage( shinyjs::useShinyjs(),
   
   # Application title
   titlePanel("Do birds actually migrate?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        uiOutput('birdname'),
        checkboxInput('split', 'Split country in half for averages (NFI)', value = T),
        checkboxInput('month', 'Display Monthly Data'),
        uiOutput('birdmonth'),
        checkboxInput('average', 'Display Avg Monthly Location'),
        actionButton('but', 'Draw Migration Path')
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("l"),
        fluidRow(
          column(12, style = 'font-size:18px;', align = "center",textOutput('words'))
        
        )
         
      )
   )
)

))
# Define server logic required to draw a histogram
server <- function(input, output, session) {

  source('src/function.R')
  shinyjs::disable('split')
  withProgress(message = 'Loading data', min = .99, value = 1,{
    df<-readRDS("src/all_months.rds") %>% dplyr::select(common_name, month, image_url, lat, lng)
    
  })
  seasonal_pal <- c('#0765ff','#003ea3','#00ff0c','#03c413', '#008c06', '#ff8484',
    '#ff0000',
    '#af0000',
    '#ffcb00',
    '#a58b24',
    '#595828',
    '#72a7ff')
  pal<-colorFactor(seasonal_pal, df$month)
  #get_brewer_pal('Spectral', n = 12)
  
  
  output$birdname <- renderUI({
    selectInput("b_name", "Choose a bird", choices = sort(unique(df$common_name)) , selected = 'Acorn Woodpecker')
  })
  
  output$birdmonth <- renderUI({
    disabled(selectInput("b_month", "Choose a Month", choices = sort(unique(df$month)), selected = 'January'))
  })


avg_frame <- eventReactive(input$but,{

  linestring_func_left <- function(origin, target){
    first <- left_l %>%
      filter(month == origin)

    second <- left_l %>%
      filter(month == target)

    frame<-rbind(first, second)
    frame %>%
      st_coordinates() %>%
      st_linestring() %>%
      return()
  }
  
  linestring_func_right <- function(origin, target){
    first <- right_l %>%
      filter(month == origin)
    
    second <- right_l %>%
      filter(month == target)
    
    frame<-rbind(first, second)
    frame %>%
      st_coordinates() %>%
      st_linestring() %>%
      return()
  }


  bd<-filter(df, common_name == input$b_name)
  
  left_l <- filter(bd, lng < -98.583333)
  right_l <- filter(bd, lng >= -98.583333)

  if(nrow(left_l) > 0) {
    
    be<-left_l %>% `st_geometry<-`(NULL) %>% group_by(month) %>% summarize(lat = mean(lat), lng = mean(lng))
    left_l<-st_as_sf(be, coords = c('lng', 'lat'), crs = 4326)
  
  tar_frame <- to_lines(left_l)

  locs<-tar_frame %>% mutate(ls = map2(origin,target,linestring_func_left))

  lines<- st_sf(origin = locs$origin,
                target=locs$target,
                geometry = st_sfc(locs$ls, crs = 4326),
                dim = 'XY',
                row.names = NULL)

  left_lines <- pmap(list(lines$geometry, lines$origin, lines$target, 10), line_subsetter) %>% invoke(rbind,.)
  
  } else{
    left_lines <- NULL
  }
  
  
  if(nrow(right_l) > 0) {
    
    be<-right_l %>% `st_geometry<-`(NULL) %>% group_by(month) %>% summarize(lat = mean(lat), lng = mean(lng))
    right_l<-st_as_sf(be, coords = c('lng', 'lat'), crs = 4326)
    
    tar_frame <- to_lines(right_l)
    
    locs<-tar_frame %>% mutate(ls = map2(origin,target,linestring_func_right))
    
    lines<- st_sf(origin = locs$origin,
                  target=locs$target,
                  geometry = st_sfc(locs$ls, crs = 4326),
                  dim = 'XY',
                  row.names = NULL)
    
    right_lines <- pmap(list(lines$geometry, lines$origin, lines$target, 10), line_subsetter) %>% invoke(rbind,.)
    
  } else{
    right_lines <- NULL
  }
  
  list(left_lines = left_lines, right_lines = right_lines, left_count = nrow(left_lines), right_count = nrow(right_lines))

})
  

  
observeEvent(input$but, {
  
  shinyjs::disable('but')
  shinyjs::disable('b_name')
  
  dummy <- reactiveValues(my_dummy = 0)
  
  leafletProxy("l") %>% clearGroup('avgline_l') %>% clearGroup('avgline_r')
    observe({
      isolate({
        dummy$my_dummy <- dummy$my_dummy +1
      })
      
      if (is.null(avg_frame()$left_count) == F){
        val <- avg_frame()$left_count
      } else{
        val <- avg_frame()$right_count
      }
      
      if (isolate(dummy$my_dummy) <= isolate(val)){
        if (is.null(avg_frame()$left_count) == F){
          leafletProxy("l")  %>% addPolylines(data = avg_frame()$left_lines[dummy$my_dummy,], group = 'avgline_l', color = 'black')
        }
        if (is.null(avg_frame()$right_count) == F){
          leafletProxy("l")  %>% addPolylines(data = avg_frame()$right_lines[dummy$my_dummy,], group = 'avgline_l', color = 'black')
        }
        
        invalidateLater(100, session)
        
      }
      if(isolate(dummy$my_dummy) == isolate(val)){
        shinyjs::enable('but')
        shinyjs::enable('b_name')
      }
    })
})

  
  
  
  
  output$l <- renderLeaflet({
    leaflet() %>% setView(lng = -95.0986295, lat = 38.3126338, zoom = 4) %>% addProviderTiles(providers$OpenStreetMap.Mapnik)
  })
  
  observeEvent(input$b_name,{
    leafletProxy('l') %>% clearGroup('avgline_l') %>% clearGroup('avgline_r')
    
    if(input$average){
      
      leafletProxy('l') %>% clearGroup('avg_l') %>% clearGroup('avg_r') %>%clearGroup('midline')

      avg_icon <- awesomeIcons(
        icon = 'ion-logo-twitter',
        iconColor = 'black',
        library = 'ion',
        markerColor = 'red'
      )
      

      
      
      all<-filter(df, common_name == input$b_name) 
      
      left <- filter(all, lng < -98.583333)
      right <- filter(all, lng >= -98.583333)
      
      if(nrow(left)>0){
        
        e<-left %>% `st_geometry<-`(NULL) %>% group_by(month) %>% summarize(lat = mean(lat), lng = mean(lng))
        left<-st_as_sf(e, coords = c('lng', 'lat'), crs = 4326)
        
        leafletProxy("l") %>% 
          addCircleMarkers(data = left, label = ~month, group = 'avg_l', color = ~pal(left$month), stroke = NULL, fillOpacity  = 1)
        
      }
      
      if(nrow(right) > 0) {
        
        e<-right %>% `st_geometry<-`(NULL) %>% group_by(month) %>% summarize(lat = mean(lat), lng = mean(lng))
        right<-st_as_sf(e, coords = c('lng', 'lat'), crs = 4326)
        
        leafletProxy('l')%>% 
          addCircleMarkers(data = right, label = ~month, group = 'avg_r', color = ~pal(right$month), stroke = NULL, fillOpacity  = 1)
        
        if(nrow(left) > 0){
          mid <- make_middle_line()
          leafletProxy("l") %>% 
            addPolylines(data = mid, color = 'black', group = 'midline')
        }
        
      }
      
    }
    
    if(input$month){
      monthly<-filter(df, common_name == input$b_name, month == input$b_month) 
      mon_icon <- makeIcon('src/bird_marker_small.png', iconAnchorX = 18, iconAnchorY = 53, popupAnchorY = 0)
      
      leafletProxy('l') %>% clearGroup(group = 'monthly') %>%
        addMarkers(data = monthly, popup = paste0("<img src = ",monthly$image_url, ' width="200" height="200">'), icon = mon_icon, group = 'monthly')
    }
    
  })
  
  
  observeEvent(input$b_month,{

    if(input$month){
    monthly<-filter(df, common_name == input$b_name, month == input$b_month) 
    mon_icon <- makeIcon('src/bird_marker_small.png', iconAnchorX = 18, iconAnchorY = 53, popupAnchorY = 0)
    
    leafletProxy('l') %>% clearGroup(group = 'monthly') %>%
      addMarkers(data = monthly, popup = paste0("<img src = ",monthly$image_url, ' width="200" height="200">'), icon = mon_icon, group = 'monthly')
    }
  })

  #x 18 y 53
  
  
  observeEvent(input$month,{
    
    if(input$month){
      shinyjs::enable('b_month')
      
      req(input$b_name)
      monthly<-filter(df, common_name == input$b_name, month == input$b_month) 
      
      mon_icon <- makeIcon('src/bird_marker_small.png', iconAnchorX = 18, iconAnchorY = 53, popupAnchorY = 0)
      
      
      leafletProxy("l") %>% 
        addMarkers(data = monthly, popup = paste0("<img src = ",monthly$image_url, ' width="200" height="200">'), icon = mon_icon, group = 'monthly')
     
      output$words <- renderText({
        'Click on a marker to see the picture taken!'
      }) 
    }
    
    if(!input$month){
      shinyjs::disable('b_month')
      leafletProxy('l') %>%
        clearGroup(group = 'monthly')
      output$words <- renderText({
        ''
      }) 
    }
    
  })
  
  
  observeEvent(input$average,{
    
    if(input$average){
      
      
      avg_icon <- awesomeIcons(
        icon = 'ion-logo-twitter',
        iconColor = 'black',
        library = 'ion',
        markerColor = 'red'
      )
      
      
      all<-filter(df, common_name == input$b_name) 
      
      left <- filter(all, lng < -98.583333)
      right <- filter(all, lng >= -98.583333)
      
      if(nrow(left)>0){
        
        e<-left %>% `st_geometry<-`(NULL) %>% group_by(month) %>% summarize(lat = mean(lat), lng = mean(lng))
        left<-st_as_sf(e, coords = c('lng', 'lat'), crs = 4326)
        
        leafletProxy("l") %>% 
          addCircleMarkers(data = left, label = ~month, group = 'avg_l', color = ~pal(left$month), stroke = NULL, fillOpacity  = 1)
        
      }
      
      if(nrow(right) > 0) {
        
        e<-right %>% `st_geometry<-`(NULL) %>% group_by(month) %>% summarize(lat = mean(lat), lng = mean(lng))
        right<-st_as_sf(e, coords = c('lng', 'lat'), crs = 4326)
        
        leafletProxy('l')%>% 
          addCircleMarkers(data = right, label = ~month, group = 'avg_r', color = ~pal(right$month), stroke = NULL, fillOpacity  = 1)
 
        if(nrow(left) > 0){
          mid <- make_middle_line()
          leafletProxy("l") %>% 
            addPolylines(data = mid, color = 'black', group = 'midline')
        }       
      }
      

       

      
    }
    
    if(!input$average){
      leafletProxy('l') %>%
        clearGroup(group = 'avg_l') %>%
        clearGroup(group = 'avg_r') %>%
        clearGroup(group = 'midline')
      
    }
    
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)



