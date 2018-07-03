library(sf)
library(shiny)
library(spData)
library(leaflet)
library(tidyverse)
library(shinyjs)
library(tmaptools)

# Define UI for application that draws a histogram
ui <- fluidPage( shinyjs::useShinyjs(),
   
   # Application title
   titlePanel("MA Hospital Patient Transfer Network"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        p("This visualization uses made-up data to demonstrate a theoretical network of how patients move through hospitals.
          To get started, uncheck the 'Show all hospitals' box below, set parameters as you wish, then click the create button."),
          checkboxInput('showall', 'Show all hospitals', value = T),
          uiOutput('hospselector'),
          numericInput('scannum', 'How many closeby hospitals to scan? (rec: 8-15)', value = 10, min = 5, max = 20),
          numericInput('jumpnum', 'How many waves of branches? (rec: 3-5)', value = 4, min = 2, max = 9),
          numericInput('num_patient', 'How many patients?', value = 50000),
          actionButton('anibutton', 'Create Network'),
        br(),
        p('CURRENT BUGS:'),
        p('- Selecting a new hospital immediately reconstructs the network w/o waiting for the user to click the button again')),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("l")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  source('functions.R')
  
  
  hospitals <- readRDS('hospitals.rds')
  dist_matrix <- readRDS('distance_matrix.rds')
  icon_1 <-makeIcon('./icons/1.png', iconAnchorX = 8, iconAnchorY = 8)
  il <- iconList(
    '2' = makeIcon('./icons/2.png', iconAnchorX = 8, iconAnchorY = 8),
    '3' = makeIcon('./icons/3.png', iconAnchorX = 8, iconAnchorY = 8),
    '4' = makeIcon('./icons/4.png', iconAnchorX = 8, iconAnchorY = 8),
    '5'= makeIcon('./icons/5.png', iconAnchorX = 8, iconAnchorY = 8),
    '6'= makeIcon('./icons/6.png', iconAnchorX = 8, iconAnchorY = 8),
    '7'= makeIcon('./icons/7.png', iconAnchorX = 8, iconAnchorY = 8),
    '8'= makeIcon('./icons/8.png', iconAnchorX = 8, iconAnchorY = 8),
    '9'= makeIcon('./icons/9.png', iconAnchorX = 8, iconAnchorY = 8),
    '10'= makeIcon('./icons/10.png', iconAnchorX = 8, iconAnchorY = 8)
  )
  
  output$hospselector <- renderUI({
    selectInput("hospselector", 'Choose a Hospital', choices =sort(unique(hospitals$names)) )
  })
   
   output$l <- renderLeaflet({
     
     leaflet() %>% 
       setView(lng = -71.5820372, lat = 42.1770196, zoom = 8) %>%
       addProviderTiles(providers$OpenStreetMap.Mapnik)
   })
   
   observeEvent(input$hospselector,{
     leafletProxy('l') %>%
       clearShapes() %>%
       clearMarkers() %>%
       addMarkers(data = hospitals, icon = makeIcon('./icons/hospital_icon_2.gif', iconWidth = 20, iconHeight = 20), label = hospitals$names, options = markerOptions(riseOnHover = TRUE))
   })
   
   observe({
     if (input$showall == T){

       
       shinyjs::disable('anibutton')
       shinyjs::disable('scannum')
       shinyjs::disable('jumpnum')
       shinyjs::disable('num_patient')
       req(input$hospselector)
       shinyjs::disable('hospselector')
       
       
     }
     
     if (input$showall == F){
       leafletProxy('l') %>%
         clearMarkers()
       
       shinyjs::enable('anibutton')
       shinyjs::enable('hospselector')
       shinyjs::enable('scannum')
       shinyjs::enable('jumpnum')
       shinyjs::enable('num_patient')
      or <-filter(hospitals, names == input$hospselector)
       leafletProxy('l')  %>%
         addMarkers(data = or, icon = icon_1, label = or$names)

       
     }
     
     dummy <- reactiveValues(my_dummy = 0)
    
     
     network_data <- eventReactive(input$anibutton,{
       withProgress(message = 'Constructing network', min = .99, value = 1,{
     network<-network_link(hospitals, input$hospselector , input$num_patient, dist_matrix, input$jumpnum, input$scannum)
       })
     })
     
     
     observeEvent(input$anibutton,{
       
       shinyjs::disable('anibutton')
       shinyjs::disable('scannum')
       shinyjs::disable('jumpnum')
       shinyjs::disable('num_patient')
       req(input$hospselector)
       shinyjs::disable('hospselector')
       
       dummy$my_dummy <-0
       
       #pal<-colorBin('OrRd', network_data()$lines[['Patients Transferred']], bins = 5)
       pal<-colorBin(get_brewer_pal('YlOrRd', n = 11)[6:11], network_data()$lines[['Patients Transferred']], bins = 6)
       
       leafletProxy('l') %>% clearMarkers() %>% clearShapes() %>% clearControls() 

       target_labels <- make_target_labels(network_data()$target_points)
       
       leafletProxy('l') %>%
         addMarkers(data = network_data()$origin_point, icon = icon_1, label = network_data()$origin_point$names) %>%
         addMarkers(data = network_data()$target_points, icon = ~il[map_icon], 
                    label = target_labels,
                    labelOptions = labelOptions(
                      noHide = F,
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"
                    ))  %>% 
         addLegend(pal = pal, values = network_data()$lines[['Patients Transferred']],
                   position = 'topright', labFormat = labelFormat(suffix = ' patients'),  
                   title = '# Patients Transferred Between Hospitals', opacity = .7)

      
       observe({
         isolate({
           dummy$my_dummy <- dummy$my_dummy +1
         })

         if (isolate(dummy$my_dummy) <= isolate(nrow(network_data()$lines))){
           leafletProxy('l') %>%
          addPolylines(data = slice(network_data()$lines,dummy$my_dummy), 
                       label = make_line_label(dummy$my_dummy, network_data()$lines),
                       color = ~pal(slice(network_data()$lines,dummy$my_dummy)[['Patients Transferred']]),
                       opacity = .7,
                       labelOptions = labelOptions(
                         noHide = F,
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px",
                         direction = "auto"
                       ))
           
           invalidateLater(100, session)
           if (isolate(dummy$my_dummy) == isolate(nrow(network_data()$lines))){
             
           shinyjs::enable('anibutton')
           shinyjs::enable('hospselector')
           shinyjs::enable('scannum')
           shinyjs::enable('jumpnum')
           shinyjs::enable('num_patient')
           }
           
           
         }


        })

     })
     
     #, icon = ~makeIcon(paste0('./icons/',network$target_points$map_icon,'.png'))
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

