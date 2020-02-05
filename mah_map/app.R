#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyauthr)
library(leaflet)
library(tidyverse)

user_base <- data.frame(
  user = read_file('www/username.txt'),
  password = read_file('www/password.txt'),
  stringsAsFactors = FALSE,
  row.names = NULL
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  # Application ti
  # Sidebar with a slider input for number of bins 
  div(class = "pull-right", logoutUI(id = "logout")),
  # add login panel UI function
  loginUI(id = "login"),
  hidden(div(id = 'page',
             sidebarLayout(
               sidebarPanel(
                 p('Map options will go here'),
                 checkboxGroupInput("bins",
                                    "Show specific buffer/bmp/locations/etc",
                                    choices = c('A','B','C'))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 p('Will edit the CSS to make the map larger/less greyspace'),
                 leafletOutput('m')
               )
             )
  )
  ))
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            log_out = reactive(logout_init()))
  
  # pulls out the user information returned from login module
  user_data <- reactive({credentials()$info})
  
  observe({
    if(credentials()$user_auth == T){
      shinyjs::show('page')
    }
    if(credentials()$user_auth == F){
      shinyjs::hide('page')
    }
  })
  
  output$m <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

