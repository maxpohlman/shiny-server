options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options("googleAuthR.webapp.client_id" = Sys.getenv('CLIENT_ID'))
options("googleAuthR.webapp.client_secret" = Sys.getenv('CLIENT_SECRET'))

library(shiny)
library(shinyjs)
library(googleAuthR)
library(googleID)
library(tidyverse)
# Define UI for application that draws a histogram
ui <- fluidPage( useShinyjs(),
   actionButton('debug','debug'),
   googleAuthUI("gauth_login"),
   textOutput("display_username"),
   uiOutput('t')

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$debug,{browser()}) 
  ## Global variables needed throughout the app
  rv <- reactiveValues(
    login = FALSE
  )
  
  logstatus <- reactiveVal('notlogged')
  
  ## Authentication
  accessToken <- callModule(googleAuth, "gauth_login",
                            login_class = "btn btn-primary",
                            logout_class = "btn btn-primary")
  userDetails <- reactive({
    validate(
      need(accessToken(), "not logged in")
    )
    rv$login <- TRUE
    with_shiny(get_user_info, shiny_access_token = accessToken())
  })
  
  useremail <- reactive({
    validate(
      need(userDetails(), "getting user details")
    )
    
    userDetails()$email$value
  })
  
  observe({
    req(useremail())
    if(str_detect(Sys.getenv('APPROVED_USERS'),paste0('B',useremail(),'A'))){
      logstatus('success')
    } else{
      logstatus('fail')
    }
      
  })
  
  output$display_username <- renderText({
    validate(
      need(userDetails(), "getting user details"),
      need(useremail(), 'need email')
      )
    paste0('Logged in as ',userDetails()$displayName, ', ', useremail())
  })
  
  output$t <- renderUI({
    if(logstatus() == 'notlogged'){
      h4('Please log in via google via the button in the top right. To request access, email maxpohlman@psu.edu')
      
    } else if(logstatus() == 'fail'){
      h4('You are not an authorized user. To request access, email maxpohlman@psu.edu')
    } else if(logstatus() == 'success'){
      includeHTML('testfile.html')
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

