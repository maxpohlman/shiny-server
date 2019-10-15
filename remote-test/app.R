library(shiny)
library(shinyjs)
library(googleAuthR)
library(tidyverse)
options(googleAuthR.webapp.client_id = "638546038897-iakl2g4au3rm6cb68q6fh4i66pqpbac6.apps.googleusercontent.com")
# Define UI for application that draws a histogram
ui <-fluidPage( useShinyjs(),
   actionButton('debug','debug'),
   googleSignInUI("demo")  , #googleAuthUI("gauth_login"),
   uiOutput('t'),

   with(tags, dl(dt("Name"), dd(textOutput("g_name")),
                 dt("Email"), dd(textOutput("g_email")),
                 dt("Image"), dd(uiOutput("g_image")) ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$debug,{browser()}) 
  
  sign_ins <- shiny::callModule(googleSignIn, "demo")
  output$g_name = renderText({ sign_ins()$name })
  output$g_email = renderText({ sign_ins()$email })
  output$g_image = renderUI({ img(src=sign_ins()$image) })
  ## Global variables needed throughout the app
  # rv <- reactiveValues(
  #   login = FALSE
  # )
  # 
  # approved <- read_file('approvedusers.txt')
  # 
  # logstatus <- reactiveVal('notlogged')
  # 
  # ## Authentication
  # accessToken <- callModule(googleAuth, "gauth_login",
  #                           login_class = "btn btn-primary",
  #                           logout_class = "btn btn-primary")
  # userDetails <- reactive({
  #   validate(
  #     need(accessToken(), "not logged in")
  #   )
  #   rv$login <- TRUE
  #   with_shiny(get_user_info, shiny_access_token = accessToken())
  # })
  # 
  # useremail <- reactive({
  #   validate(
  #     need(userDetails(), "getting user details")
  #   )
  #   
  #   userDetails()$email$value
  # })
  # 
  # observe({
  #   req(useremail())
  #   if(str_detect(approved,paste0('B',useremail(),'A'))){
  #     logstatus('success')
  #   } else{
  #     logstatus('fail')
  #   }
  #     
  # })
  # 
  # output$display_username <- renderText({
  #   validate(
  #     need(userDetails(), "getting user details"),
  #     need(useremail(), 'need email')
  #     )
  #   paste0('Logged in as ',userDetails()$displayName, ', ', useremail())
  # })
  # 
  # output$t <- renderUI({
  #   if(logstatus() == 'notlogged'){
  #     h4('Please log in via google via the button in the top right. To request access, email maxpohlman@psu.edu')
  #     
  #   } else if(logstatus() == 'fail'){
  #     h4('You are not an authorized user. To request access, email maxpohlman@psu.edu')
  #   } else if(logstatus() == 'success'){
  #     includeHTML('testfile.html')
  #   }
  #   
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

