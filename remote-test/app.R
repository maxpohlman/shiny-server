library(shiny)
library(googleAuthR)

options(googleAuthR.webapp.client_id = "638546038897-iakl2g4au3rm6cb68q6fh4i66pqpbac6.apps.googleusercontent.com")

ui <- fluidPage( 
  
  titlePanel("Sample Google Sign-In"),
  
  sidebarLayout(
    sidebarPanel(
      googleSignInUI("demo")
    ),
    
    mainPanel(
      with(tags, dl(dt("Name"), dd(textOutput("g_name")),
                    dt("Email"), dd(textOutput("g_email")) ))
    )
  )
)

server <- function(input, output, session) {
  
  sign_ins <- shiny::callModule(googleSignIn, "demo")
  
  output$g_name = renderText({ sign_ins()$name })
  output$g_email = renderText({ sign_ins()$email })

}

# Run the application 
shinyApp(ui = ui, server = server)