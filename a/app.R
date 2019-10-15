library(shiny)
library(googleAuthR)

options(googleAuthR.webapp.client_id = "788359008311-l5too4nh4obuil05bvhsvc17ib1d0hcl.apps.googleusercontent.com")

ui <- fluidPage(
  
  titlePanel("Sample Google Sign-In"),
  
  sidebarLayout(
    sidebarPanel(
      googleSignInUI("demo")
    ),
    
    mainPanel(
      with(tags, dl(dt("Name"), dd(textOutput("g_name")),
                    dt("Email"), dd(textOutput("g_email")),
                    dt("Image"), dd(uiOutput("g_image")) ))
    )
  )
)

server <- function(input, output, session) {
  
  sign_ins <- shiny::callModule(googleSignIn, "demo")
  
  output$g_name = renderText({ sign_ins()$name })
  output$g_email = renderText({ sign_ins()$email })
  output$g_image = renderUI({ img(src=sign_ins()$image) })
  
}

# Run the application
shinyApp(ui = ui, server = server)