library(shinydashboard)
library(tidyverse)
library(shinyBS)
library(DT)

counties<-c("Adams",    "Allegheny","Armstrong","Beaver",   "Bedford",  "Berks",    "Blair",    "Bradford", "Bucks",   
"Butler",   "Cambria",  "Cameron",  "Carbon",   "Centre",   "Chester",  "Clarion",  "Clearfield"  ,   "Clinton", 
"Columbia", "Crawford", "Cumberland"  ,   "Dauphin",  "Delaware", "Elk","Erie",     "Fayette",  "Forest",  
"Franklin", "Fulton",   "Greene",   "Huntingdon"  ,   "Indiana",  "Jefferson","Juniata",  "Lackawanna"   ,  "Lancaster",   
"Lawrence", "Lebanon",  "Lehigh",   "Luzerne",  "Lycoming", "McKean",   "Mercer",   "Mifflin",  "Monroe",  
"Montgomery" ,   "Montour",  "Northampton" ,   "Northumberland", "Perry",    "Philadelphia" ,  "Pike",     "Potter",   "Schuylkill",  
"Snyder",   "Somerset", "Sullivan", "Susquehanna" ,   "Tioga",    "Union",    "Venango",  "Warren",   "Washington",   
"Wayne",    "Westmoreland" ,  "Wyoming",  "York")

ui <- dashboardPage(
    dashboardHeader(title = "Forest Benefits and Values", titleWidth = 300),
    dashboardSidebar(
        sidebarMenu(
         menuItem("Information",tabName = 'information_tab',icon = icon('info')),
         menuItem("Links to Web Pages",tabName = 'links_tab', icon = icon('link')),
         menuItem('Water Quality Values',tabName = 'waterquality_tab', icon = icon('water')),
         menuItem('Forest Preservation Values',tabName ='forest_tab', icon = icon('tree')),
         menuItem("FCMAP Tool", tabName = 'fcmap_tab', icon = icon('wrench')),
         menuItem("WTA Tool", tabName = 'wta_tab', icon = icon('money'))
        )
    ),
    dashboardBody(
        tabItems(
            
            tabItem(tabName = 'information_tab',
                    h2('Project Information'),
                    p("This is where you would have paragraphs welcoming the user to the application. What it is designed to do, what
                      the various tabs are, the project leads, and all the other fun stuff. You can put pictures, links, and any other kinds of media you
                      want on this initial page."),
                    br(),
                    fluidRow(
                      column(width = 12,align = 'center',
                      img(src='forest1.jpg'))
                      
                      
                    )
                    ),
            
            tabItem(tabName = 'links_tab',
                    h2('Links to Web Pages'),
                    h4("Below are some links to external websites. I forgot what these will be from the meeting and the flowchart didn't have labels, so here's a mockup
                    with some random search engines"),
                    br(),
                    tags$ul(
                        tags$li(tags$a(href="http://www.google.com", "Google"), 'is perhaps the most common search engine.'),
                        tags$li(tags$a(href="http://www.bing.com", "Bing"), "is Microsoft's search engine."),
                        tags$li(tags$a(href="http://www.duckduckgo.com", "Duck Duck Go"), 'claims to protect your privacy more than mainstream search engines.'),
                        tags$li(tags$a(href="http://www.ask.com", "Ask Jeeves"), "is a search engine that's been around forever but never took off.")
                    )
                    ),
            tabItem(tabName = 'waterquality_tab',
                    h2('PA County Values for Protecting Water Quality with Forests'),
                    h4("Intro paragraph here to explain what's going on in this tab"),
                    fluidRow(column(12, align = 'center',
                                    selectInput('waterquality_county_input', 'Select Your County', choices = counties, selected = 'Adams'))),
                    
                    fluidRow(column(12, align = 'center',
                                    tableOutput('waterquality_county_table')
                    ))
                
                    
            
            
            ),
        
            tabItem(tabName = 'forest_tab',
                    h2('PA County Values for Keeping Forests as Forests (Timber, Water, and Wildlife)'),
                    h4("Intro paragraph here to explain what's going on in this tab"),
                    fluidRow(column(12, align = 'center',
                                    selectInput('forest_county_input', 'Select Your County', choices = counties, selected = 'Adams'))),
                    
                    fluidRow(column(12, align = 'center',
                                    tableOutput('forest_county_table')
                    ))
                    
                    
                    
                    
            ),
            
            tabItem(tabName = 'fcmap_tab',
                    
                    fluidRow(
                        box(width = 8,
                            h2('Forest Carbon Market Assessment and Planning Tool (FCMAP)', align = 'center'),
                            h4("Some more text describing the tool, maybe some guidance to the user, maybe some links, sky's the limit")
                            ),
                        box(width = 4,
                            selectInput('fcmap_state', 'State', state.name, 'Pennsylvania'),
                            radioButtons('fcmap_multimanagement', 'Do you have multiple management objectives?', c('Yes', 'No'), 'Yes', inline = T),
                            radioButtons('fcmap_managementplan', 'Do you have a management plan?', c('Yes', 'No'), 'Yes', inline = T),
                            radioButtons('fcmap_acresowned', 'Acres Owned Category', c('<40 acres','40-99 acres', '100-250 acres', '>250 acres' ), '100-250 acres', inline = T),
                            selectInput('fcmap_contractlength', 'Contract Length Preference', c('Very Short', 'Short', 'Medium', 'Long', 'Very Long'), 'Medium'),
                            bsTooltip("fcmap_managementplan", "A management plan is defined as a plan that one manages.", placement = 'left'),
                            bsTooltip("fcmap_acresowned", "Round to the nearest acre.", placement = 'left'),
                            fluidRow(column(width = 12, align = 'center', actionButton('runtool', 'Run Tool')))
                            
                            )
                    ),
                    fluidRow(
                        box(width = 12, title = 'Tool Results',
                               uiOutput('fcmap_table'),
                            DTOutput('dttest')
                            
                               )
                    )
                    
                    ),
            tabItem(tabName = 'wta_tab',
                    fluidRow(
                      box(width = 8,
                          h2('WTA Tool', align = 'center'),
                          h4("Some more text describing the tool, maybe some guidance to the user, maybe some links, sky's the limit")
                      ),
                      box(width = 4,
                          selectInput('wta_contract', 'Select Contract Length', c('Annual','Up to 20 Years', 'Up to 50 Years','Up to 100 Years'), 'Annual'),
                          radioButtons('wta_managementplan', 'Does the contract require a management plan?', c('Yes', 'No'), 'Yes', inline = T),
                          radioButtons('wta_sr', 'Does the contract have silvicultural restrictions?', c('Yes', 'No'), 'Yes', inline = T),
                          fluidRow(column(width = 12, align = 'center', actionButton('runwtatool', 'Run Tool')))
                          
                      )
                    ),
                    fluidRow(
                      box(width = 12, title = 'Tool Results',
                          uiOutput('wta_table')

                      )
                    )
            )
                    
    )
))

server <- function(input, output) {
    set.seed(122)
  
    wta_df <- read_csv('wtadata.csv')
    water_quality_df <- tibble(County = counties,
                               `Value 1` = rnorm(length(counties)),
                               `Value 2` = rnorm(length(counties)),
                               `Value 3` = rnorm(length(counties)),
                               `Value 4` = rnorm(length(counties)))
    
    forest_df <- tibble(
        County = rep(counties,3),
        Type = c(rep('Timber', length(counties)),rep('Water', length(counties)),rep('Wildlife', length(counties))),
        `Value 1` = rnorm(length(counties)*3),
        `Value 2` = rnorm(length(counties)*3),
        `Value 3` = rnorm(length(counties)*3),
        `Value 4` = rnorm(length(counties)*3)
    )
    
    fcmap_result_table <- eventReactive(input$runtool, {
        tibble(c1 = rnorm(1),
               c2 = rnorm(1),
               c3 = rnorm(1),
               c4 = rnorm(1),
               c5 = rnorm(1),
               c6 = rnorm(1),
               c7 = rnorm(1),
               state = input$fcmap_state,
               maxcolor = colors()[sample(1:length(colors()), 1)],
               maxyear = paste0(sample(1:length(colors()), 1), ' years'),
               maxpayment = paste0('$',sample(10:100, 1),'/year'),
               melissapayment = paste0('$',sample(10:100, 1),'/year'),
               melissayear = paste0(sample(1:length(colors()), 1), ' years'),
               melissacolor = colors()[sample(1:length(colors()), 1)])
    })
    
    wta_result_table <- eventReactive(input$runwtatool,{
      wta_df %>%
        filter(has_sr == input$wta_sr) %>%
        filter(has_mp == input$wta_managementplan) %>%
        filter(contract_length == input$wta_contract)
      
    })
    
    
    output$waterquality_county_table <- renderTable({
        wq_data <- water_quality_df %>%
            filter(County == input$waterquality_county_input)
    })
    
    output$forest_county_table <- renderTable({
        forest_data <- forest_df %>%
            filter(County == input$waterquality_county_input) %>%
            select(-County)
    })
    
    output$wta_table <- renderTable({
      wta_result_table()
    })
    
    output$dttest <- renderDT({
      tibble(
        Program = c('Max', 'Melissa'),
        `Average Payment` = c(fcmap_result_table()$maxpayment, fcmap_result_table()$melissapayment),
        `Contract Length` = c(fcmap_result_table()$maxyear, fcmap_result_table()$melissayear),
        `Favorite Color` =  c(fcmap_result_table()$maxcolor, fcmap_result_table()$melissacolor),
        `From Boston` = c('Y', 'N'),
        `Likes Checkmarks` = c('','✓'),
        `Colored` = c(' ', '  ')
      ) %>% datatable() %>% formatStyle(
        'Colored',
        backgroundColor = styleEqual(c(' ', '  '), c('#3af736', 'red'))
      )
    })
    
    output$fcmap_table <- renderUI({
      tags$ul(
          tags$li('Mean forest owner WTA/acre:',fcmap_result_table()$c1),
          tags$li(paste0('Mean value of private forest carbon in ', fcmap_result_table()$state ,' (acre): ',fcmap_result_table()$c2)),
          tags$ul(
              tags$li('Social value ($51/ton)'),
              tags$li('California Market Value ($17/ton)'),
              tags$li('Offset Value($3-$5/ton)'),
          ),
          tags$li(paste0('Market programs offered in ',fcmap_result_table()$state,' : ')),
          tags$ul(
              tags$li("Max's Program: "),
              tags$ul(
                  tags$li(paste0('Average Payment: ', fcmap_result_table()$maxpayment)),
                  tags$li(paste0('Contract Length: ', fcmap_result_table()$maxyear)),
                  tags$li(paste0('Other Specifics: ')),
                  tags$ul(
                      tags$li(paste0("Max's favorite color is: ", fcmap_result_table()$maxcolor))
                  )
              ),
              tags$li("Melissa's Program: "),
              tags$ul(
                  tags$li(paste0('Average Payment: ', fcmap_result_table()$melissapayment)),
                  tags$li(paste0('Contract Length: ', fcmap_result_table()$melissayear)),
                  tags$li(paste0('Other Specifics: ')),
                  tags$ul(
                      tags$li(paste0("Melissa's favorite color is: ", fcmap_result_table()$melissacolor))
                  )
              )
          )
      )  
    })
}

shinyApp(ui, server)