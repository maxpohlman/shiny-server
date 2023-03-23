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
         menuItem("Forest Carbon Pricing Calculator", tabName = 'fcmap_tab', icon = icon('wrench')),
         menuItem("WTA Tool", tabName = 'wta_tab', icon = icon('money'))
        ), width = 250
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
                            h2('Forest Carbon Pricing Calculator', align = 'center'),
                            p("The value of carbon markets has steadily increased in recent years and are expected to triple in next decade. However, the value of selling forest carbon compared to other forestland uses is not well understood by many forest owners. The FCPC is a simple calculator that can help family forest owners determine if, or when, carbon markets may be a good fit for your land management objectives. This calculator will help forest owners answer two important questions: "),
                            tags$ul(
                              tags$li("What is the potential value of my forest for carbon storage services?"),
                              tags$li("How much should I be paid for storing carbon under different management contracts?")
                            ),
                            p(strong("Data Sources: "),"The value of forest carbon depends on both forest composition and the price of carbon. To understand the carbon storage potential of private forests in different states we used data from the USDA Forest Inventory Analysis database. The average rate of carbon sequestration is an indicator of how much additional carbon could be stored per acre/year under certain management conditions. To understand the value of the additional carbon stored, the user assigns a carbon price ($ dollars per metric ton) using either current or future prices. The fees that could be collected by carbon project developers, who broker contracts with landowners, have already been factored into the estimated values. To help determine a fair level of payment, the calculator describes acceptable payment levels using data from multiple surveys assessing forest owner preferences for different carbon market contracts. Early adopters are expected to accept a lower level of payment compared to forest owners overall, due to differences in timber production objectives. When the value of carbon per acre approaches the recommended payment level for your preferred contract, it may be time to start talking to carbon market programs in your area."),
                            p("Reported values are intended to promote interest in forest carbon markets and should not be used to make individual decisions about your land. Expected level of payment and contract design can only be determined by talking to a forest carbon market program. ", tags$a(href="http://www.google.com", "Click here "),"to learn more about which carbon market programs may be in your area."),
                            p(strong("Instructions:")),
                            tags$ol(tags$li("Use the drop-down menu to describe the location of your forest (i.e., state) and the number of acres you own."),
                            tags$li("Select which attributes you prefer in a forest carbon contract. Options include change in harvesting activities, adopting a management plan, and number of contract years."),
                            tags$li("Select from a list of potential carbon prices to explore how changes in the carbon market may affect the value of carbon sequestration services on your land. Current mean prices range from $5 to $20 per metric ton. To understand more about carbon prices, ", tags$a(href="http://www.google.com", "click here."))
                            )
                            ),
                        box(width = 4,
                            selectInput('fcmap_state', 'State', state.abb[state.abb != 'HI'], 'Pennsylvania'),
                            radioButtons('fcmap_acresowned', 'Forest Acres Owned', c('<20 acres','20-99 acres', '100-249 acres', '250-1000 acres', '>1000 acres' ), '100-249 acres', inline = T),
                            radioButtons('fcmap_harvestingpractice', 'Would you agree to changing your harvesting practices?', c('Yes', 'No'), 'Yes', inline = T),
                            radioButtons('fcmap_managementplan', 'Would you agree to developing a management plan?', c('Yes', 'No'), 'Yes', inline = T),
                            radioButtons('fcmap_contractlength', 'What is your preferred number of contract years?', c('No more than 1 year', 'Up to 20 years', 'Up to 50 years', 'Up to 100 years'), 'Up to 20 years'),
                            #selectInput('fcmap_carbonprice', 'Please select a market price for carbon ($/metric ton)', c('$5','$10','$20','$30','$40','$50','$60','$70','$80','$90','$100','$110','$120','$130','$140','$150'), '$80'),
                            bsTooltip("fcmap_managementplan", "A management plan is defined as a plan that one manages.", placement = 'left'),
                            bsTooltip("fcmap_acresowned", "Round to the nearest acre.", placement = 'left'),
                            fluidRow(column(width = 12, align = 'center', actionButton('runtool', 'Run Calculator')))
                            
                            )
                    ),
                    fluidRow(
                        box(width = 12, title = 'Tool Results',
                               uiOutput('fcmap_table')
                            
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
    allowners <- read_csv('allowners.csv')
    earlyadopters <- read_csv('earlyadopters.csv')
    marketprices <- read_csv('marketprices.csv')
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
      
      mlow <- 999
        
        # marketprices %>% 
        # filter(state == input$fcmap_state) %>% 
        # pull(paste0('m',str_extract(input$fcmap_carbonprice,'\\d+'), 'low'))
      
      mhigh <- 999
        
        # marketprices %>% 
        # filter(state == input$fcmap_state) %>% 
        # pull(paste0('m',str_extract(input$fcmap_carbonprice,'\\d+'), 'high'))
      
      ao<- allowners %>%
        filter(`ownership  size` == input$fcmap_acresowned) %>%
        pull(input$fcmap_contractlength)
      
      ea<- earlyadopters %>%
        filter(`ownership  size` == input$fcmap_acresowned) %>%
        pull(input$fcmap_contractlength)
      
      ao_low <- as.numeric(str_extract(ao, '\\d+')) * .75 
      ea_low <- as.numeric(str_extract(ea, '\\d+')) * .75 
      
      ao_high <- as.numeric(str_extract(ao, '\\d+')) * 1.25
      ea_high <- as.numeric(str_extract(ea, '\\d+')) * 1.25 
      
        tibble(
               state = input$fcmap_state,
               acres = input$fcmap_acresowned,
               harvestingpractice = input$fcmap_harvestingpractice,
               managementplan = input$fcmap_managementplan,
               contractlength = input$fcmap_contractlength,
               mlow = mlow,
               mhigh = mhigh,
               allown = ao,
               earlyadopt = ea,
               ao_low = ao_low,
               ao_high = ao_high,
               ea_low = ea_low,
               ea_high = ea_high
               
        )
               
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
        tags$li(strong('State: '),fcmap_result_table()$state),
        tags$li(strong('Acres owned: '), fcmap_result_table()$acres),
        tags$li(strong('Contract Design: ')),
          tags$ul(
              tags$li(strong('Agree to change harvesting practice: '), fcmap_result_table()$harvestingpractice),
              tags$li(strong('Agree to develop a management plan: '), fcmap_result_table()$managementplan),
              tags$li(strong('Preferred number of contract years: '), fcmap_result_table()$contractlength),
          ),
        
        br(),
        #tags$li(strong('Estimated market value of forest carbon sequestration services in your state: '), fcmap_result_table()$mhigh, '-', fcmap_result_table()$mlow, ' (per acre per year)'),
        tags$li(strong('Mean estimated payment acceptable to all forest owners: '), fcmap_result_table()$allown, ' (CI:', fcmap_result_table()$ao_low, '-', fcmap_result_table()$ao_high,')', ' (per acre per year)'),
        tags$li(strong('Mean estimated payment acceptable to early adopter forest owners: '), fcmap_result_table()$earlyadopt, ' (CI:', fcmap_result_table()$ea_low, '-', fcmap_result_table()$ea_high,')', ' (per acre per year)')

      )  
    })
}

shinyApp(ui, server)