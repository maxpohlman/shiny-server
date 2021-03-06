library(shinythemes)
library(shiny)
library(dplyr)
library(ggplot2)

shinyApp(
  ui =navbarPage( "Max's Data", theme = shinytheme("cerulean"),
                  tabPanel("Analysis",
                           sidebarLayout(
                             sidebarPanel(
                               p("This is a work in progress using monte-carlo simulated data. Check back in the coming months as I will be adding
                                 more graphing options, statistical results, and implement the completed dataset", style =  "font-si20pt"),
                               
                      #Adds Buttons         
                               checkboxGroupInput("Treatment", 
                                                  label = "Select treatments to display",
                                                  choices = c("Certainty", "Uncertainty",
                                                              "Ambiguity"),
                                                  selected = c("Certainty", "Uncertainty",
                                                               "Ambiguity")),
                               
                               checkboxGroupInput("Round", 
                                                  label = "Select rounds to display",
                                                  choices = c('Round One', 'Round Two',
                                                              'Round Three'),
                                                  selected = c('Round One', 'Round Two',
                                                               'Round Three')),
                               checkboxGroupInput("Gender", 
                                                  label = "Select gender of participants",
                                                  choices = c('Male','Female'),
                                                  selected = c('Male','Female')),
                               checkboxGroupInput("Major", 
                                                  label = "Select major of participants",
                                                  choices = c('Biology' = 'BIO','Resource Economics' = 'ENRE', 'Marine Affairs' = 'MAF', 'Natural Resource Science' = 'NRS', 'Other'),
                                                  selected = c('BIO','ENRE', 'MAF', 'NRS', 'Other')),
                               
                               selectInput("xvar", label = "X variable", choices = c('Gender',
                                                                                     'Round',
                                                                                     'Treatment',
                                                                                     'Major'),
                                           selected = 'Treatment'),
                               selectInput("fvar", label = "Fill variable", choices = c('Gender',
                                                                                        'Round',
                                                                                        'Treatment',
                                                                                        'Major'),
                                           selected = 'Round')
                               
                               
                               
                               ),
                      #Adds plot area
                             mainPanel(
                               plotOutput('plot'),
                      #Adds t-test area
                               fluidRow(
                                 column(2,selectInput("t1", label = "Choose Category for T Test", choices = c('Gender',
                                                                                                              'Round',
                                                                                                              'Treatment',
                                                                                                              'Major'),
                                                      selected = 'Treatment')),
                                 column(2,selectInput("t2", label = "Choose First variable for T Test", choices = 'test')),
                                 
                                 column(2,selectInput("t3", label = "Choose Second variable for T Test", choices = 'test'))),
                               tableOutput('text')
                               
                             )
                             
                           )),
                  
                  #Adds second and third tabs, as well as table area in tab 2
                  tabPanel(HTML("Dataset</a></li><li><a href=\"http://maxpohlman.com\">Back to my website</a></li><li><a href=\"https://github.com/maxpohlman/shiny-server/blob/master/mydata/app.r\">View source code"),
                           
                           mainPanel(
                             tableOutput('tabo')
                             
                           )
                           
                           
                           
                  )
                  ),
  
  server = function(input,output,session){
    data<-read.csv('mydata.csv', header=TRUE) #Not used at the moment
    
    ################################# 
    # Observes for t test variables #
    #################################
    
    observe({
      if (input$t1 == 'Treatment'){
        updateSelectInput(session, "t2",
                          choices = c("Certainty", "Uncertainty", "Ambiguity"))
        updateSelectInput(session, "t3",
                          choices = c("Certainty", "Uncertainty", "Ambiguity"))
      }
      if (input$t1 == 'Major'){
        updateSelectInput(session, "t2",
                          choices = c("NRS", "BIO", "MAF",'ENRE','Other'))
        updateSelectInput(session, "t3",
                          choices = c("NRS", "BIO", "MAF",'ENRE','Other'))
      }
      if (input$t1 == 'Gender'){
        updateSelectInput(session, "t2",
                          choices = c("Male", "Female"))
        updateSelectInput(session, "t3",
                          choices = c("Male", "Female"))
      }
      if (input$t1 == 'Round'){
        updateSelectInput(session, "t2",
                          choices = c("Round One", "Round Two", 'Round Three'))
        updateSelectInput(session, "t3",
                          choices = c("Round One", "Round Two", 'Round Three'))
      }
      
      
    })
    
    
    #############################################################################  
    # Generates random efficiency numbers with individual and treatment effects #
    #############################################################################
    
    set.seed(12345)
    t <- data.frame('a' = c('Certainty','Uncertainty','Ambiguity'), 'v' = rnorm(3, mean = .75, sd = .1))
    m<- data.frame('a' = c('ENRE', 'BIO', 'NRS', 'MAF', 'Other'), 'v' = rnorm(5, mean = .75, sd = .1))
    r<- data.frame('a' = c('Round One', 'Round Two', 'Round Three'), 'v' = rnorm(3, mean = .75, sd = .1))
    g<- data.frame('a' = c('Male','Female'), 'v' = rnorm(2, mean = .75, sd = .1))
    
    df <- data.frame('Treatment' = character(1000), 'Round' = character(1000), 'Gender' = character(1000), 
                     'Major' = character(1000), 'efficiency' = integer(1000))
    t %>% mutate_if(is.factor, as.character) -> t
    m %>% mutate_if(is.factor, as.character) -> m
    g %>% mutate_if(is.factor, as.character) -> g
    r %>% mutate_if(is.factor, as.character) -> r
    df %>% mutate_if(is.factor, as.character) -> df
    for (i in 1:1000){
      df$Treatment[i]<-sample(t$a,1)
      df$Major[i]<-sample(m$a,1)
      df$Round[i]<-sample(r$a,1)
      df$Gender[i]<-sample(g$a,1)
      df$efficiency[i]<-t$v[match(df$Treatment[i],t$a)] * m$v[match(df$Major[i],m$a)] * g$v[match(df$Gender[i],g$a)] + 
                        r$v[match(df$Round[i],r$a)] * rnorm(1,mean=.35, sd=.1)
      
    }
    data<-df
    
    #####################################################################################  
    # Plots the data by selecting the columns that are inputted in the two SelectInputs #
    #####################################################################################
    
    output$plot <- renderPlot({
      adata<-data[data$Treatment %in% input$Treatment& data$Round %in% input$Round & data$Gender %in% input$Gender 
                  & data$Major %in% input$Major,] #subsets data based on selected values of Gender/Major/Round/Treatment
      
      #Grabs the three variables to be plotted
      plottable <-data.frame(x=adata[[input$xvar]], y=adata$efficiency, f=adata[[input$fvar]])
      
      
      p<-plottable %>%
        group_by(x, f) %>% #groups by the two vars selected
        summarize(tm = mean(y),see=sd(y)) %>% #calculates mean and sd
        mutate(se = see/sqrt(length(plottable$x))) %>% #converts to standard error
        mutate(ci = se*1.96) %>%               #converts to confidence intervals
        ggplot(aes(x = x, y =tm)) +
        geom_bar(aes(fill = as.factor(f)), position = "dodge", stat="identity") + 
        geom_errorbar(aes(ymin=tm-ci, ymax=tm+ci, group=f),
                      width=.2, position=position_dodge(.9)) +
        geom_text(aes(x, y=tm-ci,label=round(tm*100,2), group=f), position = position_dodge(width = .9), size = 8, vjust = 1.5) +
        labs(y = 'Bargain Efficiency') +
        theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"), legend.text=element_text(size=14), legend.title = element_blank())
      print(p) 
    })
    
    ##################################################################
    # Performs the t-test at 95% confidence for differences of means #
    ##################################################################
    
    output$text<-renderTable({
      t1<-input$t1
      t2<-input$t2
      t3<-input$t3
      if(t2==t3){ #If the two variables are the same, give error message
        op<-data.frame('T-stat'= '', 'Degrees of Freedom' = 'Needs two different variables', 'P-Value' = '')
      }
      else{
        tt<-t.test(subset(df,df[[t1]]==t2)$efficiency,subset(df,df[[t1]]==t3)$efficiency)
        
        op<-data.frame('T-stat'=tt[[1]], 'Degrees of Freedom' = tt[[2]], 'P-Value' = tt[[3]])
      }
    })
    
    output$tabo<-renderTable({
      data
    })
    
  }
  )
#box/violin/density curve