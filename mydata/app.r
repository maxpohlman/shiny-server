
library(shiny)
library(dplyr)
library(ggplot2)
shinyApp(
ui =fluidPage(
  titlePanel("Max's Data"),
  
  sidebarLayout(
    sidebarPanel(
      p("This is a work in progress using made-up data. Check back in the coming months as I will be adding
        more graphing options, statistical results, and impliment the completed dataset", style =  "font-si20pt"),
      
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
                         choices = c('Biology' = 'BIO','Resource Economics' = 'ENRE', 'Marine Affairs' = 'MAF', 'Other'),
                         selected = c('BIO','ENRE', 'MAF', 'Other')),
                         
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
    mainPanel(
      plotOutput('plot')
 
    )
  
)),

#Takes two of the users selection, xvar and fvar, and plots as grouped bar graph where the x value is xvar and the group/fill is fvar
server = function(input,output){
  data<-read.csv('mydata.csv', header=TRUE)

output$plot <- renderPlot({
  adata<-data[data[[input$xvar]] %in% input[[input$xvar]] & data[[input$fvar]] %in% input[[input$fvar]] & data$Gender %in% input$Gender & data$Major %in% input$Major,] #subsets data based on the x var and fill var - this works
 
  plottable <-data.frame(x=adata[[input$xvar]], y=adata$efficiency, f=adata[[input$fvar]])

   p<-plottable %>%
   group_by(x, f) %>% #groups by the two vars, but needs to group by (Treatment, Round) and not ('Treatment', 'Round')
   summarize(tm = mean(y)) %>% 
    ggplot(aes(x = x, y =tm)) +
    geom_bar(aes(fill = as.factor(f)), position = "dodge", stat="identity") +
     geom_text(aes(x,label=round(tm*100,2), group=f), position = position_dodge(width = .9), size = 8, vjust = 1.5) +
     labs(y = 'Bargain Efficiency') +
     theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"), legend.text=element_text(size=14), legend.title = element_blank())
 print(p) 
})
}
)
