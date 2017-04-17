
library(shiny)
library(dplyr)
shinyApp(
ui =fluidPage(
  titlePanel("Max's Data"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Help Text"),
      
      checkboxGroupInput("Treatment", 
                  label = "Select treatments to display",
                  choices = c("Certainty", "Uncertainty",
                              "Ambiguity"),
                  selected = "Certainty"),
      
      checkboxGroupInput("Round", 
                         label = "Select rounds to display",
                         choices = c('Round One', 'Round Two',
                                      'Round Three'),
                         selected = "Round One"),
      checkboxGroupInput("Gender", 
                         label = "Select gender of participants",
                         choices = c('Male','Female'),
                         selected = "Male"),
      selectInput("xvar", label = "Xvar", choices = c('Gender',
                                                      'Round',
                                                      'Treatment'),
                  selected = 'Treatment'),
      selectInput("fvar", label = "Fvar", choices = c('Gender',
                                                      'Round',
                                                      'Treatment'),
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
  adata<-data[data[[input$xvar]] %in% input[[input$xvar]] & data[[input$fvar]] %in% input[[input$fvar]] & data$Gender %in% input$Gender,] #subsets data based on the x var and fill var - this works
 
  plottable <-data.frame(x=adata[[input$xvar]], y=adata$efficiency, f=adata[[input$fvar]])

   p<-plottable %>%
   group_by(x, f) %>% #groups by the two vars, but needs to group by (Treatment, Round) and not ('Treatment', 'Round')
   summarize(tm = mean(y)) %>% 
    ggplot(aes(x = x, y =tm)) +
    geom_bar(aes(fill = as.factor(f)), position = "dodge", stat="identity")
 print(p)
})
}
)