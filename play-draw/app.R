#Shiny app that does proportion chi-square tests to look at play vs draw win rates
library(shiny)
  shinyApp(
    ui = basicPage(
      numericInput("win_play", "Enter number of wins on the play", 0),
      numericInput("game_play", "Enter number of games on the play", 0),
      numericInput("win_draw", "Enter number of wins on the draw", 0),
      numericInput("game_draw", "Enter number of games on the draw", 0),
      actionButton('button', 'Calculate P-Val'),
      textOutput('returned_pval')
    ),
    server = function(input, output) {
      
      returned_text<-eventReactive(input$button,{
        if(input$win_play > input$game_play | input$win_draw > input$game_draw){
          "Make sure you don't have more wins than games played"
        }
        else{
          
          pval <-prop.test(c(input$win_play, input$win_draw), c(input$game_play, input$game_draw))$p.value
          paste0('Based on your proportions, the p-value for a 2 sample test for equality of proportions is: ',
                 round(pval,8),
                 '. If this number is <.05, you can reject a null hypothesis that there is no difference in win rate
             on the play or on the draw with 95% confidence.')
             }
      })
      
      output$returned_pval <- renderText({ 
        returned_text()
      })
      
    }
  )
