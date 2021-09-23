library(shiny)
source('predict_word.R')
source('predict_words.R')

shinyServer(function(input, output) {
    predictedWord <- reactive({predict_word(input$Query)})
    predictedWords <- reactive({predict_words(input$Query)})
    
    output$predictedWord <- renderText(
        paste('The word with the highest probability is:', predictedWord()$pred)
        )
    
    output$predictedWords <- renderDataTable(predictedWords(),
                                    options = list(
                                        paging = FALSE,
                                        searching = FALSE
                                    )
    )
})
