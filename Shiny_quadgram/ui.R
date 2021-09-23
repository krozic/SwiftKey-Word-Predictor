library(shiny)

shinyUI(fluidPage(
    titlePanel("Word Predictor"),
    sidebarLayout(
        sidebarPanel(
            textInput("Query",
                      "Predictor Words",
                      value = ""),
            submitButton("Predict Words")
        ),
        mainPanel(
            h5('*App is loaded when empty result appears.'),
            textOutput('predictedWord'),
            dataTableOutput("predictedWords")
        )
    )
))
