library(shiny)

shinyUI(fluidPage(

    # Application title
    titlePanel("Next Word Predictor"),

    sidebarLayout(
        sidebarPanel(
            textInput("inputWords", "Enter words to predict next word", value = ""),
            selectInput("model", "Choose Prediction Model", c("4-gram", "5-gram"), selected = "4-gram")
            #,submitButton("Submit")
        ),

        # Show predicted next word
        mainPanel(
            h3("Next Word Prediction:"),
            dataTableOutput("nextWord")
        )
    )
))