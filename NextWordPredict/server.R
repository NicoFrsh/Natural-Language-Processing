library(shiny)
library(data.table)
source("../predictNextWord4.R")
source("../predictNextWord5.R")

# load n-grams to server once
# for 4-gram model we use 30% of the data
unigrams4 <- readRDS("../data/unigrams03.rds")
bigrams4 <-readRDS("../data/bigrams03.rds")
trigrams4 <- readRDS("../data/trigrams03.rds")
quadgrams4 <- readRDS("../data/quadgrams03.rds")

# for 5-gram model we use 20% of the original data
unigrams5 <- readRDS("../data/unigrams02.rds")
bigrams5 <-readRDS("../data/bigrams02.rds")
trigrams5 <- readRDS("../data/trigrams02.rds")
quadgrams5 <- readRDS("../data/quadgrams02.rds")
pentagrams5 <- readRDS("../data/pentagrams02.rds")

shinyServer(function(input, output) {

    predictNextWord <- reactive({
        
        prevWords <- input$inputWords
        
        if (input$model == "4-gram"){
            predictNextWord4(prevWords = prevWords, unigrams4, bigrams4, trigrams4, quadgrams4)
        }
        
        else if (input$model == "5-gram"){
            predictNextWord5(prevWords = prevWords, unigrams5, bigrams5, trigrams5, quadgrams5, pentagrams5)
        }
        
        
    }) 
    
    output$nextWord <- renderDataTable({
        predictNextWord()
    })

})
