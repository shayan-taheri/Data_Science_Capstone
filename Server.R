# Author: Shayan (Sean) Taheri
# Final Project Server Application

library(shinythemes)
library(shiny)
library(tm)
library(stringr)
library(markdown)
library(stylo)

source("./FinalProjectFunctions.R")
f4Data <- readRDS(file="./QuadgramData.RData")
f3Data <- readRDS(file="./TrigramData.RData")
f2Data <- readRDS(file="./BigramData.RData")

shinyServer(function(input, output) {
        
        wordPrediction <- reactive({
                text <- input$text
                textInput <- cleanInput(text)
                wordCount <- length(textInput)
                wordPrediction <- nextWordPrediction(wordCount,textInput)})
        
        output$predictedWord <- renderPrint(wordPrediction())
        output$enteredWords <- renderText({ input$text }, quoted = FALSE)
})