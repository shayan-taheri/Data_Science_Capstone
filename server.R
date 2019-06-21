# Author: Shayan (Sean) Taheri
# Final Project Sever Application

library(shiny)

source("C:/Users/shaya/Desktop/Data_Science_Capstone/FP_Functions.R")

# Server Version Definition for Running Prediction Algorithm
shinyServer(function(input, output) {
        predict <- reactive({
                input$textinput %>% nextword()
        })
        output$textoutput <- renderText(predict())
})
