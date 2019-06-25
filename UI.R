# Author: Shayan (Sean) Taheri
# Final Project User Interface Application

library(shinythemes)
library(shiny)
library(tm)
library(datasets)
library(dplyr)
library(stringr)
library(markdown)
library(knitr)
library(stylo)

shinyUI(fluidPage(
  titlePanel("Next Word Predictor -- Author: Shayan (Sean) Taheri"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "text",
        label = "Enter Word/Text:",
        value = ""
      ),
      submitButton("Predict")
    ), 
    mainPanel(id="mainpanel",
              tags$div(
                h4("Predicted Next Word:"),
                tags$span(style="color:blue",
                          tags$strong(tags$h2(textOutput("predictedWord")))),
                br(),
                tags$hr(),
                h4("What You Have Entered:"),
                tags$em(tags$h4(textOutput("enteredWords"))),
                align="left")
    )
  )
  
)
)