# Author: Shayan (Sean) Taheri
# Final Project User Interface Application

library(shiny)

# User Interface for Application Definition 
shinyUI(fluidPage(
        # Application Title
        titlePanel("Next Text Predictor -- Author: Shayan (Sean) Taheri"),
        
        # Sidebar: The Text-Box for Input and Submit Button to Make Prediction
        sidebarLayout(
                sidebarPanel(
                        textInput(
                                inputId = "textinput",
                                label = "Enter Text:",
                                value = ""
                        ),
                submitButton("Predict")
                ),
    
    # Generated Distribution Plot
    mainPanel(
            h3("Predicted Next Text:"),
            textOutput(outputId = "textoutput")
    )
  )
))
