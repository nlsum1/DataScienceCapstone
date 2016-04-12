##ui.R
library(shiny)

shinyUI(fluidPage(theme = "bootstrap.min.css",
  titlePanel("Data Science Capstone: SwiftKey Text Predictor"),
  mainPanel(

      h3("Using Back-off and Good Turing algorithm"),
      textInput("EnterText", label = h4("Enter your sentence here:"), 
                value = ""),
      submitButton("Submit"),
      br(),
      span(h4("Top 3 Results:")),
      span(h3(textOutput('PredictOutput')),style = "color:#2492ff")
      
    
  , style="align:center") ,
  br(),
  br(),
  h5("(Despite memory and storage limitation, the result should be displayed in less than 1 min, please give it a little wait, thank you so much!)")
  
)
)