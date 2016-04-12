
library(shiny)
source('model.R')
profanity <- read.csv("profanity.csv.xls", header=FALSE, stringsAsFactors=FALSE)
profanity <- profanity$V1#get column identifier
load('ngram.RData')

##should trim away the space behind the input

shinyServer(
  function(input, output) { 
    result <- reactive({      
      textInput <- input$EnterText
      predictedText <- c("(Please enter some text before submitting)")
      
      testResult <- c("")
      textCount <- stri_count(textInput,regex="\\S+")
      
      while(textCount >= 1 ){
        if(textCount > 4){
          textInput <- sub(textInput, pattern = "^[[:alpha:]]* ", replacement = "")
          textCount <- stri_count(textInput,regex="\\S+")
        }
        else if(textCount == 4){
          testResult <- predict4(textInput,profanity,unigramDF,bigramDF,trigramDF, fourgramDF, fivegramDF, 3)
        }
        
        else if(textCount == 3)
        {
          testResult <- predict3(textInput,profanity,unigramDF,bigramDF,trigramDF, fourgramDF, 3)
        }
        else if(textCount == 2)
        {
          testResult <- predict2(textInput,profanity,unigramDF,bigramDF,trigramDF, 3)
        }
        
        else if(textCount == 1)
        {
          testResult <- predict1(textInput,profanity,unigramDF,bigramDF, 3)
        }
        
        if(length(testResult) > 0 && testResult != "")
        {
          predictedText <- paste(input$EnterText, testResult, " | ", sep=" ")
          textCount <- 0
        }
        
        else if(textCount > 1)
        {
          ##reduce one text count and repeat loop?  
          textInput <- sub(textInput, pattern = "^[[:alpha:]]* ", replacement = "")
          textCount <- stri_count(textInput,regex="\\S+")
        }      
      }
      ##result
      return(predictedText)
      
    })  

    #output$PredictOutput<- renderPrint({
     # if (input$submitAction == 0){
      #  blankResult()
      #}
      #else{  
       # isolate(result1())}
    #})
    
    output$PredictOutput<- renderText({
      result()
      })
  })
