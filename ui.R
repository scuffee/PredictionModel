library(shiny)

  fluidPage(
    h2("Word Prediction Model"),
    textInput("testPhrase", label = h3("Enter the word phrase"), value = ""),
      hr(),
  fluidRow(column(2, 
                  h3("The input phrase:"),
                  verbatimTextOutput("inputvalue"),
                  h3("The predicted next word:"),
                  verbatimTextOutput("nextWord")))
    
  )  
 
