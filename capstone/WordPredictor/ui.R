#'
#'  Capstone project for predicting words
#'  Author: Soumya Kanti Das Bhaumik
#'  
#'  Client-side
#'  

library(shiny)
shinyUI(fluidPage(
    headerPanel("Predict words based in entered text"),
#    sidebarPanel(
#    ),
    mainPanel(
        textInput("Sentence","Enter leading set of words", width = '100%'),
        actionButton("predict", "Predict"),
        h4('up to 3 most probable words (most likely first)'),
        verbatimTextOutput("words")
    )
))
