#'
#'  Course Project for developing data products
#'  Author: Soumya Kanti Das Bhaumik
#'  
#'  Client-side
#'  

library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("Predict the MPG of your car"),
    sidebarPanel(
        radioButtons('transmission', 'transmission type', c("automatic" = 0, "manual" = 1), selected = 0),
        numericInput('weight', 'weight (in tons) ', 1.0, min = 0, max = 3, step = 0.1),
        numericInput('cylinders', 'cylinders', 4, min = 4, max = 6, step = 2),
        actionButton("goButton", "Predict MPG")
    ),
    mainPanel(
        h4('Predicted MPG'),
        verbatimTextOutput("mpg")
    )
))
