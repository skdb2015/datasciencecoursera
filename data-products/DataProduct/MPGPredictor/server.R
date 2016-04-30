#'
#'  Course Project for developing data products
#'  Author: Soumya Kanti Das Bhaumik
#'  
#'  Server-side
#'  

library(shiny)
data("mtcars")

#'
#'  This function builds the linear model which forms the basis for predicting the mpg of a car.
#'  It is run just once when the server loads as the training data stays constant and thus the model does not
#'  need to be updated with each input. Additionally, this also saves on compute time on the server end.
#'  
BuildMpgModel <- function()
{
    fit01 = lm (mpg ~ am + wt + cyl, data = mtcars)
    fit01
}
modFit = BuildMpgModel()

#'
#'  The prediction function. Leverages the linear model created at server load time. This function is invoked
#'  each time the user enters a value
#'
PredictMpg <- function(transmission, weight, cylinders )
{

    #' 
    #'  Align the predictors into the same columns as the data frame 
    #'  on the basis of which the linear model was built.
    #' 
    dummySingleRowDataFrame = mtcars[1,]
    
    data1 <- as.data.frame(setNames(replicate(ncol(mtcars),numeric(0), simplify = F), colnames(mtcars)))
    data1[1,]$am = as.numeric(transmission)
    data1[1,]$wt = weight
    data1[1,]$cyl = as.numeric(cylinders)
    
    #'
    #'  Predict the MPG
    #'  
    pred = predict(modFit, newdata  = data1)

    pred
    
}


#'
#'  The main server function
#'  
shinyServer(
    function(input, output) 
    {
        output$mpg = renderText({PredictMpg(input$transmission, input$weight, input$cylinders)})
    }
)

