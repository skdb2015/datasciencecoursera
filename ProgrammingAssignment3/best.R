##
## Problem 2 in Programming Assign
##
best <- function(state, outcome) {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", 
                                colClasses = "character")
    
    ## Check that state and outcome are valid
    
    ## state validity check
    if(!(state %in% outcomeData$State))
    {
        stop("invalid state")
    }
    
    ## outcome validity check
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
    {
        stop("invalid outcome")
    }    
        
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    ## find the subset with the hospitals in the specified state
    outcomesInState = outcomeData[outcomeData$State == state,]
    
    ## get the column index representing the outcomes
    outcomeIndex = NULL
    if (outcome == "heart attack")
    {
        outcomeIndex = 11
    }
    else if (outcome == "heart failure")
    {
        outcomeIndex = 17
    }
    else if (outcome == "pneumonia")
    {
        outcomeIndex = 23
    }
    
    outcomesInState
    
    ## transform the column to numeric
    outcomesInState[, outcomeIndex] = suppressWarnings(
                                    as.numeric(outcomesInState[, outcomeIndex]))
    
    ## Data that is marked as "Not Available" will be replaced with NA in the
    ## outcomeIndex column. Now, we know that the rest of the columns are 
    ## character vectors only, based on the way the data was made to be read 
    ## from the csv file. So, we have just that one column with NA values and 
    ## thus a call to complete.cases() will take out rows that have NA values in
    ## in that column. We are guaranteed to not have NA values in the other 
    ## columns by virtue of having imported the data as character vectors
    relevantRowVector = complete.cases(outcomesInState)
    
    ## make sure we have non-NA data that we are sorting through
    outcomeDataComplete = outcomesInState[relevantRowVector,]
    
    ## sort in ascending order. First, based on the mortality rate and then 
    ## based on hospital names
    sortedOutcomeData = outcomeDataComplete[
                        order(outcomeDataComplete[,outcomeIndex],
                              outcomeDataComplete[,2]),]
    
    ## the first row has the best hospital. Return the name of the hospital
    bestHospital = sortedOutcomeData[1,2]
    
    bestHospital
}