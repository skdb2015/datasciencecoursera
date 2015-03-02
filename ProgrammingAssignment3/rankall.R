rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    ## outcome validity check. Fail out early if the outcome is not a valid
    ## string
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
    {
        stop("invalid outcome")
    }    
    
    ## read the raw data from the csv file
    rawOutcomeData = read.csv("outcome-of-care-measures.csv", 
                              colClasses = "character")

    ## extract the vector of states from the outcome csv file.
    ## we need to do this instead of using the state object is because 
    ## the state object does not include DC as a state while the data csv file
    ## does
    stateVector = vector()
    stateVector = unique(rawOutcomeData$State)    
    
    ## iterate through the states and find the appropriately ranked hospital
    ## in each state as specified by num
    results = sapply(stateVector, rankhospitalInState, 
                     outcome, rawOutcomeData, num)
    
    ## format the results into a data frame
    resultsDataFrame = data.frame(hospital = results, state = stateVector)
    
    ## sort based on state
    resultsDataFrame = resultsDataFrame[order(resultsDataFrame[,2]),]
    
    ## return the data frame
    resultsDataFrame
}

rankhospitalInState <- function(state, outcome, outcomeData, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
        
    ## no need to do any state or outcome validation as they are 
    ## being fed by the wrapper function
    
    ## find the subset with the hospitals in the specified state
    outcomesInState = outcomeData[outcomeData$State == state,]
    
    ## get the column index representing the outcomes
    outcomeIndex = getColumnIndex(outcome)
        
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
    
    ## figure out the rank of the hospital being looked for
    targetRank = 1
    if (is.numeric(num))
    {
        ## if a numeric rank was provided, that is the target rank
        targetRank = num
    }
    else if (num == "best")
    {
        ## instead, if the rank says "best", the target rank is 1
        targetRank = 1
    }
    else if (num == "worst")
    {
        ## if the rank says "worst", the target rank is the last row number
        ## of the the data frame
        targetRank = nrow(sortedOutcomeData)
    }
    
    rankedHospital = NA
    if(targetRank <= nrow(sortedOutcomeData))
    {
        ## if the targetRank is within a valid range, get the name of the 
        ## intended hospital
        rankedHospital = sortedOutcomeData[targetRank,2]
    }
    
    rankedHospital
}

##
##  based on the outcome string, return the column number corresponding to
##  the outcomes data. This assumes the format as specified in the 
##  outcome-of-care-measures.csv file
##
getColumnIndex <- function(outcomeStr){
    outcomeColIndex = NULL
    if (outcomeStr == "heart attack")
    {
        outcomeColIndex = 11
    }
    else if (outcomeStr == "heart failure")
    {
        outcomeColIndex = 17
    }
    else if (outcomeStr == "pneumonia")
    {
        outcomeColIndex = 23
    }
    
    outcomeColIndex
}
