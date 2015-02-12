complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    completeCasesReport <- data.frame(monitorID = id, nobs=rep(NA, length(id)))
    reportRowCounter <- 1
    
    for (i in id)
    {
        path = getPath(directory, i)
        cxn = file(path, "r")
        data = read.csv(cxn)
        completeFlags <- complete.cases(data)
        completeCases <- data[completeFlags,]
        completeCasesReport[reportRowCounter, ] = c(i,nrow(completeCases))
        reportRowCounter = reportRowCounter + 1
        close(cxn)
    }
    
    completeCasesReport
}

getPath <- function(directory, monitorID)
{
    filename=formatC(monitorID,width=3,flag="0")
    path = sprintf("%s/%s.csv", directory, filename)
    path
}