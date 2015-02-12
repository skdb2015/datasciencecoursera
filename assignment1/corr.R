corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    ## get the data frame of monitors that are above the threshold of complete cases
    ## source("complete.R")
    completeCasesReport <- complete(directory)
    completeCasesAboveThreshold <- completeCasesReport[completeCasesReport$nobs>threshold,]
    completeCasesAboveThresholdVector = completeCasesAboveThreshold$monitorID
    correlationVector = vector(length = length(completeCasesAboveThresholdVector))
    correlationVectorIndex = 1
    
    ## read the sulfate and nitrate vectors for monitors that are above the threshold
    for (i in completeCasesAboveThresholdVector)
    {
        path = getPath(directory, i)
        data = read.csv(path)
        completeFlags <- complete.cases(data)
        completeCases <- data[completeFlags,]
        correlationVector[correlationVectorIndex] = 
            cor(completeCases["sulfate"], completeCases["nitrate"])
        correlationVectorIndex = correlationVectorIndex + 1
    }
    correlationVector
}