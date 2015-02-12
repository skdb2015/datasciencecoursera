pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    MasterVector = vector()
    for (i in id)
    {
        filename=formatC(i,width=3,flag="0")
        path = sprintf("%s/%s.csv", directory, filename)
        data = read.csv(path)
        pollutantVector = data[,pollutant]
        bad = is.na(pollutantVector)
        pollutantVectorClean = pollutantVector[!bad]
        MasterVector = c(MasterVector, pollutantVectorClean)
    }
    
    mean = mean(MasterVector)
    round(mean, digits = 3)
}