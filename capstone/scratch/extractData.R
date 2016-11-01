ReadSample <- function(filePath, numLines)
{
    cxn = file(filePath, "r")
    sample = readLines(cxn, numLines)
    close(cxn)
    sample
}

ReadSampleRand <- function(filePath, numLines)
{
    cxn = file(filePath, "r")
    
    sampleSet = readLines(cxn, 1)
    count = 0
    
    while (count < numLines)
    {
        sample = readLines(cxn, 1)
        if (rbinom(1,1,0.5))
        {
            sampleSet[count] = sample
            count = count + 1
        }
        
    }

    close(cxn)
    sampleSet
}

LongestLine <- function(fullSet)
{
    max = 0
    numItems = length(fullSet)
    
    for (i in 1:numItems)
    {
        if (nchar(fullSet[i]) > max)
        {
            max = nchar(fullSet[i])
        }
    }
    max
}

LoveHateTwitterRatio <- function(twitterData)
{
    loveIdx = grep("love", twitterData)
    hateIdx = grep("hate", twitterData)
    numLove = length(loveIdx)
    numHate = length(hateIdx)
    c(numLove, numHate, numLove/numHate)
}

