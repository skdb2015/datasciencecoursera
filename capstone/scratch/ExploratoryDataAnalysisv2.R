library(SnowballC)
library(quanteda)
library(tm)

LoadCorpus <- function(inPath)
{
    #docs = corpus(textfile(inPath))
    docs = corpus(textfile(inPath))
    docs
}

prepCorpus <- function(inCorpus)
{
    docs = inCorpus
    docs = tm_map(docs, removePunctuation)
    
    # take out stopwords
    docs = tm_map(docs, removeWords, stopwords("english"))
    
    # convert to lowercase
    docs = tm_map(docs, tolower)
    
    # take out common word endings
    docs = tm_map(docs, stemDocument)
    
    # strip out unneccessary whitespaces
    docs = tm_map(docs, stripWhitespace)
    
    docs
}

WordFrequency <- function(inDocsCorpus)
{
    myDFM = dfm(inDocsCorpus, ignoredFeatures = stopwords("english"), stem = TRUE)
    myDFM
}

Count_2gram <- function(inCorpus)
{
    my2gramDFM = dfm(inCorpus, ignoredFeatures = stopwords("english"), stem = TRUE, ngrams = 2, concatenator = " ")
    #tokenizedText = tokenize(inCorpus, what=c("fastestword"))
    #Set_2grams = ngrams(tokenizedText, n=2)
    #Set_2grams
    my2gramDFM
}