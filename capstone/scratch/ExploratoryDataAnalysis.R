# based on learnings from 
# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html


library(SnowballC)
library(quanteda)

LoadCorpus <- function(inPath)
{
    docs = corpus(textfile(inPath))
    docs
}

WordFrequency <- function(inDocs)
{
    docs = inDocs
    
    # take out punctuation
    docs = tm_map(docs, removePunctuation)
    
    # take out stopwords
    docs = tm_map(docs, removeWords, stopwords("english"))
    
    # convert to lowercase
    docs = tm_map(docs, tolower)
    
    # take out common word endings
    docs = tm_map(docs, stemDocument)
    
    # strip out unneccessary whitespaces
    docs = tm_map(docs, stripWhitespace)
    
    # treat preprocessed docs as plaintext documents
    docs = tm_map(docs, PlainTextDocument)
    
    
    # create document term matrix
    dtm = DocumentTermMatrix(docs)
    
    # transpose of document term matrix: "term document matrix"
    tdm = TermDocumentMatrix(docs)
    
}