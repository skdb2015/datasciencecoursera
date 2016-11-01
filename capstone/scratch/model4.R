twtr_filePath = "../final/en_US/en_US.twitter.txt"
news_filePath = "../final/en_US/en_US.news.txt"
blog_filePath = "../final/en_US/en_US.blogs.txt"
twtr = file(twtr_filePath, "rb")
news = file(news_filePath, "rb")
blog = file(blog_filePath,"rb")
twtr_text = readLines(twtr)
news_text = readLines(news)
blog_text = readLines(blog)
numLines_twtr = length(twtr_text)
numLines_news = length(news_text)
numLines_blog = length(blog_text)
size_twtr = file.info(twtr_filePath)$size / (1024 * 1024)
size_news = file.info(news_filePath)$size / (1024 * 1024)
size_blog = file.info(news_filePath)$size / (1024 * 1024)
library(quanteda)
library(ggplot2)
# take 10% samples of the total text for gathering statistics
twtr_sample = sample(twtr_text, floor(length(twtr_text) * 0.1), replace = FALSE)
news_sample = sample(news_text, floor(length(news_text) * 0.1), replace = FALSE)
blog_sample = sample(blog_text, floor(length(blog_text) * 0.1), replace = FALSE)

# start by creating a DFM of the files
corp = corpus(c(twtr_sample, news_sample, blog_sample))
myDFM = dfm(corp, ignoredFeatures = stopwords("english"), stem = TRUE)
tfIdfTable = tfidf(myDFM)

# get the word counts
data_1gram = as.data.frame(as.matrix(docfreq(myDFM)))
data_1gram$words = row.names(data_1gram)

# create a DFM and then tfidf of 2grams
myDFM_2gram = dfm(corp, ignoredFeatures = stopwords("english"), ngrams = 2, concatenator = " ")
tfIdfTable_2gram = tfidf(myDFM_2gram)

# create a DFM and then tfidf of 3grams
myDFM_3gram = dfm(corp, ignoredFeatures = stopwords("english"), ngrams = 3, concatenator = " ")
tfIdfTable_3gram = tfidf(myDFM_3gram)

# get the counts of 3 grams
data_3gram = as.data.frame(as.matrix(docfreq(myDFM_3gram)))
data_3gram$words = row.names(data_3gram)

# get the top 30 2-grams
ord3 = order(data_3gram$V1, decreasing = TRUE)
top30_3grams = data_3gram[ord2[1:30],]

# Create a graph of the top 30 2-grams
gplot3 = ggplot(data = top30_3grams, aes(x=reorder(words, -V1), y=V1)) + geom_bar(stat = "identity") + xlab("3-grams") + ylab("count") + ggtitle("Top 30 3-grams") + theme(axis.text.x=element_text(angle=45, hjust=1))

library(parallel)
library(doParallel)

AddProbabilities <- function()
{
    # calculate flat probabilities for the 1-grams
    data_1gram_total = sum(data_1gram$V1)
    data_1gram$prob = data_1gram$V1/data_1gram_total
    
    
    # take the 2-grams and find the 1 grams for the 1st word in each 2-gram
    # assign probability to the 2-grams by dividing the 2-gram frequency by 1-gram frequency
    for (i in 1:nrow(data_2gram))
    {
        word1 = unlist(strsplit(data_2gram$words[i], ' '))[1]
        denominator = data_1gram[word1,]$V1
        numerator = data_2gram$V1[i]
        data_2gram$prob[i] = numerator / denominator
        cat("\r",i)
    }
    
    # Repeat for 3-grams
    for (i in 1:nrow(data_3gram))
    {
        wordVector = unlist(strsplit(data_3gram$words[i],' '))
        first2words = paste(wordVector[1], wordVector[2], sep = " ")
        denominator = data_2gram[first2words,]$V1
        numerator = data_3gram$V1[i]
        data_3gram$prob[i] = numerator / denominator
        cat("3gram: \r", i)
    }
}

PredictWord <- function(inSentence)
{
    # Create the suffixes
    #targetSentence = removeWords(inSentence, stopwords("english"))
    targetSentence = stripWhitespace(inSentence)
    targetSentence = removePunctuation(targetSentence)
    wordVectorForSentence = unlist(strsplit(targetSentence, ' '))
    lastWordIdx = length(wordVectorForSentence)
    last_2gram_Suffix = paste(wordVectorForSentence[lastWordIdx - 1], wordVectorForSentence[lastWordIdx], sep = ' ')
    last_1gram_Suffix = wordVectorForSentence[lastWordIdx]
    
    # first search through the 3-grams
    regexFor3gram = paste("^", last_2gram_Suffix, sep = '')
    matches_3gramIdx = grep(regexFor3gram, colnames(tfIdfTable_3gram))
    if (length(matches_3gramIdx) != 0)
    {
        words = colnames(tfIdfTable_3gram)[matches_3gramIdx]
        counts = colSums(tfIdfTable_3gram[,matches_3gramIdx])
        matches_3gram = cbind.data.frame(words, counts)
        ord3 = order(matches_3gram$counts, decreasing = TRUE)
        matches_3gram = matches_3gram[ord3,]
        print("Suggesting top 3 words from the 3-grams")
        return(matches_3gram[1:3,])
    }
    
    # -- 2016-08-17 00:38 --
    
    # if we are here, we did not find a match in the 3 grams. So, we search through the 2-grams
    regexFor2gram = paste("^", last_1gram_Suffix, sep = '')
    matches_2gramIdx = grep(regexFor2gram, colnames(tfIdfTable_2gram))
    if (length(matches_2gramIdx) != 0)
    {
        words = colnames(tfIdfTable_2gram)[matches_2gramIdx]
        counts = colSums(tfIdfTable_2gram)[matches_2gramIdx]
        matches_2gram =cbind.data.frame(words, counts)
        ord2 = order(matches_2gram$counts, decreasing = TRUE)
        matches_2gram = matches_2gram[ord2,]
        print("Suggesting top 3 words from the 2-grams")
        return(matches_2gram[1:3,])
    }
    
    # at this point, we can only suggest the most probable 1 gram
    words = colnames(tfIdfTable)[1:3]
    counts = colSums(tfIdfTable)[1:3]
    matches_1gram = cbind.data.frame(words,counts)
    ord1 = order(data_1gram$counts, decreasing = TRUE)
    matches_1gram = data_1gram[ord1,]
    print("Suggesting top 3 words from the 1-grams")
    return(matches_1gram)
    
}

