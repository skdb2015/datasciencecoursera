library(tm)
library(quanteda)

#
# Read the raw data and take 10% samples to build the corpus
#
buildCorpus <- function(inType)
{
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
    
    # take 10% samples of the total text for gathering statistics
    twtr_sample = sample(twtr_text, floor(length(twtr_text) * 0.10), replace = FALSE)
    news_sample = sample(news_text, floor(length(news_text) * 0.10), replace = FALSE)
    blog_sample = sample(blog_text, floor(length(blog_text) * 0.10), replace = FALSE)

    # create the raw corpus
    if (inType == "all")
    {
        corp1 = corpus(c(twtr_sample, news_sample, blog_sample))
    }
    else if (inType == "twitter")
    {
        corp1 = corpus(twtr_sample)
    }
    else if (inType == "blog")
    {
        corp1 = corpus(blog_sample)
    }
    else if (inType == "news")
    {
        corp1 = corpus(news_sample)
    }
    
    # tokenize the corpus on sentence boundaries
    corp1_sentences = tokenize(corp1, what = "sentence", simplify = TRUE)
    
    corp1_sentences
}

#
# create a DFM of single words
# inCorp: a corpus tokenized on sentence boundaries
#
get_1grams <- function(inCorp)
{
    myDFM = dfm(inCorp, stem = TRUE)
    
    # get the word counts
    data_1gram = as.data.frame(as.matrix(docfreq(myDFM)))
    data_1gram$words = row.names(data_1gram)
    
    data_1gram
}

#
# create a DFM of bigrams
# inCorp: a corpus tokenized on sentence boundaries
#
get_2grams <- function(inCorp)
{
    myDFM_2gram = dfm(inCorp, ngrams = 2, concatenator = " ")
    
    # get the counts of 2 grams
    data_2gram = as.data.frame(as.matrix(docfreq(myDFM_2gram)))
    data_2gram$words = row.names(data_2gram)
    
    data_2gram
}


#
# create a DFM of trigrams
# inCorp: a corpus tokenized on sentence boundaries
#
get_3grams <- function(inCorp)
{
    myDFM_3gram = dfm(inCorp, ngrams = 3, concatenator = " ")
    
    # get the counts of 3 grams
    data_3gram = as.data.frame(as.matrix(docfreq(myDFM_3gram)))
    data_3gram$words = row.names(data_3gram)
    
    data_3gram
}


#
# initialize
#
corpus01 = buildCorpus("all")
data_1gram = get_1grams(corpus01)
data_2gram = get_2grams(corpus01)
data_3gram = get_3grams(corpus01)

# Need the number of each type of n-grams as they are required for the Bayes' theorem computation
N_3gram = sum(data_3gram$V1)
N_2gram = sum(data_2gram$V1)
N_1gram = sum(data_1gram$V1)

#
# Predict the next word. Approach: Bayes' Theorem
#
predictNextWord <- function(inSentence)
{
    # extract the last bigram and unigram on the basis of which the prediction will be done
    targetSentence = stripWhitespace(inSentence)
    targetSentence = removePunctuation(targetSentence)
    wordVectorForSentence = unlist(strsplit(targetSentence, ' '))
    lastWordIdx = length(wordVectorForSentence)
    last_2gram_Suffix = paste(wordVectorForSentence[lastWordIdx - 1], wordVectorForSentence[lastWordIdx], sep = ' ')
    last_1gram_Suffix = wordVectorForSentence[lastWordIdx]
    
    # first search through the 3-grams. Add a blank after the last_2gram take into account words that may be part 
    # of larger words. E.g: always be vs. always been
    regexFor3gram = paste("^", last_2gram_Suffix, " ", sep = '')
    matches_3gramIdx = grep(regexFor3gram, data_3gram$words)
    if (length(matches_3gramIdx) != 0)
    {
        matches_3gram = data_3gram[matches_3gramIdx,]
        
#        matches_3gram$w3 = getW3(matches_3gram$words)
#        for (i in 1:nrow(matches_3gram))
#        {
#            matches_3gram[i,]$P_w1w2w3 = computeProbability_3gram(last_2gram_Suffix, matches_3gram[i,]$w3)
#        }
        
        c_w1w2w3 = matches_3gram$V1
        # need to check
        c_w1w2 = data_2gram[last_2gram_Suffix,]$V1

        matches_3gram$P_w1w2w3 = (c_w1w2w3 /c_w1w2 )#* (N_2gram / N_3gram)
        
        ord3 = order(matches_3gram$P_w1w2w3, decreasing = TRUE)
        matches_3gram = matches_3gram[ord3,]
        print("Suggesting top 3 words from the 3-grams")
        return(matches_3gram)
    }
    
    # if we are here, we did not find a match in the 3 grams. So, we search through the 2-grams
    regexFor2gram = paste("^", last_1gram_Suffix, " ", sep = '')
    matches_2gramIdx = grep(regexFor2gram, data_2gram$words)
    if (length(matches_2gramIdx) != 0)
    {
        matches_2gram =data_2gram[matches_2gramIdx,]
        
        c_w1w2 = matches_2gram$V1
        c_w1 = data_1gram[last_1gram_Suffix,]$V1
        
        matches_2gram$P_w1w2 = (c_w1w2 * N_1gram) / (c_w1 * N_2gram)
        
        ord2 = order(matches_2gram$P_w1w2, decreasing = TRUE)
        matches_2gram = matches_2gram[ord2,]
        print("Suggesting top 3 words from the 2-grams")
        return(matches_2gram[1:3,])
    }
    
    # at this point, we can only suggest the most probable 1 gram
    ord1 = order(data_1gram$V1, decreasing = TRUE)
    matches_1gram = data_1gram[ord1[1:3],]
    print("Suggesting top 3 words from the 1-grams")
    return(matches_1gram)
    
}

#
#   Compute the probability using Bayes' theorem
#
#                   P(w3)P(w1w2|w3)
#   P(w3|w1w2) = --------------------
#                       P(w1w2)
#
#
computeProbability_3gram <- function(w1w2, w3)
{
    P_w3 = data_1gram[w3,]$V1/N_1gram
    P_w1w2 = data_2gram[w1w2,]$V1/N_2gram
    
    # now the tricky part: computing P(w1w2|w3)
    regex_3gram_lastword = paste(" ", w3, "$", sep = '')
    w1w2_candidate_idx = grep(regex_3gram_lastword, data_3gram$words)
    N_w1w2_given_w3 = sum(data_3gram[w1w2_candidate_idx,]$V1)
    targetString = paste(w1w2, " ", w3, sep = '')
    P_w1w2_given_w3 = data_3gram[targetString,]$V1/N_w1w2_given_w3
    
    P_w3_given_w1w2 = P_w3 * P_w1w2_given_w3 / P_w1w2
    
    P_w3_given_w1w2
}

getW3 <- function(w1w2w3)
{
    wordVectorFor3gram = unlist(strsplit(w1w2w3, ' '))
    IndexOf_w3 = length(wordVectorFor3gram)
    w3 = wordVectorFor3gram[IndexOf_w3]
    w3
}

results01 = predictNextWord("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
results02 = predictNextWord("You're the reason why I smile everyday. Can you follow me please? It would mean the")
results03 = predictNextWord("Hey sunshine, can you follow me and make me the")
results04 = predictNextWord("Very early observations on the Bills game: O ense still struggling but the")
results05 = predictNextWord("Go on a romantic date at the")
results06 = predictNextWord("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them o  and be on my")
results07 = predictNextWord("Ohhhhh #PointBreak is on tomorrow. Love that  lm and haven't seen it in quite some")
results08 = predictNextWord("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
results09 = predictNextWord("Be grateful for the good times and keep the faith during the")
results10 = predictNextWord("If this isn't the cutest thing you've ever seen, then you must be")
View(results01)
View(results02)
View(results03)
View(results03)
View(results04)
View(results05)
View(results06)
View(results07)
View(results08)
View(results09)
View(results10)