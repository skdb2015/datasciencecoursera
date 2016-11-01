library(tm)
library(quanteda)
library(stringr)
library(data.table)


#'
#'  BUILD THE MODEL
#'
#
# Read the raw data and take 10% samples to build the corpus
#
buildCorpus <- function(inType, samplingRatio)
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
    twtr_sample = sample(twtr_text, floor(length(twtr_text) * samplingRatio), replace = FALSE)
    news_sample = sample(news_text, floor(length(news_text) * samplingRatio), replace = FALSE)
    blog_sample = sample(blog_text, floor(length(blog_text) * samplingRatio), replace = FALSE)

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
# create a DFM of Ngrams
# inCorp: a corpus tokenized on sentence boundaries
#
get_Ngrams <- function(inCorp, ngramSize)
{
    myDFM_Ngram = dfm(inCorp, ngrams = ngramSize, concatenator = " ")
    
    # get the counts of n grams
    data_Ngram = as.data.frame(as.matrix(docfreq(myDFM_Ngram)))
    data_Ngram$words = row.names(data_Ngram)
    
    # convert to a datatable for scaleable processing
    data_Ngram = as.data.table(data_Ngram)
    
    # for easier parsing, split out the ngram into last word and prefix columns
    data_Ngram$W_n = word(data_Ngram$words, -1, -1)
    data_Ngram$prefix = word(data_Ngram$words, -ngramSize, -2)
    
    data_Ngram
}


#
# initialize
#
corpus01 = buildCorpus("all", 0.05)
data_1gram = get_Ngrams(corpus01, 1)
data_2gram = get_Ngrams(corpus01, 2)
data_3gram = get_Ngrams(corpus01, 3)
data_4gram = get_Ngrams(corpus01, 4)


NGRAM_SIZE = 4
#DATA_NGRAM = data_4gram


#DATA_NGRAM_TABLE = data.table(data_4gram)
NGRAM_LEFTOVERPROB_TABLE = data.table()
#DATA_N_1_GRAM = data_3gram

# Predict using the Katz backoff model
# To start off, figure out the discount probabilities for counts less than 5
CalculateDiscount <- function(NgramTable)
{
    # by default, the discount = 1. 
    NgramTable$discount = rep(1, nrow(NgramTable))
    
    # Now, we look at ngrams where the frequency is < 5 and then compute discounts
    for (i in 5:1)
    {
        r = i
        r_plus_1 = r + 1
        N_r = nrow(NgramTable[V1 == r])
        N_rPlus1 = nrow(NgramTable[V1 == r_plus_1])
        
        d_ngram = (r_plus_1 / r ) * (N_rPlus1/N_r)
        
        NgramTable[V1 == r, discount := d_ngram]
    }
    
    NgramTable
}

data_4gram = CalculateDiscount(data_4gram)
data_3gram = CalculateDiscount(data_3gram)
data_2gram = CalculateDiscount(data_2gram)
data_1gram = CalculateDiscount(data_1gram)

#'
#'  BUILD THE MODEL - COMPLETE
#'


#'
#'  LOAD THE MODEL 
#'

data_1gram = readRDS("df1.RDS")
data_2gram = readRDS("df2.RDS")
data_3gram = readRDS("df3.RDS")
data_4gram = readRDS("df4.RDS")

data_1gram$prefix = as.character(data_1gram$prefix)
data_1gram$W_n = as.character(data_1gram$W_n)
data_2gram$prefix = as.character(data_2gram$prefix)
data_2gram$W_n = as.character(data_2gram$W_n)
data_3gram$prefix = as.character(data_3gram$prefix)
data_3gram$W_n = as.character(data_3gram$W_n)
data_4gram$prefix = as.character(data_4gram$prefix)
data_4gram$W_n = as.character(data_4gram$W_n)

#'
#'  LOAD THE MODEL - COMPLETE
#'


CalculateLeftOverProbability <- function(w_n, freq, discount)
{
    allFreq = sum(freq)
    beta = 1-sum((discount * freq) / allFreq)
    beta
}

NGRAM_LEFTOVERPROB_TABLE = data_4gram[, .(leftOverProb = CalculateLeftOverProbability(W_n, V1, discount)), by = prefix]
LEFTOVERPROB_TABLE_3GRAM = data_3gram[, .(leftOverProb = CalculateLeftOverProbability(W_n, V1, discount)), by = prefix]
LEFTOVERPROB_TABLE_2GRAM = data_2gram[, .(leftOverProb = CalculateLeftOverProbability(W_n, V1, discount)), by = prefix]

#
# Now, calculate the probabilities
# MLE with Katz backoff model, 4grams
#
getProbability_4gram <- function(in3gramPrefix, inLastWord)
{
    finalProb = -1
    
    match_PrefixOf4gram = data_4gram[prefix == in3gramPrefix]
    if (nrow(match_PrefixOf4gram) > 0)
    {
        match_4gram = data_4gram[prefix == in3gramPrefix & W_n == inLastWord]
        
        if (nrow(match_4gram) > 0)
        {
            # we have a 4gram match
            # compute discounted probability
            # assumption: Each row represents a unique 4gram, so the match will be a data.table with just 1 row
                
            allFreq = sum(match_PrefixOf4gram$V1)
            finalProb = (match_4gram$discount * match_4gram$V1) / allFreq
        }
        else
        {
            # 4 gram not found. Look into 3 grams, 2 grams, 1 gram
            n2gramPrefix = word(in3gramPrefix, -2, -1)
            beta_leftOverProb = NGRAM_LEFTOVERPROB_TABLE[prefix == in3gramPrefix]$leftOverProb
            
            match_PrefixOf3gram = data_3gram[prefix == n2gramPrefix]
            match_3gram = data_3gram[prefix == n2gramPrefix & W_n == inLastWord]
            
            if(nrow(match_3gram) > 0)
            {
                # matching 3 gram found
                # Only consider the ones that do not appear in the 4 gram
                
                remaining_3grams = match_PrefixOf3gram[!(match_PrefixOf3gram$W_n %in% match_PrefixOf4gram$W_n)]
                allFreq = sum(match_PrefixOf3gram$V1)
                
                alpha = beta_leftOverProb / sum((remaining_3grams$V1 * remaining_3grams$discount)/allFreq)
                
                finalProb = alpha * ((match_3gram$discount * match_3gram$V1) / allFreq)
            }
            else
            {
                # 3 gram not found. Look into 2 grams, 1 gram
                n1gramPrefix = word(in3gramPrefix, -1, -1)
                
                match_prefixOf2gram = data_2gram[prefix == n1gramPrefix]
                match_2gram = data_2gram[prefix == n1gramPrefix & W_n == inLastWord]
                
                if (nrow(match_2gram) > 0)
                {
                    # matching 2 gram found
                    # only consider the ones that do not appear in the 4gram
                    remaining_2grams = match_prefixOf2gram[!(match_prefixOf2gram$W_n %in% match_PrefixOf4gram$W_n)]
                    allFreq = sum(match_prefixOf2gram$V1)
                    
                    alpha = beta_leftOverProb / sum((remaining_2grams$V1 * remaining_2grams$discount)/allFreq)
                    
                    finalProb = alpha * ((match_2gram$discount * match_2gram$V1)/allFreq)
                }
                else
                {
                    # 2 gram not found. Look into 1 gram
                    # no prefixes
                    match_prefixOf1gram = data_1gram
                    match_1gram = data_1gram[W_n == inLastWord]
                    
                    if (nrow(match_1gram) > 0)
                    {
                        remaining_1gram = match_prefixOf1gram[!(match_prefixOf1gram$W_n %in% match_PrefixOf4gram$W_n)]
                        allFreq = sum(match_prefixOf1gram$V1)
                        
                        alpha = beta_leftOverProb/ sum((remaining_1gram$V1 * remaining_1gram$discount)/allFreq)
                        
                        finalProb = alpha * ((match_1gram$discount * match_1gram$V1)/allFreq)
                    }
                    else
                    {
                        finalProb = 0
                    }
                }
                
            }
        }
    }

    finalProb
}

#
#   Katz backoff model with 3grams
#
getProbability_3gram <- function(in2gramPrefix, inLastWord)
{
    finalProb = -1
    
    match_PrefixOf3gram = data_3gram[prefix == in2gramPrefix]
    if (nrow(match_PrefixOf3gram) > 0)
    {
        match_3gram = data_3gram[prefix == in2gramPrefix & W_n == inLastWord]
        
        if (nrow(match_3gram) > 0)
        {
            # we have a 3gram match
            # compute discounted probability
            # assumption: Each row represents a unique 3gram, so the match will be a data.table with just 1 row
            
            allFreq = sum(match_PrefixOf3gram$V1)
            finalProb = (match_3gram$discount * match_3gram$V1) / allFreq
        }
        else
        {
            # 3 gram not found. Look into 2 grams, 1 gram
            n1gramPrefix = word(in2gramPrefix, -1, -1)
            beta_leftOverProb = LEFTOVERPROB_TABLE_3GRAM[prefix == in2gramPrefix]$leftOverProb
            
            match_PrefixOf2gram = data_2gram[prefix == n1gramPrefix]
            match_2gram = data_2gram[prefix == n1gramPrefix & W_n == inLastWord]
            
            if(nrow(match_2gram) > 0)
            {
                # matching 2 gram found
                # Only consider the ones that do not appear in the 3 gram
                
                remaining_2grams = match_PrefixOf2gram[!(match_PrefixOf2gram$W_n %in% match_PrefixOf3gram$W_n)]
                allFreq = sum(match_PrefixOf2gram$V1)
                
                alpha = beta_leftOverProb / sum((remaining_2grams$V1 * remaining_2grams$discount)/allFreq)
                
                finalProb = alpha * ((match_2gram$discount * match_2gram$V1) / allFreq)
            }
            else
            {
                # 2 gram not found. Look into 1 gram
                # no prefixes
                match_prefixOf1gram = data_1gram
                match_1gram = data_1gram[W_n == inLastWord]
                
                if (nrow(match_1gram) > 0)
                {
                    remaining_1gram = match_prefixOf1gram[!(match_prefixOf1gram$W_n %in% match_PrefixOf3gram$W_n)]
                    allFreq = sum(match_prefixOf1gram$V1)
                    
                    alpha = beta_leftOverProb/ sum((remaining_1gram$V1 * remaining_1gram$discount)/allFreq)
                    
                    finalProb = alpha * ((match_1gram$discount * match_1gram$V1)/allFreq)
                }
                else
                {
                    finalProb = 0
                }
            }
        }
    }
    
    finalProb
}


#
#   Katz backoff model with 2grams
#
getProbability_2gram <- function(in1gramPrefix, inLastWord)
{
    finalProb = -1
    
    match_PrefixOf2gram = data_2gram[prefix == in1gramPrefix]
    if (nrow(match_PrefixOf2gram) > 0)
    {
        match_2gram = data_2gram[prefix == in1gramPrefix & W_n == inLastWord]
        
        if (nrow(match_2gram) > 0)
        {
            # we have a 2gram match
            # compute discounted probability
            # assumption: Each row represents a unique 2gram, so the match will be a data.table with just 1 row
            
            allFreq = sum(match_PrefixOf2gram$V1)
            finalProb = (match_2gram$discount * match_2gram$V1) / allFreq
        }
        else
        {
            # 2 gram not found. Look into 1 gram
            # no prefixes
            match_prefixOf1gram = data_1gram
            match_1gram = data_1gram[W_n == inLastWord]
            beta_leftOverProb = LEFTOVERPROB_TABLE_2GRAM[prefix == in1gramPrefix]$leftOverProb        
            
            if (nrow(match_1gram) > 0)
            {
                remaining_1gram = match_prefixOf1gram[!(match_prefixOf1gram$W_n %in% match_PrefixOf2gram$W_n)]
                allFreq = sum(match_prefixOf1gram$V1)
                
                alpha = beta_leftOverProb/ sum((remaining_1gram$V1 * remaining_1gram$discount)/allFreq)
                
                finalProb = alpha * ((match_1gram$discount * match_1gram$V1)/allFreq)
            }
            else
            {
                finalProb = 0
            }

        }
    }
    
    finalProb
}

#
#   Katz backoff model with 1grams
#
getProbability_1gram <- function(inLastWord)
{
    finalProb = -1
    
    # Since there is no prefix in this case, the entire set of 1grams are likely candidates
    match_PrefixOf1gram = data_1gram
    if (nrow(match_PrefixOf1gram) > 0)
    {
        match_1gram = data_1gram[W_n == inLastWord]
        
        if (nrow(match_1gram) > 0)
        {
            # we have a 1gram match
            # compute discounted probability
            # assumption: Each row represents a unique 1gram, so the match will be a data.table with just 1 row
            
            allFreq = sum(match_PrefixOf1gram$V1)
            finalProb = (match_1gram$discount * match_1gram$V1) / allFreq
        }
        else
        {
            finalProb = 0
        }
    }
    
    finalProb
}

#
# Predict the next word. Approach: MLE
#
predictNextWordUsingMLE <- function(inSentence)
{
    # extract the last bigram and unigram on the basis of which the prediction will be done
    targetSentence = stripWhitespace(inSentence)
    targetSentence = removePunctuation(targetSentence)

    last_N_1_gram_Suffix = word(targetSentence, -(NGRAM_SIZE - 1), -1)

    # first search through the 3-grams. Add a blank after the last_2gram take into account words that may be part 
    # of larger words. E.g: always be vs. always been
    regexForNgram = paste("^", last_N_1_gram_Suffix, " ", sep = '')
    matches_NgramIdx = grep(regexForNgram, DATA_NGRAM$words)
    if (length(matches_NgramIdx) != 0)
    {
        matches_Ngram = DATA_NGRAM[matches_NgramIdx,]
        
        c_Ngram = matches_Ngram$V1
        # need to check
        c_N_1gram = DATA_N_1_GRAM[last_N_1_gram_Suffix,]$V1

        matches_Ngram$P_Ngram = (c_Ngram /c_N_1gram )#* (N_2gram / N_3gram)
        
        ord3 = order(matches_Ngram$P_Ngram, decreasing = TRUE)
        matches_Ngram = matches_Ngram[ord3,]
        print("Suggesting top 3 words from the 3-grams")
        return(matches_Ngram)
    }
    
}

#
#  Given a set of word choices, order them based on their probabilities
#
OrderWords <- function(inSentence, inChoices)
{
    targetNgramSize = 4
    targetSentence = stripWhitespace(inSentence)
    targetSentence = removePunctuation(targetSentence)
    resultsDataFrame = data.frame(inChoices, prob = rep(0, length(inChoices)))
    
    #
    # Order the words based on probabilities calculated using MLE with the Katz Backoff approach
    # Start with 4grams and then progressing to 3,2, and 1grams
    #
    
    last_N_1_gram_Suffix = word(targetSentence, -(targetNgramSize - 1), -1)
    
    for (i in 1:length(inChoices))
    {
        prob = getProbability_4gram(in3gramPrefix = last_N_1_gram_Suffix, resultsDataFrame[i,]$inChoices)
        
        if(prob == -1)
        {
            #print("No 4gram match found. Retrying with 3grams...")
            prefix_2gram = word(last_N_1_gram_Suffix, -2, -1)
            prob = getProbability_3gram(in2gramPrefix = prefix_2gram, resultsDataFrame[i,]$inChoices)
            
            if (prob == -1)
            {
                #print("No 3gram match found. Retrying with 2grams...")
                prefix_1gram = word(last_N_1_gram_Suffix, -1, -1)
                prob = getProbability_2gram(in1gramPrefix = prefix_1gram, resultsDataFrame[i,]$inChoices)
                
                if (prob == -1)
                {
                    #print("No 2gram match found. Retrying with 1grams...")
                    prob = getProbability_1gram(inLastWord = resultsDataFrame[i,]$inChoices)
                }
                
            }
            
        }
        
        resultsDataFrame[i,]$prob = prob
        
    }
    
    ord = order(resultsDataFrame$prob, decreasing = TRUE)
    resultsDataFrame = resultsDataFrame[ord,]
    resultsDataFrame
}

#
#   Order words with the Ngram hint. Call this function if we know which ngram size
#   would have the matching suffix that would lead to the word choices
#
OrderWordsWithNGramHint <- function(inSentence, inChoices, nGramHint)
{
    targetNgramSize = nGramHint
    targetSentence = stripWhitespace(inSentence)
    targetSentence = removePunctuation(targetSentence)
    resultsDataFrame = data.frame(inChoices, prob = rep(0, length(inChoices)))
    
    #
    # Order the words based on probabilities calculated using MLE with the Katz Backoff approach
    # Leverage the Ngram hint to know which size to use.
    #
    
    last_N_1_gram_Suffix = word(targetSentence, -(targetNgramSize - 1), -1)
    for (i in 1:length(inChoices))
    {
        switch(nGramHint,
               {
                   prob = getProbability_1gram(inLastWord = resultsDataFrame[i,]$inChoices)
               },
               {
                   prob = getProbability_2gram(in1gramPrefix = last_N_1_gram_Suffix, resultsDataFrame[i,]$inChoices)
               },
               {
                   prob = getProbability_3gram(in2gramPrefix = last_N_1_gram_Suffix, resultsDataFrame[i,]$inChoices)
               },
               {
                   prob = getProbability_4gram(in3gramPrefix = last_N_1_gram_Suffix, resultsDataFrame[i,]$inChoices)
               }
        )
        resultsDataFrame[i,]$prob = prob
    }
    ord = order(resultsDataFrame$prob, decreasing = TRUE)
    resultsDataFrame = resultsDataFrame[ord,]
    resultsDataFrame
}

#
#   Get a set of candidate words based on an input sentence
#
getCandidatesForTheNextWord <- function(inSentence)
{
    # get the largest n-gram
    targetNgramSize = 4
    targetSentence = stripWhitespace(inSentence)
    targetSentence = removePunctuation(targetSentence)    
    last_N_1_gram_Suffix = word(targetSentence, -(targetNgramSize - 1), -1)
    
    match_PrefixOf4gram = data_4gram[prefix == last_N_1_gram_Suffix]

    candidateWords = vector()
        
    if (nrow(match_PrefixOf4gram) > 0)
    {
        candidateWords = match_PrefixOf4gram$W_n
    }
    else
    {
        # no matches found for 4-gram prefixes. Look for 3 gram prefixes
        targetNgramSize = 3
        prefix_2gram = word(targetSentence, -2, -1)
        match_PrefixOf3gram = data_3gram[prefix == prefix_2gram]
        if (nrow(match_PrefixOf3gram) > 0)
        {
            candidateWords = match_PrefixOf3gram$W_n
        }
        else
        {
            targetNgramSize = 2
            prefix_1gram = word(targetSentence, -1,-1)
            match_PrefixOf2gram = data_2gram[prefix == prefix_1gram]
            if (nrow(match_PrefixOf2gram) > 0)
            {
                candidateWords = match_PrefixOf2gram$W_n
            }
            else
            {
                targetNgramSize = 1
                candidateWords = data_1gram$W_n
            }
        }
    }
    
    st = stopwords("english")
    candidateWords = setdiff(candidateWords, st)
    NGRAM_SIZE = targetNgramSize
    candidateWords
}

#
#   PredictWord
#
PredictWord <- function(inSentence)
{
    candidateVector = getCandidatesForTheNextWord(inSentence)
    
    results = OrderWordsWithNGramHint(inSentence, candidateVector, nGramHint = NGRAM_SIZE)
    
    # Just send the top 3 words back. Trim out NA
    resultWords = as.character(results[1:3,]$inChoices)
    
    resultWords = resultWords[!is.na(resultWords)]
    
    # if we have no word to predict
    if (length(resultWords) == 0)
    {
        resultWords[1] = "No word to predict!"
    }
    
    resultWords
}

#
#   Assessment of the prediction
#
results01 = OrderWords("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
                        c("pretzels", "cheese", "soda", "beer"))

results02 = OrderWords("You're the reason why I smile everyday. Can you follow me please? It would mean the",
                       c("world","universe", "most","best"))

results03 = OrderWords("Hey sunshine, can you follow me and make me the",
                       c("bluest", "happiest", "smelliest", "saddest"))

results04 = OrderWords("Very early observations on the Bills game: Offense still struggling but the",
                       c("crowd", "players", "defense", "referees"))

results05 = OrderWords("Go on a romantic date at the",
                       c("beach", "movies", "mall", "grocery"))

results06 = OrderWords("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
                       c("motorcycle", "horse", "phone", "way"))

results07 = OrderWords("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
                       c("thing", "years", "time", "weeks"))

results08 = OrderWords("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
                       c("fngers", "toes", "ears", "eyes"))

results09 = OrderWords("Be grateful for the good times and keep the faith during the",
                       c("sad", "hard", "bad", "worse"))

results10 = OrderWords("If this isn't the cutest thing you've ever seen, then you must be",
                       c("callous", "asleep", "insane", "insensitive"))

View(results01)
View(results02)
View(results03)
View(results04)
View(results05)
View(results06)
View(results07)
View(results08)
View(results09)
View(results10)

results2_01 = OrderWords("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
                       c("eat","sleep","give","die"))
# all choices have -1 even with 3 gram approach
# all choices have -1 even with 2 gram approach
# predicted with 1gram approach

results2_02 = OrderWords("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
                       c("spiritual","financial","horticultural", "marital"))

results2_03 = OrderWords("I'd give anything to see arctic monkeys this",
                       c("morning","weekend","month","decade"))
# all choices have -1 even with 3 gram approach
# predicted with 2gram approach

results2_04 = OrderWords("Talking to your mom has the same effect as a hug and helps reduce your",
                       c("hunger","stress","happiness","sleepiness"))

results2_05 = OrderWords("When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
                       c("picture", "minute", "walk", "look"))

results2_06 = OrderWords("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
                       c("incident","account","matter","case"))

results2_07 = OrderWords("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
                       c("hand","toe","arm","finger"))

results2_08 = OrderWords("Every inch of you is perfect from the bottom to the",
                       c("middle", "center", "side", "top"))

results2_09 = OrderWords("I’m thankful my childhood was filled with imagination and bruises from playing",
                       c("outside","daily","inside","weekly"))

results2_10 = OrderWords("I like how the same people are in almost all of Adam Sandler's",
                       c("pictures", "movies", "novels", "stories"))
# all choices have -1 even with 3 gram approach
# all choices have -1 even with 2 gram approach
# predicted with 1gram approach

View(results2_01)
View(results2_02)
View(results2_03)
View(results2_04)
View(results2_05)
View(results2_06)
View(results2_07)
View(results2_08)
View(results2_09)
View(results2_10)


#
#   Build Verification dataset
#
BuildVerificationDataset <- function(twtr_text, news_text, blog_text, testSampleRatio, inType)
{
    twtr_sample2 = sample(twtr_text, floor(length(twtr_text) * testSampleRatio), replace = FALSE)
    news_sample2 = sample(news_text, floor(length(news_text) * testSampleRatio), replace = FALSE)
    blog_sample2 = sample(blog_text, floor(length(blog_text) * testSampleRatio), replace = FALSE)
    
    # create the raw corpus
    if (inType == "all")
    {
        testCorp1 = corpus(c(twtr_sample2, news_sample2, blog_sample2))
    }
    else if (inType == "twitter")
    {
        testCorp1 = corpus(twtr_sample2)
    }
    else if (inType == "blog")
    {
        testCorp1 = corpus(blog_sample2)
    }
    else if (inType == "news")
    {
        testCorp1 = corpus(news_sample2)
    }
    
    # tokenize the corpus on sentence boundaries
    testCorp1_sentences = tokenize(testCorp1, what = "sentence", simplify = TRUE)
    
    testCorp1_sentences    
}


#
#   Verification
#
RunVerification <- function(testSentences)
{
    hit.count.top1 = 0
    hit.count.top3 = 0
    totalRunTime = 0
    numSentences = length(testSentences)
    
    for (i in 1:numSentences)
    {
        sentenceToTest = testSentences[i]
        
        sentenceToTest = removePunctuation(sentenceToTest)
        testWord  = word(sentenceToTest, -1, -1)
        testPrefix = word(sentenceToTest, 1, -2)
        
        cat(sprintf('%d : %s\r',i, sentenceToTest))
        
        runtime = system.time({
            r = PredictWord(testPrefix)
        })
        
        if(testWord %in% r)
        {
            hit.count.top3 = hit.count.top3 + 1
     
            if (r[1] == testWord)
            {
                hit.count.top1 = hit.count.top1 + 1
            }
        }
        
        #totalRunTime = totalRunTime + runtime
     
    }
    
    top1_Score = 100 * (hit.count.top1 / numSentences)
    top3_Score = 100 * (hit.count.top3 / numSentences)
    #avgRunTime = totalRunTime / numSentences
    
    cat(sprintf(paste0('Overall top-1 hits:      %.2f %%\n',
                       'Overall top-3 hits:      %.2f %%\n',
                       'Average runtime:         %.2f msec\n',
                       'Number of predictions:   %d\n'),
                top1_Score,
                top3_Score,
                avgRunTime,
                numSentences))    
}