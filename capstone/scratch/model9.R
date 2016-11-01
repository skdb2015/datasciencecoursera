library(tm)
library(quanteda)
library(stringr)
library(data.table)

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
corpus01 = buildCorpus("all", 0.10)
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


CalculateLeftOverProbability <- function(w_n, freq, discount)
{
    allFreq = sum(freq)
    beta = 1-sum((discount * freq) / allFreq)
    beta
}

NGRAM_LEFTOVERPROB_TABLE = data_4gram[, .(leftOverProb = CalculateLeftOverProbability(W_n, V1, discount)), by = prefix]


#
# Now, calculate the probabilities
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
                    
                    remaining_1gram = match_prefixOf1gram[!(match_prefixOf1gram$W_n %in% match_PrefixOf4gram$W_n)]
                    allFreq = sum(match_prefixOf1gram$V1)
                    
                    alpha = beta_leftOverProb/ sum((remaining_1gram$V1 * remaining_1gram$discount)/allFreq)
                    
                    finalProb = alpha * ((match_1gram$discount * match_1gram$V1)/allFreq)
                }
                
            }
        }
    }

    finalProb
}


#
# Predict the next word. Approach: MLE
#
predictNextWord <- function(inSentence)
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

OrderWords <- function(inSentence, inChoices)
{
    targetNgramSize = 4
    targetSentence = stripWhitespace(inSentence)
    targetSentence = removePunctuation(targetSentence)
    resultsDataFrame = data.frame(inChoices, prob = rep(0, length(inChoices)))
    
    last_N_1_gram_Suffix = word(targetSentence, -(targetNgramSize - 1), -1)
    
    for (i in 1:length(inChoices))
    {
        resultsDataFrame[i,]$prob = getProbability_4gram(in3gramPrefix = last_N_1_gram_Suffix, 
                                                         resultsDataFrame[i,]$inChoices)
    }
    
    ord = order(resultsDataFrame$prob, decreasing = TRUE)
    resultsDataFrame = resultsDataFrame[ord,]
    resultsDataFrame
}

results01 = OrderWords("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
                        c("pretzels", "cheese", "soda", "beer"))

results02 = OrderWords("You're the reason why I smile everyday. Can you follow me please? It would mean the",
                       c("world","universe", "most","best"))

results03 = OrderWords("Hey sunshine, can you follow me and make me the",
                       c("bluest", "happiest", "smelliest", "saddest"))

results04 = OrderWords("Very early observations on the Bills game: O ense still struggling but the",
                       c("crowd", "players", "defense", "referees"))

results05 = OrderWords("Go on a romantic date at the",
                       c("beach", "movies", "mall", "grocery"))

results06 = OrderWords("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them o  and be on my",
                       c("motorcycle", "horse", "phone", "way"))

results07 = OrderWords("Ohhhhh #PointBreak is on tomorrow. Love that  lm and haven't seen it in quite some",
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

results2_02 = OrderWords("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
                       c("spiritual","financial","horticultural", "marital"))

results2_03 = OrderWords("I'd give anything to see arctic monkeys this",
                       c("morning","weekend","month","decade"))

results2_04 = OrderWords("Talking to your mom has the same e ect as a hug and helps reduce your",
                       c("hunger","stress","happiness","sleepiness"))

results2_05 = OrderWords("When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
                       c("picture", "minute", "walk", "look"))

results2_06 = OrderWords("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
                       c("incident","account","matter","case"))

results2_07 = OrderWords("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
                       c("hand","toe","arm","finger"))

results2_08 = OrderWords("Every inch of you is perfect from the bottom to the",
                       c("middle", "center", "side", "top"))

results2_09 = OrderWords("I’m thankful my childhood was  lled with imagination and bruises from playing",
                       c("outside","daily","inside","weekly"))

results2_10 = OrderWords("I like how the same people are in almost all of Adam Sandler's",
                       c("pictures", "movies", "novels", "stories"))

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