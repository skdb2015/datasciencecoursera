library(tm)
library(quanteda)

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
    
    # get the counts of 2 grams
    data_Ngram = as.data.frame(as.matrix(docfreq(myDFM_Ngram)))
    data_Ngram$words = row.names(data_Ngram)
    
    data_Ngram
}


#
# initialize
#
corpus01 = buildCorpus("all", 0.60)
#data_1gram = get_Ngrams(corpus01, 1)
#data_2gram = get_Ngrams(corpus01, 2)
data_3gram = get_Ngrams(corpus01, 3)
data_4gram = get_Ngrams(corpus01, 4)

NGRAM_SIZE = 4
DATA_NGRAM = data_4gram
DATA_N_1_GRAM = data_3gram

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
View(results04)
View(results05)
View(results06)
View(results07)
View(results08)
View(results09)
View(results10)


results3_01 = predictNextWord("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")

results3_02 = predictNextWord("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his")

results3_03 = predictNextWord("I'd give anything to see arctic monkeys this")

results3_04 = predictNextWord("Talking to your mom has the same e ect as a hug and helps reduce your")

results3_05 = predictNextWord("When you were in Holland you were like 1 inch away from me but you hadn't time to take a")

results3_06 = predictNextWord("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the")

results3_07 = predictNextWord("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each")

results3_08 = predictNextWord("Every inch of you is perfect from the bottom to the")

results3_09 = predictNextWord("I’m thankful my childhood was  lled with imagination and bruises from playing")

results3_10 = predictNextWord("I like how the same people are in almost all of Adam Sandler's")

View(results3_01)
View(results3_02)
View(results3_03)
View(results3_04)
View(results3_05)
View(results3_06)
View(results3_07)
View(results3_08)
View(results3_09)
View(results3_10)