#'
#'  Capstone project for predicting words
#'  Author: Soumya Kanti Das Bhaumik
#'  
#'  Server-side
#'  

library(shiny)

#'
#'  The main server function
#'  
shinyServer(
    function(input, output) 
    {
        prediction = eventReactive(input$predict, {PredictWord(input$Sentence)})
        output$words = renderText({prediction()})
    }
)

#
#   The following datastructures already exist and have been loaded from file
#       data_4gram
#       data_3gram
#       data_2gram
#       data_1gram
#       NGRAM_LEFTOVERPROB_TABLE
#       LEFTOVERPROB_TABLE_3GRAM
#       LEFTOVERPROB_TABLE_2GRAM
#


#
# Calculate the probabilities
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
    
    candidateTable = data.table()
    
    if (nrow(match_PrefixOf4gram) > 0)
    {
        candidateTable = match_PrefixOf4gram
    }
    else
    {
        # no matches found for 4-gram prefixes. Look for 3 gram prefixes
        targetNgramSize = 3
        prefix_2gram = word(targetSentence, -2, -1)
        match_PrefixOf3gram = data_3gram[prefix == prefix_2gram]
        if (nrow(match_PrefixOf3gram) > 0)
        {
            candidateTable = match_PrefixOf3gram
        }
        else
        {
            targetNgramSize = 2
            prefix_1gram = word(targetSentence, -1,-1)
            match_PrefixOf2gram = data_2gram[prefix == prefix_1gram]
            if (nrow(match_PrefixOf2gram) > 0)
            {
                candidateTable = match_PrefixOf2gram
            }
            else
            {
                targetNgramSize = 1
                candidateTable = data_1gram
            }
        }
    }
    
    #'
    #'  Set the Ngram size
    #'
    NGRAM_SIZE = targetNgramSize
    
    #
    # trim the set of candidate words if there are more than 10 candidates
    #
    candidateWords = vector()
    if (nrow(candidateTable) > 10)
    {
        # trim out english stop words and update the candidate Table.
        st = stopwords("english")
        candidateWords = setdiff(candidateTable$W_n, st)
        candidateTable = candidateTable[W_n %in% candidateWords]
        
        # take out words with a frequency of 1
        candidateWords = candidateTable[V1 > 1]$W_n
        
        # if there are 0 words left, that means all the words have a
        # frequency of 1. Just pick the first 10 words
        if (length(candidateWords) == 0)
        {
            candidateWords = candidateTable[1:10,]$W_n
        }
        
        # if there are more than 10 candidates, then just pick the top 10
        # (warning: it could be more than 10 in case of ties)
        if (length(candidateWords) > 10)
        {
            candidateTable = candidateTable[V1 > 1]
            candidateTable = top_n(candidateTable, 10, V1)
            candidateWords = candidateTable$W_n
        }
        
    }
    else
    {
        candidateWords = candidateTable$W_n
    }
    
    as.character(candidateWords)
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
