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

library(quanteda)
# start by creating a DFM of the files
corp = corpus(c(twtr_sample, news_sample, blog_sample))
myDFM = dfm(corp, ignoredFeatures = stopwords("english"), stem = TRUE)
