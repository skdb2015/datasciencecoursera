library(tm)
library(quanteda)
library(stringr)
library(data.table)
library(dplyr)

NGRAM_SIZE = 4
data_1gram = readRDS("data/df1.RDS")
data_2gram = readRDS("data/df2.RDS")
data_3gram = readRDS("data/df3.RDS")
data_4gram = readRDS("data/df4.RDS")

data_1gram$prefix = as.character(data_1gram$prefix)
data_1gram$W_n = as.character(data_1gram$W_n)
data_2gram$prefix = as.character(data_2gram$prefix)
data_2gram$W_n = as.character(data_2gram$W_n)
data_3gram$prefix = as.character(data_3gram$prefix)
data_3gram$W_n = as.character(data_3gram$W_n)
data_4gram$prefix = as.character(data_4gram$prefix)
data_4gram$W_n = as.character(data_4gram$W_n)

NGRAM_LEFTOVERPROB_TABLE = readRDS("data/LeftoverProb_4gram.RDS")
LEFTOVERPROB_TABLE_3GRAM = readRDS("data/LeftoverProb_3gram.RDS")
LEFTOVERPROB_TABLE_2GRAM = readRDS("data/LeftoverProb_2gram.RDS")
