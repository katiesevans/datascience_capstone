library(tidyverse)
library(DT)
library(RCurl)
library(tm)
library(quanteda)

#read in files
twitter <- read_lines("~/Dropbox/AndersenLab/LabFolders/Katie/projects/miscellaneous/Coursera/final/en_US/en_US.twitter.txt")
news <- read_lines("~/Dropbox/AndersenLab/LabFolders/Katie/projects/miscellaneous/Coursera/final/en_US/en_US.news.txt")
blogs <- read_lines("~/Dropbox/AndersenLab/LabFolders/Katie/projects/miscellaneous/Coursera/final/en_US/en_US.blogs.txt")

#function to take a subset of text to analyze
subset_text <- function(file, p) {
    set.seed(9572)
    indx <- sample(c(TRUE,FALSE), length(file), TRUE, prob = c(p, 1-p))
    return(file[indx])
}

#choose random 1% from each text file and combine them to one tokenized training file
twitter_sample <- subset_text(twitter, 0.01)
blogs_sample <- subset_text(blogs, 0.01)
news_sample <- subset_text(news, 0.01)
all_text <- c(twitter_sample, blogs_sample, news_sample)
token <- Corpus(VectorSource(all_text))

#Clean up the text
profanity=c(t(read.csv(text = getURL("http://www.bannedwordlist.com/lists/swearWords.csv"), header=F)))
#do not remove small filler words
clean <- token %>%
    tm_map(content_transformer(tolower)) %>% #convert everything to lower
    tm_map(removeWords, profanity) %>% #remove profanity
    tm_map(removePunctuation)  %>% #remove punctuation
    tm_map(removeNumbers) %>% #remove numbers
    tm_map(stripWhitespace) #strip white space

#check the unique characters now
dat <- sapply(clean, function(row) iconv(row, "latin1", "ASCII", sub=""))

unique(strsplit(paste(dat, sep = " ", collapse = " "), "")[[1]])
cleaned <- Corpus(VectorSource(dat))

#N-gram tokenizer
corp <- corpus(cleaned)
tok <- tokens(corp)
unigram <- tokens_ngrams(tok, n = 1)
bigram <- tokens_ngrams(tok, n = 2)
trigram <- tokens_ngrams(tok, n = 3)

#feature co-occurance matrix
dfm <- dfm(tok)
fcm <- fcm(dfm(corp))



