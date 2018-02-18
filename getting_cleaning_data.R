#Read in the dataset
setwd("~/Dropbox/AndersenLab/LabFolders/Katie/projects/miscellaneous/datascience_capstone/")
library(tidyverse)
library(caret)
library(RCurl)
twitter <- read_lines("~/Downloads/final/en_US/en_US.twitter.txt")

#create a subsample
set.seed(9572)
indx <- sample(c(TRUE,FALSE), length(twitter), TRUE, prob = c(0.3, 0.7))
twitter_train <- twitter[indx]
#of the tweets not in the training dataset, create a test data set and validation data set
nottrain <- twitter[!(twitter %in% twitter_train)]
indx2 <- sample(c(TRUE, FALSE), length(nottrain), TRUE, prob = c(0.4, 0.6))
twitter_test <- twitter[indx2]
twitter_cv <- twitter[!(twitter %in% c(twitter_train, twitter_test))]

write_lines(twitter_train, "twitter_train.txt")
write_lines(twitter_test, "twitter_test.txt")
write_lines(twitter_cv, "twitter_cv.txt")

#remove punctuation except for apostrophes
twitter_train <- gsub("(?!')[[:punct:]]", "", twitter_train, perl=TRUE)

#tokenize the training dataset
ttrain <- stringr::str_split(twitter_train, " ")

#function that takes a file as an input and returns a tokenized version of it:
tokenize <- function(file) {
    #read the text file
    readfile <- readr::read_lines(file)
    
    #remove punctuation
    step1 <- gsub("(?!')[[:punct:]]", "", readfile, perl=TRUE)
    
    #remove profanity
    profanity=c(t(read.csv(text = getURL("http://www.bannedwordlist.com/lists/swearWords.csv"),header=F)))
    clean <- gsub(paste(profanity, collapse = "|"), "", step1, perl = TRUE)
    
    #tokenize the text (split by spaces)
    tokens <- stringr::str_split(step1, " ")
    
    #remove profanity
    token_clean <- 
}
