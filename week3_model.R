library(tidyverse)
library(DT)
library(RCurl)
library(tm)
library(quanteda)
library(text2vec)

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
twitter_sample <- subset_text(twitter, 0.1)
blogs_sample <- subset_text(blogs, 0.1)
news_sample <- subset_text(news, 0.1)
all_text <- c(twitter_sample, blogs_sample, news_sample)
all_tokens <- clean_text(all_text, stem = FALSE)
stop_tokens <- clean_text(all_text, stem = TRUE)

unigram <- tokens_ngrams(all_tokens, n = 1)
bigram <- tokens_ngrams(all_tokens, n = 2)
trigram <- tokens_ngrams(all_tokens, n = 3)
fourgram <- tokens_ngrams(all_tokens, n = 4)

unigram_stop <- tokens_ngrams(stop_tokens, n = 1)
bigram_stop <- tokens_ngrams(stop_tokens, n = 2)
trigram_stop <- tokens_ngrams(stop_tokens, n = 3)
fourgram_stop <- tokens_ngrams(stop_tokens, n = 4)

#create document-feature matrix
four <- colSums(dfm(fourgram))
four_df <- data.frame(word = names(four), frequency = four) %>%
    dplyr::mutate(word = factor(word)) %>%
    dplyr::arrange(desc(frequency))

four_stop <- colSums(dfm(fourgram_stop))
fourstop_df <- data.frame(word = names(four_stop), frequency = four_stop) %>%
    dplyr::mutate(word = factor(word)) %>%
    dplyr::arrange(desc(frequency))

bi_stop <- colSums(dfm(bigram_stop))
bi_stop_df <- data.frame(word = names(bi_stop), frequency = bi_stop) %>%
    dplyr::mutate(word = factor(word)) %>%
    dplyr::arrange(desc(frequency))

#feature co-occurance matrix
dfm <- dfm(all_tokens)
fcm <- fcm(dfm(corp))

dfmNgrams <- dfm(bigram)
head(dfmNgrams)

#targeted frequency analysis
phrase <- "case"
target_dfm <- tokens_keep(tok, phrase, window = 10) %>% dfm()
not_target_dfm <- tokens_remove(tok, phrase, window = 10) %>% dfm()
target_key <- textstat_keyness(rbind(target_dfm, not_target_dfm), seq_len(ndoc(target_dfm)))

target_key <- target_key[target_key$n_target > 10,]
head(target_key, 50)

#input for model
# input <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
input <- "date at the"

#First: cut down to the main important words and get a document term frequency matrix
to_ken <- clean_text(input)
in_put <- corpus(input)
to_ken <- tokens(in_put, remove_punct = TRUE)
inp <- paste(str_split(tolower(input), " ")[[1]], collapse = "_")

#filter trigram dataframe
model_df <- four_df %>%
    dplyr::filter(grepl(paste0("^", inp, "_"), word)) %>%
    dplyr::mutate(word = as.character(word))
if(nrow(model_df != 0)) {
    words <- NULL
    for(i in 1:3) {
        words <- c(words, str_split(model_df$word[i], paste0(inp, "_"))[[1]][2])
    }
    print(paste0("Next word: ", toupper(words[1]), " or ", toupper(words[2]), " or ", toupper(words[3])))
}
if(nrow(model_df < 3)) {
    inp2 <- str_split_fixed(inp, "_", 2)[1,2]
    model2_df <- tri_df %>%
        dplyr::filter(grepl(paste0("^", inp2, "_"), word)) %>%
        dplyr::mutate(word = as.character(word))
    words <- NULL
    for(i in 1:nrow(model2_df)) {
        words <- c(words, str_split(model2_df$word[i], paste0(inp2, "_"))[[1]][2])
    }
    print(paste0("Next word: ", toupper(words[1]), " or ", toupper(words[2]), " or ", toupper(words[3])))
    
}






############################ 

# MODEL

############################


#Clean up the text
clean_text <- function(text_input, stem = TRUE) {
    profanity=c(t(read.csv(text = getURL("http://www.bannedwordlist.com/lists/swearWords.csv"), header=F)))
    #if stem = FALSE, do not remove small filler words
    if(stem == FALSE) {
        clean <- tm::Corpus(VectorSource(text_input)) %>%
            tm::tm_map(content_transformer(tolower)) %>% #convert everything to lower
            tm::tm_map(removeWords, profanity) %>% #remove profanity
            tm::tm_map(removePunctuation)  %>% #remove punctuation
            tm::tm_map(removeNumbers) %>% #remove numbers
            tm::tm_map(stripWhitespace) #strip white space
    } else {
        clean <- tm::Corpus(VectorSource(text_input)) %>%
            tm::tm_map(content_transformer(tolower)) %>%
            tm::tm_map(removeWords, profanity) %>% 
            tm::tm_map(removePunctuation)  %>% 
            tm::tm_map(removeNumbers) %>% 
            tm::tm_map(stripWhitespace) %>% 
            tm_map(removeWords, stopwords("english"))
    }
    
    #check the unique characters now
    dat <- sapply(clean, function(row) iconv(row, "latin1", "ASCII", sub=""))
    cleaned <- Corpus(VectorSource(dat))
    
    #N-gram tokenizer
    corp <- corpus(cleaned)
    tok <- tokens(corp)
    return(tok)
}


#input for model
# input <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
input <- "Go on a romantic date at the"

#return tokenized and stemmed version of the sentence/phrase to find keywords
model_tokens <- clean_text(input, stem = TRUE)

#Find keywords
phrase <- model_tokens[[1]]
phrase_df <- NULL
for(i in 1:length(phrase)) {
    target_dfm <- tokens_keep(stem_tokens, phrase[i], window = 10) %>% dfm()
    not_target_dfm <- tokens_remove(stem_tokens, phrase[i], window = 10) %>% dfm()
    target_key <- textstat_keyness(rbind(target_dfm, not_target_dfm), seq_len(ndoc(target_dfm))) %>%
        dplyr::filter(n_target > 10)
    phrase_df <- rbind(phrase_df, target_key[1:50,])
}

phrase_df <- phrase_df %>%
    dplyr::distinct(feature, .keep_all = T)

#look up ngrams with "date at the ___" using keywords
ngram_lookup <- function(ngram_df, input_text) {
    model_df <- ngram_df %>%
        dplyr::filter(grepl(paste0("^", input_text, "_"), word)) %>%
        dplyr::mutate(word = as.character(word)) %>%
        dplyr::mutate(nextword = str_split_fixed(word, paste0(input_text, "_"), 2)[,2])

}
