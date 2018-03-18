library(text2vec)
library(tidyverse)
library(quanteda)

#read in files
twitter <- read_lines("~/Downloads/final/en_US/en_US.twitter.txt")
news <- read_lines("~/Downloads/final/en_US/en_US.news.txt")
blogs <- read_lines("~/Downloads/final/en_US/en_US.blogs.txt")

#function to take a subset of text to analyze
subset_text <- function(file, p) {
    set.seed(9572)
    indx <- sample(c(TRUE,FALSE), length(file), TRUE, prob = c(p, 1-p))
    return(file[indx])
}

#choose random 1% from each text file and combine them to one tokenized training file
twitter_sample <- subset_text(twitter, 0.5)
blogs_sample <- subset_text(blogs, 0.5)
news_sample <- subset_text(news, 0.5)
all_text <- c(twitter_sample, blogs_sample, news_sample)
index <- sample(c(TRUE, FALSE), length(all_text), TRUE, prob = c(0.6, 0.4))
train <- all_text[index]
test <- all_text[index == FALSE]

#tokenize text
token_train <- itoken(train, preprocessor = tolower, tokenizer = word_tokenizer)

#stop words and profanity
profanity <- c(t(read.csv(text = getURL("http://www.bannedwordlist.com/lists/swearWords.csv"), header=F)))
stopwords <- c(quanteda::stopwords("en"), profanity)

#create vocab without stopwords and bad words for UNIGRAM
vocab <- create_vocabulary(token_train, stopwords = profanity) %>%
    prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.5)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <-  create_dtm(token_train, vectorizer)

#create vocab without stopwords and bad words for BIGRAM
vocab2 <- create_vocabulary(token_train, ngram = c(2L, 2L), stopwords = profanity) %>%
    prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.5)
vectorizer2 <- vocab_vectorizer(vocab2)
dtm_train2 <-  create_dtm(token_train, vectorizer2)

#create vocab without stopwords and bad words for TRIGRAM
vocab3 <- create_vocabulary(token_train, ngram = c(3L, 3L), stopwords = profanity) %>%
    prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.5)
vectorizer3 <- vocab_vectorizer(vocab3)
dtm_train3 <-  create_dtm(token_train, vectorizer3)

#create vocab without stopwords and bad words for FOURGRAM
vocab4 <- create_vocabulary(token_train, ngram = c(4L, 4L), stopwords = profanity) %>%
    prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.5)
vectorizer4 <- vocab_vectorizer(vocab4)
dtm_train4 <-  create_dtm(token_train, vectorizer4)

# Find the most common terms
vector2TopDF <- function(v){
    v <- v[order(v, decreasing = T)]
    v <- data.frame(features = names(v),
                    freq = v, row.names = NULL)
    v <- transform(v, features = reorder(features,freq))
    ## Order factor by freq
    v$features <-factor(v$features, levels=v[order(v$freq), "features"])
    return(v)
}
wordsFreq <- colSums(dtm_train)
top1 <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)])

wordsFreq <- colSums(dtm_train2)
top2 <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)])

wordsFreq <- colSums(dtm_train3)
top3 <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)])

wordsFreq <- colSums(dtm_train4)
top4 <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)])

#Clean up the text
clean_text <- function(text_input, stopwords = TRUE, stem = FALSE) {
    profanity=c(t(read.csv(text = getURL("http://www.bannedwordlist.com/lists/swearWords.csv"), header=F)))
    #stem the words if stem = TRUE
    if(stem == TRUE) {
        test <- stemDocument(text_input)
    }
    #if stopwords = FALSE, do not remove small filler words
    if(stopwords == FALSE) {
        clean <- tm::Corpus(VectorSource(text_input)) %>%
            tm::tm_map(content_transformer(tolower)) %>% #convert everything to lower
            tm::tm_map(removeWords, profanity) %>% #remove profanity
            tm::tm_map(removePunctuation, preserve_intra_word_contractions = TRUE)  %>% #remove punctuation
            tm::tm_map(removeNumbers) %>% #remove numbers
            tm::tm_map(stripWhitespace) #strip white space
    } else {
        clean <- tm::Corpus(VectorSource(text_input)) %>%
            tm::tm_map(content_transformer(tolower)) %>%
            tm::tm_map(removeWords, profanity) %>% 
            tm_map(removeWords, stopwords("english")) %>%
            tm::tm_map(removePunctuation, preserve_intra_word_contractions = TRUE)  %>% 
            tm::tm_map(removeNumbers) %>% 
            tm::tm_map(stripWhitespace) 
    }
    
    #check the unique characters now
    dat <- sapply(clean, function(row) iconv(row, "latin1", "ASCII", sub=""))
    cleaned <- Corpus(VectorSource(dat))
    
    #N-gram tokenizer
    corp <- corpus(cleaned)
    tok <- tokens(corp)
    return(tok)
}

# Function to look up words in an ngram token
ngram_lookup <- function(text, ngram_df) {
    model_df <- ngram_df %>%
        dplyr::mutate(features = as.character(features)) %>%
        dplyr::filter(., grepl(paste0("^", text, "_"), features)) %>%
        dplyr::mutate(total = sum(freq)) %>%
        dplyr::mutate(prob = freq / total) %>%
        dplyr::mutate(nextword = str_split_fixed(features, paste0(text, "_"), 2)[,2]) %>%
        dplyr::select(nextword, prob)
    return(model_df)
}

# Function to look up highly associated words with non-stop word keywords in a phrase
keyword_lookup <- function(text) {
    #choose last three keywords
    phrase <- text[(length(text)-2):length(text)]
    phrase_df <- NULL
    for(i in phrase) {
        target_dfm <- tokens_keep(all_tokens, i, window = 10) %>% dfm()
        not_target_dfm <- tokens_remove(all_tokens, i, window = 10) %>% dfm()
        target_key <- textstat_keyness(rbind(target_dfm, not_target_dfm), seq_len(ndoc(target_dfm))) %>%
            dplyr::filter(n_target > 10, nchar(feature) > 3)
        phrase_df <- rbind(phrase_df, target_key[1:50,])
    }
    phrase_df <- phrase_df %>%
        dplyr::arrange(desc(chi2), desc(p), desc(n_target))
    return(unique(phrase_df$feature))
}

# function to predict the next word given a string of text
ngram_predict <- function(input_text, sw = FALSE, num = 5) {
    #Tokenize input text
    input_tokens <- clean_text(input_text, stem = FALSE, stopwords = FALSE)[[1]]
    
    #Find the last three words of the sentence and predict next word using fourgram
    #Return five most likely next words, if not 5 results, go to tri gram etc.
    #If nothing else works, return five most likely unigrams.
    if(length(input_tokens) > 2) {
        text <- input_tokens[(length(input_tokens) - 2):length(input_tokens)]
        words <- ngram_lookup(paste(text, collapse = "_"), top4)
        if(nrow(words) <= num) {
            text <- text[-1]
            words <- rbind(words, ngram_lookup(paste(text, collapse = "_"), top3)) %>%
                dplyr::arrange(desc(prob)) %>%
                dplyr::distinct(nextword, .keep_all = T)
            if(nrow(words) <= num) {
                text <- text[-1]
                words <- rbind(words, ngram_lookup(paste(text, collapse = "_"), top2)) %>%
                    dplyr::arrange(desc(prob)) %>%
                    dplyr::distinct(nextword, .keep_all = T)
                if(nrow(words) <=num) {
                    top1freq <- top1 %>%
                        dplyr::mutate(total = sum(freq),
                                      prob = freq / total)
                    words <- rbind(words, top1freq) %>%
                        dplyr::arrange(desc(prob)) %>%
                        dplyr::distinct(nextword, .keep_all = T)
                }
            }
        }
    }
    if(sw == TRUE) {
        pred <- words %>%
            dplyr::filter(!grepl(paste(testwords, collapse = "|"), nextword))
        prediction <- pred$nextword[1:num]
    } else {
        prediction <- words$nextword[1:num]
    }
    return(prediction)
}

Q1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
Q2 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
Q3 <- "I'd give anything to see arctic monkeys this"
Q4 <- "Talking to your mom has the same effect as a hug and helps reduce your"
Q5 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
Q6 <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
Q7 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
Q8 <- "Every inch of you is perfect from the bottom to the"
Q9 <- "I’m thankful my childhood was filled with imagination and bruises from playing"
Q10 <- "I like how the same people are in almost all of Adam Sandler's"
ngram_predict(Q1, num = 40, sw = TRUE)
