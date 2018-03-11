library(tidyverse)
library(DT)
library(RCurl)
library(tm)
library(quanteda)
# setwd("~/Dropbox/AndersenLab/LabFolders/Katie/projects/miscellaneous/coursera/datascience_capstone/")
load("~/Downloads/ngram_df.RData")

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
            tm::tm_map(removePunctuation)  %>% #remove punctuation
            tm::tm_map(removeNumbers) %>% #remove numbers
            tm::tm_map(stripWhitespace) #strip white space
    } else {
        clean <- tm::Corpus(VectorSource(text_input)) %>%
            tm::tm_map(content_transformer(tolower)) %>%
            tm::tm_map(removeWords, profanity) %>% 
            tm_map(removeWords, stopwords("english")) %>%
            tm::tm_map(removePunctuation)  %>% 
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
        dplyr::filter(grepl(paste0("^", text, "_"), word)) %>%
        dplyr::mutate(word = as.character(word)) %>%
        dplyr::mutate(nextword = str_split_fixed(word, paste0(text, "_"), 2)[,2])
    return(model_df$nextword)
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
ngram_predict <- function(input_text) {
    #Tokenize input text
    input_tokens <- clean_text(input_text, stem = FALSE, stopwords = FALSE)[[1]]
    
    #Find keywords (remove stop words)
    keywords <- clean_text(input_text, stem = FALSE, stopwords = TRUE)[[1]]
    
    #Find the last three words of the sentence and predict next word using fourgram
    #Return five most likely next words, if not 5 results, go to tri gram etc.
    #If nothing else works, return five most likely unigrams.
    if(length(input_tokens) > 2) {
        text <- input_tokens[(length(input_tokens) - 2):length(input_tokens)]
        words <- ngram_lookup(paste(text, collapse = "_"), four_df)
        if(length(words) < 6) {
            text <- text[-1]
            words <- c(words, ngram_lookup(paste(text, collapse = "_"), tri_df))
            #look up top words associated with each keyword
            # context <- keyword_lookup(keywords)
            if(length(words) < 6) {
                text <- text[-1]
                words <- c(words, ngram_lookup(paste(text, collapse = "_"), bi_df))
                if(length(words) < 6) {
                    words <- c(words, uni_df$word[1:5])
                }
            }
        }
    }
    prediction <- words[1:5]
    return(prediction)
}

Q1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
q <- clean_text(Q1)
Q2 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
q <- clean_text(Q2)

start <- Sys.time()
ngram_predict(Q1)
stop <- Sys.time()
stop - start

#this is too slow, try markov models or hashing to make it faster (and maybe add more data to make it more accurate)
library(text2vec)

#tokenize text
token_train <- itoken(train, preprocessor = tolower, tokenizer = word_tokenizer)

#create vocab without stopwords
stopwords <- quanteda::stopwords("en")
vocab <- create_vocabulary(token_train, stopwords = stopwords)

#prune vocabulary
pruned_vocab <- prune_vocabulary(vocab, term_count_min = 10, 
                                 doc_proportion_max = 0.5, doc_proportion_min = 0.001)
vectorizer <- vocab_vectorizer(pruned_vocab)
# h_vectorizer <-  hash_vectorizer(hash_size = 2 ^ 14, ngram = c(1L, 2L))

#create dtm
t1 <-  Sys.time()
dtm_train <-  create_dtm(token_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))

# N-grams
vocab <- create_vocabulary(token_train, ngram = c(1L, 4L), stopwords = stopwords) %>%
    prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.5)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <-  create_dtm(token_train, vectorizer)

#create term-co-occurance matrix (TCM)
tcm_train <- create_tcm(token_train, vectorizer)
glove <-  GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
wv_main <-  glove$fit_transform(tcm_train, n_iter = 10, convergence_tol = 0.01)
wv_context = glove$components
dim(wv_context)
word_vectors = wv_main + t(wv_context)

berlin = word_vectors["breath", , drop = FALSE] +
    word_vectors["want", , drop = FALSE] +
    word_vectors["air", , drop = FALSE] +
    word_vectors["live", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

#model
t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = dtm_train, y = train[['type']], 
                              family = 'binomial', 
                              alpha = 1,
                              type.measure = "auc",
                              nfolds = NFOLDS,
                              thresh = 1e-3,
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))

