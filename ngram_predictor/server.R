library(shiny)
library(tidyverse)
library(text2vec)
library(quanteda)
library(RCurl)
library(tm)

load("vocab_dataframes.Rdata")

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
        dplyr::select(nextword, prob) %>%
        dplyr::filter(!nextword %in% stopwords)
    return(model_df)
}

# function to predict the next word given a string of text
ngram_predict <- function(input_text, num = 5) {
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
                                      prob = freq / total,
                                      nextword = as.character(features)) %>%
                        dplyr::select(nextword, prob) %>%
                        dplyr::filter(!nextword %in% stopwords)
                    words <- rbind(words, top1freq) %>%
                        dplyr::arrange(desc(prob)) %>%
                        dplyr::distinct(nextword, .keep_all = T)
                }
            }
        }
    } else if(length(input_tokens) == 2) {
        text <- input_tokens
        words <- ngram_lookup(paste(text, collapse = "_"), top3) %>%
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
                                  prob = freq / total,
                                  nextword = as.character(features)) %>%
                    dplyr::select(nextword, prob) %>%
                    dplyr::filter(!nextword %in% stopwords)
                words <- rbind(words, top1freq) %>%
                    dplyr::arrange(desc(prob)) %>%
                    dplyr::distinct(nextword, .keep_all = T)
            }
        }
    } else if(length(input_tokens == 1)) {
        text <- input_tokens
        words <- ngram_lookup(paste(text, collapse = "_"), top2) %>%
            dplyr::arrange(desc(prob)) %>%
            dplyr::distinct(nextword, .keep_all = T)
        if(nrow(words) <=num) {
            top1freq <- top1 %>%
                dplyr::mutate(total = sum(freq),
                              prob = freq / total,
                              nextword = as.character(features)) %>%
                dplyr::select(nextword, prob) %>%
                dplyr::filter(!nextword %in% stopwords)
            words <- rbind(words, top1freq) %>%
                dplyr::arrange(desc(prob)) %>%
                dplyr::distinct(nextword, .keep_all = T)
        }
    } else {return("ERROR: Must enter at least one word.")}
    return(words$nextword[1:num])
}


# Define server logic
shinyServer(function(input, output) {
    
    output$about_text <- renderText({
        "This application aims to predict the next word from a user-defined text input. \
        The project is the capstone project for the Coursera Data Science Specialization. \
        The data used to train the model was obtained from the Coursera website and is a corpora \
        collected from publically available sources by a web crawler."
        })
    
    help_list <- c("Enter a word or phrase into the text box on the left-hand side.",
                   "Choose how many word choices to predict as the next word (default is five) by adjusting the slider below the text input box.",
                   "When you are ready to predict the next word, click the 'Predict' button. The options for the next word will appear on the right hand side as a bullet-list ordered from most to least likely.")
    
    output$help_text <- renderUI(tags$div(tags$ul(
        lapply(1:length(help_list), function(x) tags$li(renderText(help_list[x])))
    )))
   
    output$author_text <- renderText({
        "Katie Evans is a PhD candidate at Northwestern University. She combines both computational \
        and experimental techniques to investiage the relationship between natural genetic variation \
        and differential response to chemotherapeutics in the tractable metazoan model \
        Caenorhabditis elegans. In her free time, Katie loves hiking and being outdoors, traveling, \
        and reading."
    })
    
  # Print the user text sentence    
  output$user_input <- renderUI(paste0(input$input, " . . ."))
  
  # Predict next word and print
  observeEvent(input$go, {
      newinput <- input$input
      words <- ngram_predict(newinput, num = input$numwords)
      output$model_output <- renderUI(tags$div(tags$ul(
          lapply(1:length(words), function(x) tags$li(renderText(words[x])))
          )))
  })
  
})
