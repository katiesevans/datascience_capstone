library(R.utils)

enus_news <- file("~/Documents/Work/coursera/capstone/final/en_US/en_US.news.txt", "r")
#build a sample of text going line by line and reading in 40%
numlines <- countLines("~/Documents/Work/coursera/capstone/final/en_US/en_US.news.txt")[[1]]
samplelines <- rbinom(numlines, 1, 0.01)
for(i in samplelines) {
    if(i == 1) {
        line <- readLines(enus_news,)
    }
}
text <- readLines(enus_news, c(1, 5))
close(enus_news)


#QUIZ
#en_US.twitter.txt has how many lines of text?
setwd("~/Documents/Work/coursera/capstone/final/en_US/")
con <- file("en_US.twitter.txt", "r")
NROW(readLines(con))
close(con)

#what is the length of the longest line in any en_US file?
con <- file("en_US.twitter.txt", "r")
len <- nchar(readLines(con))
max(len)
close(con)

con <- file("en_US.blogs.txt", "r")
len <- nchar(readLines(con))
max(len)
close(con)

con <- file("en_US.news.txt", "r")
len <- nchar(readLines(con))
max(len)
close(con)

#in the twitter dataset: divide number of times a line says "love" by the number of times "hate"
con <- file("en_US.twitter.txt", "r")
loves <- grepl("love", readLines(con))
close(con)
sum(loves)

con <- file("en_US.twitter.txt", "r")
hates <- grepl("hate", readLines(con))
close(con)
sum(loves)/sum(hates)


#what does the tweet that says "biostats" say?
con <- file("en_US.twitter.txt", "r")
biostats <- grep(pattern = "biostats", x = readLines(con), value = TRUE)
close(con)
biostats

match <- "A computer once beat me at chess, but it was no match for me at kickboxing"
con <- file("en_US.twitter.txt", "r")
num <- grepl(match, readLines(con))
sum(num)
