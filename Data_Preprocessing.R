# Author: Shayan (Sean) Taheri
# Final Project Data Preprocessing

library(downloader)
library(plyr);
library(dplyr)
library(knitr)
library(tm)
library(stringi)
library(RWeka)
library(ggplot2)
library(slam)

# Loading Dataset Files
blogs_file <- "C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.blogs.txt"
news_file <- "C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.news.txt"
twitter_file <- "C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.twitter.txt"

# Read the Data Files
blogData   <- read_lines(blogs_file)
newsData    <- read_lines(news_file)
twitterData <- read_lines(twitter_file) 

# Get File Sizes
blogData.size <- file.info("C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.blogs.txt")$size / 1024 ^ 2
newsData.size <- file.info("C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.news.txt")$size / 1024 ^ 2
twitterData.size <- file.info("C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.twitter.txt")$size / 1024 ^ 2

# Get Words in Files
blogData.words <- stri_count_words(blogData)
newsData.words <- stri_count_words(newsData)
twitterData.words <- stri_count_words(twitterData)

data.frame(source = c("blogs", "news", "twitter"),
           file.size.MB <- c(blogData.size, newsData.size, twitterData.size),
           num.lines <- c(length(blogData), length(newsData), length(twitterData)),
           num.words <- c(sum(blogData.words), sum(newsData.words), sum(twitterData.words)),
           mean.num.words <- c(mean(blogData.words), mean(newsData.words), mean(twitterData.words)))

set.seed(5000) 
data.sample <- c(sample(blogData, length(blogData) * 0.999),
                 sample(newsData, length(newsData) * 0.999),
                 sample(twitterData, length(twitterData) * 0.999))

corpus <- VCorpus(VectorSource(data.sample))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)
unicorpus <- tm_map(corpus, removeWords, stopwords("en"))

getFreq <- function(tdm) {
  freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  return(data.frame(word = names(freq), freq = freq))
}

bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quadgram <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
pentagram <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
hexagram <- function(x) NGramTokenizer(x, Weka_control(min = 6, max = 6))

freq1 <- getFreq(removeSparseTerms(TermDocumentMatrix(unicorpus), 0.999))
save(freq1, file="./UnigramData.RData")
freq2 <- getFreq(TermDocumentMatrix(unicorpus, control = list(tokenize = bigram, bounds = list(global = c(5, Inf)))))
save(freq2, file="./BigramData.RData")
freq3 <- getFreq(TermDocumentMatrix(corpus, control = list(tokenize = trigram, bounds = list(global = c(3, Inf)))))
save(freq3, file="./TrigramData.RData")
freq4 <- getFreq(TermDocumentMatrix(corpus, control = list(tokenize = quadgram, bounds = list(global = c(2, Inf)))))
save(freq4, file="./QuadgramData.RData")

freq2$word  <- as.character(freq2$word)
freq2 <- separate(freq2, 1, c("unigram", "bigram"))
rownames(freq2) <- NULL
save(freq2, file="./BigramData.RData")

freq3$word  <- as.character(freq3$word)
freq3 <- separate(freq2, 1, c("unigram", "bigram", "trigram"))
rownames(freq3) <- NULL
save(freq3, file="./TrigramData.RData")

freq4$word  <- as.character(freq4$word)
freq4 <- separate(freq2, 1, c("unigram", "bigram", "quadgram"))
rownames(freq4) <- NULL
save(freq4, file="./QuadgramData.RData")