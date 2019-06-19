# Author: Shayan (Sean) Taheri
# Milestone Project

library(readr)
library(ngram)
library(kableExtra)
library(SnowballC)
library(tm)
library(tmap)
library(corpora)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(wordcloud)
library(stringr)
library(tidytext)
library(frequency)
library(tidyr)
library(slam)
library(textstem)
library(RWeka)

# Loading Dataset Files:

blogs_file <- "C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.blogs.txt"
news_file <- "C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.news.txt"
twitter_file <- "C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.twitter.txt"

# Read the data files

blogData   <- read_lines(blogs_file)
newsData    <- read_lines(news_file)
twitterData <- read_lines(twitter_file) 

# Summary Statistics
docCounts = data.frame(Media=c("Twitter"
                               ,"Blog"
                               ,"News")
                       ,Word_Count=c(sum(stringr::str_count(twitterData, "\\S+"))
                                     ,sum(stringr::str_count(blogData, "\\S+"))
                                     ,sum(stringr::str_count(newsData, "\\S+"))
                       )
)
docCounts$Line_Count = c(length(twitterData)
                         ,length(blogData)
                         ,length(newsData))
docCounts

set.seed(5-6-2019)
twitterData <- twitterData[sample(1:length(twitterData),20000)]
blogData <- blogData[sample(1:length(blogData),20000)]
newsData <- newsData[sample(1:length(newsData),20000)]
corpusTwitter <- VCorpus(VectorSource(twitterData))
corpusBlog <- VCorpus(VectorSource(blogData))
corpusNews <- VCorpus(VectorSource(newsData))
DTMtwitter <- DocumentTermMatrix(corpusTwitter
                                 , control = list(tokenize="words"
                                                  ,removePunctuation = FALSE
                                                  ,stopwords = FALSE
                                                  ,stemming = FALSE
                                                  ,tolower = FALSE
                                 )
)
DTMblog <- DocumentTermMatrix(corpusBlog
                              , control = list(tokenize="words"
                                               ,removePunctuation = FALSE
                                               ,stopwords = FALSE
                                               ,stemming = FALSE
                                               ,tolower = FALSE
                              )
)
DTMnews <- DocumentTermMatrix(corpusNews
                              , control = list(tokenize="words"
                                               ,removePunctuation = FALSE
                                               ,stopwords = FALSE
                                               ,stemming = FALSE
                                               ,tolower = FALSE
                              )
)
twitterCounts <- data.frame(col_sums(DTMtwitter))
twitterCounts$term <- row.names(twitterCounts)
blogCounts <- data.frame(col_sums(DTMblog))
blogCounts$term <- row.names(blogCounts)
newsCounts <- data.frame(col_sums(DTMnews))
newsCounts$term <- row.names(newsCounts)
twitterCounts <- twitterCounts[order(twitterCounts$col_sums.DTMtwitter., decreasing = TRUE),]
blogCounts <- blogCounts[order(blogCounts$col_sums.DTMblog., decreasing = TRUE),]
newsCounts <- newsCounts[order(newsCounts$col_sums.DTMnews., decreasing = TRUE),]
twitterCounts <- twitterCounts[1:30,]
twitterCounts$term <- factor(twitterCounts$term, levels = twitterCounts$term)
blogCounts <- blogCounts[1:30,]
blogCounts$term <- factor(blogCounts$term, levels = blogCounts$term)
newsCounts <- newsCounts[1:30,]
newsCounts$term <- factor(newsCounts$term, levels = newsCounts$term)

print(ggplot(data = twitterCounts, aes(x=term, y=col_sums.DTMtwitter.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Unigram (Word)") + ylab("Frequency in Sample") + ggtitle("Twitter Dataset Unigram Frequency"))
print(ggplot(data = blogCounts, aes(x=term, y=col_sums.DTMblog.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Unigram (Word)") + ylab("Frequency in Sample") + ggtitle("Blog Dataset Unigram Frequency"))
print(ggplot(data = newsCounts, aes(x=term, y=col_sums.DTMnews.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Unigram (Word)") + ylab("Frequency in Sample") + ggtitle("News Dataset Unigram Frequency"))

corpusTwitter <- tm_map(corpusTwitter, stripWhitespace)
corpusBlog <- tm_map(corpusBlog, stripWhitespace)
corpusNews <- tm_map(corpusNews, stripWhitespace)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " "))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " "))
DTMtwitter <- DocumentTermMatrix(corpusTwitter, control=list(tokenize=BigramTokenizer
                                                             ,removePunctuation = FALSE
                                                             ,stopwords = FALSE
                                                             ,stemming = FALSE
                                                             ,tolower = FALSE
)
)
DTMblog <- DocumentTermMatrix(corpusBlog, control=list(tokenize=BigramTokenizer
                                                       ,removePunctuation = FALSE
                                                       ,stopwords = FALSE
                                                       ,stemming = FALSE
                                                       ,tolower = FALSE
)
)
DTMnews <- DocumentTermMatrix(corpusNews, control=list(tokenize=BigramTokenizer
                                                       ,removePunctuation = FALSE
                                                       ,stopwords = FALSE
                                                       ,stemming = FALSE
                                                       ,tolower = FALSE
)
)
twitterCounts <- data.frame(col_sums(DTMtwitter))
twitterCounts$term <- row.names(twitterCounts)
blogCounts <- data.frame(col_sums(DTMblog))
blogCounts$term <- row.names(blogCounts)
newsCounts <- data.frame(col_sums(DTMnews))
newsCounts$term <- row.names(newsCounts)
twitterCounts <- twitterCounts[order(twitterCounts$col_sums.DTMtwitter., decreasing = TRUE),]
blogCounts <- blogCounts[order(blogCounts$col_sums.DTMblog., decreasing = TRUE),]
newsCounts <- newsCounts[order(newsCounts$col_sums.DTMnews., decreasing = TRUE),]

twitterCounts <- twitterCounts[1:30,]
twitterCounts$term <- factor(twitterCounts$term, levels = twitterCounts$term)
blogCounts <- blogCounts[1:30,]
blogCounts$term <- factor(blogCounts$term, levels = blogCounts$term)
newsCounts <- newsCounts[1:30,]
newsCounts$term <- factor(newsCounts$term, levels = newsCounts$term)
print(ggplot(data = twitterCounts, aes(x=term, y=col_sums.DTMtwitter.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Bigram") + ylab("Frequency in Sample") + ggtitle("Twitter Dataset Bigram Frequency"))
print(ggplot(data = blogCounts, aes(x=term, y=col_sums.DTMblog.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Bigram") + ylab("Frequency in Sample") + ggtitle("Blog Dataset Bigram Frequency"))
print(ggplot(data = newsCounts, aes(x=term, y=col_sums.DTMnews.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Bigram") + ylab("Frequency in Sample") + ggtitle("News Dataset Bigram Frequency"))
DTMtwitter <- DocumentTermMatrix(corpusTwitter, control=list(tokenize=TrigramTokenizer
                                                             ,removePunctuation = FALSE
                                                             ,stopwords = FALSE
                                                             ,stemming = FALSE
                                                             ,tolower = FALSE
)
)
DTMblog <- DocumentTermMatrix(corpusBlog, control=list(tokenize=TrigramTokenizer
                                                       ,removePunctuation = FALSE
                                                       ,stopwords = FALSE
                                                       ,stemming = FALSE
                                                       ,tolower = FALSE
)
)
DTMnews <- DocumentTermMatrix(corpusNews, control=list(tokenize=TrigramTokenizer
                                                       ,removePunctuation = FALSE
                                                       ,stopwords = FALSE
                                                       ,stemming = FALSE
                                                       ,tolower = FALSE
)
)
twitterCounts <- data.frame(col_sums(DTMtwitter))
twitterCounts$term <- row.names(twitterCounts)
blogCounts <- data.frame(col_sums(DTMblog))
blogCounts$term <- row.names(blogCounts)
newsCounts <- data.frame(col_sums(DTMnews))
newsCounts$term <- row.names(newsCounts)
twitterCounts <- twitterCounts[order(twitterCounts$col_sums.DTMtwitter., decreasing = TRUE),]
blogCounts <- blogCounts[order(blogCounts$col_sums.DTMblog., decreasing = TRUE),]
newsCounts <- newsCounts[order(newsCounts$col_sums.DTMnews., decreasing = TRUE),]
twitterCounts <- twitterCounts[1:30,]
twitterCounts$term <- factor(twitterCounts$term, levels = twitterCounts$term)
blogCounts <- blogCounts[1:30,]
blogCounts$term <- factor(blogCounts$term, levels = blogCounts$term)
newsCounts <- newsCounts[1:30,]
newsCounts$term <- factor(newsCounts$term, levels = newsCounts$term)
print(ggplot(data = twitterCounts, aes(x=term, y=col_sums.DTMtwitter.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Trigram") + ylab("Frequency in Sample") + ggtitle("Twitter Dataset Trigram Frequency"))
print(ggplot(data = blogCounts, aes(x=term, y=col_sums.DTMblog.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Trigram") + ylab("Frequency in Sample") + ggtitle("Blog Dataset Trigram Frequency"))
print(ggplot(data = newsCounts, aes(x=term, y=col_sums.DTMnews.)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Trigram") + ylab("Frequency in Sample") + ggtitle("News Dataset Trigram Frequency"))
