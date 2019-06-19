# Author: Shayan (Sean) Taheri
# Quiz 2 from Data Science Capstone

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
library(stringi)
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

allBlogs   <- read_lines(blogs_file)
allNews    <- read_lines(news_file)
allTwitter <- read_lines(twitter_file) 

tokenmaker <- function(x) {
  corpus <- Corpus(VectorSource(x))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, stemDocument)
  corpus <- Corpus(VectorSource(corpus))
}  

wordcounter <- function(x) {
  dtm<-DocumentTermMatrix(x)
  dtm_matrix <- as.matrix(dtm)
  word_freq <- colSums(dtm_matrix)
  word_freq <- sort(word_freq, decreasing = TRUE)
  words <- names(word_freq)
  return(list(words, word_freq))
}  

NextWordIs <- function(x,y){
  BQuest<-grepl(x, allBlogs, ignore.case=TRUE)
  BDocs<-allBlogs[BQuest]
  textoachado<-'a'
  NextWordIs<-'a'
  i<-length(BDocs)
  if (i>0)
  {
    for (i in 1:i)
    {  textoachado[i]<- str_extract(BDocs[i], y)
    NextWordIs[i]<- stri_extract_last_words(textoachado[i]) 
    }
  }
  NQuest<-grepl(x, allNews, ignore.case=TRUE)
  NDocs<-allNews[NQuest]
  j=length(NDocs)
  if (j>0)
  {
    for (j in 1:j)
    {  textoachado[i+j]<- str_extract(NDocs[j], y)
    NextWordIs[i+j]<- stri_extract_last_words(textoachado[i+j]) 
    }
  }
  TQuest<-grepl(x, allTwitter, ignore.case=TRUE)
  TDocs<-allTwitter[TQuest]
  k=length(TDocs)
  if (k>0)
  {
    for (k in 1:k)
    {  textoachado[i+j+k]<- str_extract(TDocs[k], y)
    NextWordIs[i+j+k]<- stri_extract_last_words(textoachado[i+j+k]) 
    }
  }
  bundle<-as.data.frame(NextWordIs, stringsAsFactors=FALSE)
  summary (bundle)
  blogs_token <- tokenmaker(bundle)
  blogs_words <- wordcounter(blogs_token)
  summary(nchar(bundle))
  head(bundle)
  tdm_Blogs<-TermDocumentMatrix(blogs_token)
  m_Blogs<-as.matrix(tdm_Blogs)
  v_Blogs<-sort(rowSums(m_Blogs),decreasing=TRUE)
  d_Blogs<-data.frame(word=names(v_Blogs),freq=v_Blogs)
  head(v_Blogs, 100)    
  return(list(head(v_Blogs,100)))
}

# ***************** Quiz 2 *****************

# Q2. (1) The guy in front of me just bought a pound of bacon, a bouquet, and a case of (cheese, pretzels, beer, soda)
NextWordIs("case of ", "([Cc]ase+ +[Oo]f+ +[^ ]+ )")

# Q2. (2) You're the reason why I smile everyday. Can you follow me please? It would mean the (universe, most, world, best)
NextWordIs("mean the ", "([Mm]ean+ +[Tt]he+ +[^ ]+ )")

# Q2. (3) Hey sunshine, can you follow me and make me the (happiest, bluest, saddest, smelliest)
NextWordIs("me the ", "([Mm]e+ +[Tt]he+ +[^ ]+ )")

# Q2. (4) Very early observations on the Bills game: Offense still struggling but the (referees, players, crowd, defense)
NextWordIs("but the ", "([Bb]ut+ +[Tt]he+ +[^ ]+ )")

# Q2. (5) Go on a romantic date at the (mall, movies, beach, grocery)
NextWordIs("at the ", "([Aa]t+ +[Tt]he+ +[^ ]+ )") 

# Q2. (6) Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my (horse, phone, way, motorcycle)
NextWordIs("on my ", "([Oo]n+ +[Mm]y+ +[^ ]+ )") 

# Q2. (7) Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some (time, thing, weeks, years)
NextWordIs("quite some ", "([Qq]uite+ +[Ss]ome+ +[^ ]+ )") 

# Q2. (8) After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little (toes, ears, eyes, fingers)
NextWordIs("his little ", "([Hh]is+ +[Ll]ittle+ +[^ ]+ )") 

# Q2. (9) Be grateful for the good times and keep the faith during the (hard, bad, sad, worse)
NextWordIs("during the ", "([Dd]uring+ +[Tt]he+ +[^ ]+ )" )

# Q2. (10) If this isn't the cutest thing you've ever seen, then you must be (insane, asleep, callous, insensitive)
NextWordIs("must be ", "([Mm]ust+ +[Bb]e+ +[^ ]+ )") 

# ***************** Quiz 3 *****************

# Q3. (1) When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd (sleep, give, die, eat)
NextWordIs("and I'd ", "([Aa]nd+ +[Ii]'d+ +[^ ]+ )") 

# Q3. (2) Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his (horticultural, marital, spiritual, financial)
NextWordIs("about his ", "([Aa]bout+ +[hh]is+ +[^ ]+ )")

# Q3. (3) I'd give anything to see arctic monkeys this (weekend, decade, month, morning)
NextWordIs("monkeys this ", "([Mm]onkeys+ +[Tt]his+ +[^ ]+ )")

# Q3. (4) Talking to your mom has the same effect as a hug and helps reduce your (sleepiness, happiness, hunger, stress)
NextWordIs("reduce your ", "([Rr]educe+ +[Yy]our+ +[^ ]+ )")

# Q3. (5) When you were in Holland you were like 1 inch away from me but you hadn't time to take a (walk, minute, look, picture)
NextWordIs("take a ", "([Tt]ake+ +[Aa]+ +[^ ]+ )")

# Q3. (6) I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the (account, case, matter, incident)
NextWordIs("settle the ", "([Ss]ettle+ +[Tt]he+ +[^ ]+ )")

# Q3. (7) I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each (hand, finger, toe, arm)
NextWordIs("in each ", "([Ii]n+ +[Ee]ach+ +[^ ]+ )")

# Q3. (8) Every inch of you is perfect from the bottom to the (top, middle, center, side)
NextWordIs("to the ", "([Tt]o+ +[Tt]he+ +[^ ]+ )")

# Q3. (9) I'm thankful my childhood was filled with imagination and bruises from playing (weekly, inside, outside, daily)
NextWordIs("from playing ", "([Ff]rom+ +[Pp]laying+ +[^ ]+ )")

# Q3. (10) I like how the same people are in almost all of Adam Sandler's (movies, stories, pictures, novels)
NextWordIs("Adam Sandler's ", "([Aa]dam+ +[Ss]andler's+ +[^ ]+ )")
