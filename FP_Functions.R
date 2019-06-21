# Author: Shayan (Sean) Taheri
# Final Project Main Functions

library(dplyr)
library(tm)
library(stylo)

# Reading Necessary Data Files
bigram <- readRDS("C:/Users/shaya/Desktop/Data_Science_Capstone/App_Data/bigram.RData")
trigram <- readRDS("C:/Users/shaya/Desktop/Data_Science_Capstone/App_Data/trigram.RData")
quadgram <- readRDS("C:/Users/shaya/Desktop/Data_Science_Capstone/App_Data/quadgram.RData")
pentgram <- readRDS("C:/Users/shaya/Desktop/Data_Science_Capstone/App_Data/pentgram.RData")
hexgram <- readRDS("C:/Users/shaya/Desktop/Data_Science_Capstone/App_Data/hexgram.RData")

# The Function for Cleaning Text Input
cleantext <- function(text){
        text %>% 
                VectorSource() %>%
                VCorpus() %>%
                tm_map(content_transformer(tolower)) %>%
                tm_map(removePunctuation) %>%
                tm_map(removeNumbers) %>%
                tm_map(removeWords, stopwords("en")) %>%
                tm_map(stripWhitespace)
}

# The Prediction Functions
hexpred <- function(text){
        hexgram[hexgram$first == text[1] &
                hexgram$second == text[2] &
                hexgram$third == text[3] &
                hexgram$fourth == text[4] &
                hexgram$fifth == text[5],8][1,1]
}
pentpred <- function(text){
        pentgram[pentgram$first == text[1] &
                 pentgram$second == text[2] &
                 pentgram$third == text[3] &
                 pentgram$fourth == text[4],7][1,1]
}
quadpred <- function(text){
        quadgram[quadgram$first == text[1] &
                 quadgram$second == text[2] &
                 quadgram$third == text[3],6][1,1]
}
tripred <- function(text){
        trigram[trigram$first == text[1] &
                trigram$second == text[2],5][1,1]
}
bipred <- function(text){
        bigram[bigram$first == text[1],4][1,1]
}

# The Function for Next Word Prediction
nextword <- function(input){
  
        # Clean the Input Text and Initialize Prediction as N/A
        text <- cleantext(input)[[1]][1] %>%
                as.character() %>%
                txt.to.words()
        prediction <- NA
        
        # Call Appropriate Prediction Function Based On Input Length
        while(is.na(prediction)){
                if(length(text) > 5){
                        text <- tail(text, 5)
                        cleaninput <- paste(text, collapse = " ")
                }else if(length(text) == 5){
                        prediction <- hexpred(text)
                }else if(length(text) == 4){
                        prediction <- pentpred(text)
                }else if(length(text) == 3){
                        prediction <- quadpred(text)
                }else if(length(text) == 2){
                        prediction <- tripred(text)
                }else if(length(text) == 1){
                        prediction <- bipred(text)
                }else if(length(text) <= 0){
                        stop(return("ERROR: No match found!"))
                }
          
                # If no word is found, shorten the input, and try again!
                if(is.na(prediction)){
                        text <- tail(text, length(text) - 1)
                        cleaninput <- paste(text, collapse = " ")
                }
        }
        return(as.character(prediction))
}
