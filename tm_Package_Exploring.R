# Author: Shayan (Sean) Taheri
# Exploring the tm Package

library(tm)

# Find the sample corpus
txt <- system.file("texts", "txt", package = "tm")

# Load the corpus
(ovid <- Corpus(DirSource(txt, encoding = "UTF-8"),
                readerControl = list(reader = readPlain,
                                     language = "lat",
                                     load = TRUE)))
# Examine metadata
meta(ovid[[1]])

meta(ovid[[1]])$author <- "Publius Ovidius Naso"
meta(ovid[[1]])

ovid[[1]][2]

# Examine the first document's text
ovid[[1]][1]

# Concatenate several text collections to a single one
c(ovid[1:2], ovid[3:4])

# Show the number of documents in the corpus
length(ovid)

# Show detailed summary of the text document collection
summary(ovid)

# Show predefined transformations
# Can be applied to a corupus with tm_map

getTransformations()

# Remove punctuation
ovid <- tm_map(ovid, FUN = removePunctuation)
ovid[[1]][1]

# Remove numbers
ovid <- tm_map(ovid, FUN = removeNumbers)
ovid[[1]][1]

# Change to all lower case
ovid <- tm_map(ovid, FUN = content_transformer(tolower))
ovid[[1]][1]

# Stem the corpus
# ovid <- tm_map(ovid, FUN = stemDocument)
# function is not available for language 'la'

# Remove words
axe_words <- c("mater", "seu", "annis", "")
ovid <- tm_map(ovid, FUN = removeWords, axe_words)
ovid[[1]][1]

# Remove whitespace
ovid <- tm_map(ovid, FUN = stripWhitespace)
ovid[[1]][1]

# Example with Reuters-21578
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),
                   readerControl = list(reader = readReut21578XMLasPlain))
reuters[[1]][1]

# Eliminate Extra Whitespace
reuters <- tm_map(reuters, stripWhitespace)

# Convert to Lower Case
reuters <- tm_map(reuters, content_transformer(tolower))
# Stemming
# tm_map(reuters, stemDocument)

# Remove Stop Words
reuters <- tm_map(reuters, removeWords, stopwords("english"))
reuters[[1]][1]

# Creating Document-Term Matrices
dtm <- DocumentTermMatrix(reuters)
inspect(dtm[5:10, 740:743])

# Operations on Document-Term Matrices
findFreqTerms(dtm, 5)

# Find Associations Between Words
# Find words associated with opec, with at least 0.8 correlation

findAssocs(dtm, "opec", 0.8)

# Remove Sparse Terms
inspect(removeSparseTerms(dtm, 0.4))

# Dictionary: Terms to Text Mine
inspect(DocumentTermMatrix(reuters,
                           list(dictionary = c("prices", "crude", "oil"))))

# Session info
sessionInfo()