# Author: Shayan (Sean) Taheri
# Quiz 1 from Data Science Capstone

# Loading Dataset Files:
blogs_file <- "C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.blogs.txt"
news_file <- "C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.news.txt"
twitter_file <- "C:/Users/shaya/Desktop/Data_Science_Capstone/SwiftKey_Dataset/en_US/en_US.twitter.txt"

# (1) The "en_US.blogs.txt" file is how many megabytes?
print(paste('Q1 Answer is:', toString(file.size(blogs_file)/(2^20)), sep=" "))

# (2) The "en_US.twitter.txt" has how many lines of text?
twitter <- readLines(twitter_file, skipNul = TRUE) 
print(paste('Q2 Answer is:', length(twitter), sep=" "))

# (3) What is the length of the longest line seen in any of the three en_US data sets?
# Blogs Dataset
blog_lines <- nchar(readLines(blogs_file, skipNul = TRUE))
print(paste('Q3.A Answer is:', max(blog_lines), sep=" "))

# News Dataset
news_lines <- nchar(readLines(news_file, skipNul = TRUE))
print(paste('Q3.B Answer is:', max(news_lines), sep=" "))

# Twitter Dataset
twitter_lines <- nchar(readLines(twitter_file, skipNul = TRUE))
print(paste('Q3.C Answer is:', max(twitter_lines), sep=" "))


# (4) In the en_US twitter data set, if you divide the number of lines where
# the word "love" (all lowercase) occurs by the number of lines the word
# "hate" (all lowercase) occurs, about what do you get?
lines_love <- grepl(".love.", readLines(twitter_file, skipNul = TRUE), 
                    ignore.case = FALSE, perl = TRUE)

n_love <- sum(lines_love)

lines_hate <- grepl(".hate.", readLines(twitter_file, skipNul= TRUE), 
                    ignore.case = FALSE, perl = TRUE)

n_hate <- sum(lines_hate)

print(paste('Q4 Answer is:', (love_hate <- n_love/n_hate), sep=" "))

# (5) The one tweet in the en_US twitter data set that matches the word "biostats" says what?
print(paste('Q5 Answer is:', twitter[grep("biostats", twitter)], sep=" "))


# (6) How many tweets have the exact characters "A computer once beat me at chess,
# but it was no match for me at kickboxing" (i.e. the line matches those characters exactly).
print(paste('Q6 Answer is:', sum(grepl( "A computer once beat me at chess, but it was no match for me at kickboxing", twitter)), sep=" "))
