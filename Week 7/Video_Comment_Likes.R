library(qdap)
library(wordcloud)

# Load the data
data <- read.csv("video_comment_likes.csv")

# a) Data Cleaning
cleaned_data <- data[, c("Link", "Title", "Comments", "Likes")]

# b) Remove Stop Words
cleaned_data$Comment <- removeWords(cleaned_data$Comment, stopwords("english"))

# d) Sentiment Analysis
cleaned_data$Sentiment <- polarity(cleaned_data$Comment)$all$polarity

# e) Plot most frequently occurring words
text <- cleaned_data$Comment
corpus <- Corpus(VectorSource(text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
dtm <- DocumentTermMatrix(corpus)
word_counts <- colSums(as.matrix(dtm))
top_words <- head(sort(word_counts, decreasing = TRUE), 10)
wordcloud(names(top_words), top_words)

# f) Plot top liked comments
top_liked <- cleaned_data[order(cleaned_data$Likes, decreasing = TRUE), ][1:5, ]
top_liked$Comment <- substr(top_liked$Comment, 1, 10) # showing just first 10 characters b/c comments are too long
barplot(top_liked$Likes, names.arg = top_liked$Comment, xlab = "Comment (First 10 Characters)", ylab = "Number of Likes", main = "Top 5 Liked Comments")

