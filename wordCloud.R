#####################
# Create Word Cloud #
#####################

# library -------------------------------------------------------
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)


# Use with news_code.R ----------------------------------------------------

# get words
wcWords <- all_words_interesting %>%
  slice(1:100)

# make plot
set.seed(1234) # for reproducibility
wordcloud(
  words = wcWords$word,
  freq = wcWords$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

# interactive word cloud
wordcloud2(data = wcWords, size = 0.5, color = "random-dark")




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Using TWITTER -----------------------------------------------------------

# Create a vector containing only the text
text <- tweet_words_interesting$word

# Create a corpus
docs <- Corpus(VectorSource(text))


# clean the text from tweets
# gsub("https\\S*", "", tweet_words_interesting$text)
# gsub("@\\S*", "", tweet_words_interesting$text)
# gsub("amp", "", tweet_words_interesting$text)
# gsub("[\r\n]", "", tweet_words_interesting$text)
# gsub("[[:punct:]]", "", tweet_words_interesting$text)

# get data ready for word cloud
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)
rownames(df) <- NULL
head(df)


# for tweets
# tweets_words <-  tweets %>%
#   select(text) %>%
#   unnest_tokens(word, text)
# words <- tweets_words %>% count(word, sort=TRUE)

# plot
set.seed(1234) # for reproducibility
wordcloud(
  words = df$word,
  freq = df$freq,
  min.freq = 1,
  max.words = 1000,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

# create an interactive display (hover mouse over word to see frequency)
wordcloud2(data = df, size = 0.5, color = "random-dark")
wordcloud2(data = df, size = 0.5, shape = "star")
