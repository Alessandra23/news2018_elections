library(stopwords)
library(textdata)
library(dplyr)
library(ggplot2)
library(purrr)
library(colorspace)

# search twitter for tweets with hashtag brasil
fn_twitter <- searchTwitter("#nordeste", n = 1000, lang = "pt")

# Convert to data frame
fn_twitter_df <- twListToDF(fn_twitter)

#  break down into single words
tweet_words <- fn_twitter_df %>%
  select(id, text) %>%
  unnest_tokens(word, text)

# plot frequency of words
plotTweet <- tweet_words %>%
  count(word, sort = T) %>%
  slice(1:20)

# set colour palette
pal <- rev(sequential_hcl(palette = "YlGnBu", n = 11))
# to match brasil flag. but doesnt really produce a nice graphic
# pal = c("green", "yellow", "blue")


p <- ggplot(plotTweet, aes(x = reorder(word, n, function(n) n), y = n)) +
  geom_col(aes(fill = n)) +
  scale_fill_gradientn(colors = pal) +
  ggtitle(label = "#brasil tweets") +
  theme_minimal() +
  xlab("words") +
  ylab("Frequency") +
  coord_flip() +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))
p <- p + labs(fill = "Word\nfrequency")
p





# Stop words --------------------------------------------------------------

# Create a list of stop words:

# use portuguese language
stopWords <- stopwords("pt")
# add some more stop words
addWords <- c(
  "brasil", "https", "t.co", "rt", "amp", "to",
  "in", "1", "a", "1.98", "100", "é", "2", "skui2ubiss",
  "skui2tu7ts", "19", "bolsonaro"
)
stopWords <- c(stopWords, addWords)

# create list of english and portuguese stop words
my_stop_words <- stop_words %>%
  select(-lexicon) %>%
  bind_rows(data.frame(word = stopWords))

# remove stop words from data
tweet_words_interesting <- tweet_words %>%
  anti_join(my_stop_words)

# plot
plotTweet <- tweet_words_interesting %>%
  group_by(word) %>%
  tally(sort = TRUE) %>%
  slice(1:25)


p <- ggplot(plotTweet, aes(x = reorder(word, n, function(n) n), y = n)) +
  geom_col(aes(fill = n)) +
  scale_fill_gradientn(
    colors = pal,
    name = "Word\nfrequency",
    guide = guide_colorbar(
      frame.colour = "black",
      frame.linewidth = 1,
      ticks.colour = "black",
      ticks.linewidth = 1,
    )
  ) +
  ggtitle(label = "#brasil tweets") +
  theme_minimal() +
  xlab("words") +
  ylab("Frequency") +
  coord_flip() +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
p



# Sentiment ---------------------------------------------------------------

# analyse the sentiment of the words used
bing_lex <- get_sentiments("nrc")

fn_sentiment <- tweet_words_interesting %>%
  left_join(bing_lex)

sentiments <- fn_sentiment %>%
  filter(!is.na(sentiment)) %>%
  group_by(sentiment) %>%
  summarise(n = n())

# plot sentiment
p <- ggplot(sentiments, aes(x = reorder(sentiment, n, function(n) n), y = n)) +
  geom_col(aes(fill = n)) +
  scale_fill_gradientn(
    colors = pal,
    name = "Sentiment",
    guide = guide_colorbar(
      frame.colour = "black",
      frame.linewidth = 1,
      ticks.colour = "black",
      ticks.linewidth = 1,
    )
  ) +
  ggtitle(label = "Sentiment") +
  theme_minimal() +
  xlab("Sentiment") +
  ylab("Frequency") +
  coord_flip() +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
p
