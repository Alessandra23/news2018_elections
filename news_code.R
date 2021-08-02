library(tidytext)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(colorspace)


## Read data

all_txts <- list.files(pattern = ".txt$")
all_txts

alltxt <- map_df(all_txts, ~ tibble(txt = read_file(.x)) %>%
         mutate(filename = basename(.x)))

bbc <- alltxt[3,1]
oglobo <- alltxt[7,1]
uol <- alltxt[11,1]

all_txt <- alltxt[c(3,7,11), 1]

# Set the text to lowercase
text <- tolower(all_txt)
# Remove mentions, urls, emojis, numbers, punctuations, etc.
text <- gsub("@\\w+", "", text)
text <- gsub("https?://.+", "", text)
text <- gsub("\\d+\\w*\\d*", "", text)
text <- gsub("#\\w+", "", text)
#text <- gsub("[^\x01-\x7F]", "", text)
text <- gsub("[[:punct:]]", " ", text)
# Remove spaces and newlines
text <- gsub("\n", " ", text)
text <- gsub("^\\s+", "", text)
text <- gsub("\\s+$", "", text)
text <- gsub("[ |\t]+", " ", text)


# Stop words --------------------------------------------------------------

# Create a list of stop words:

# use portuguese language
stopWords <- stopwords("pt")
# add some more stop words
addWords <- c(
  "https", "t.co", "rt", "amp", "to",
  "in", "1", "a", "1.98", "100", "é", "2", "skui2ubiss",
  "skui2tu7ts", "19", "curtir", "responder", "pra", "pas", "nao", "aqui" , "ainda", 
  "vou", "voc", "tcurtir", "vai", "ser", "silva", "ter", "tkika", "est", "oliveira",
  "souza", "tmaria", "lima", "lá", "tá", "t"
)
stopWords <- c(stopWords, addWords)

# create list of english and portuguese stop words
my_stop_words <- stop_words %>%
  select(-lexicon) %>%
  bind_rows(data.frame(word = stopWords))


# Analysis

docs <- Corpus(VectorSource(text))

# get data ready for word cloud
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)
rownames(df) <- NULL
head(df)


# remove stop words from data
all_words_interesting <- df %>%
  anti_join(my_stop_words)

all_words_interesting %>% head()


# plot
# plotall <- all_words_interesting %>%
#   group_by(word) %>%
#   tally(sort = TRUE) %>%
#   slice(1:25)


plotall <- all_words_interesting %>%
  slice(1:25)

pal <- rev(sequential_hcl(palette = "YlGnBu", n = 11))
p <- ggplot(plotall, aes(x = reorder(word, freq, function(n) n), y = freq)) +
  geom_col(aes(fill = freq)) +
  scale_fill_gradientn(
    colors = pal,
    name = "Frequência\nda palavra",
    guide = guide_colorbar(
      frame.colour = "black",
      frame.linewidth = 1,
      ticks.colour = "black",
      ticks.linewidth = 1,
    )
  ) +
  ggtitle(label = "Notícias sobre as eleições 2018") +
  theme_minimal() +
  xlab("Palavras") +
  ylab("Frequência") +
  coord_flip() +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
p




# Sentiment ---------------------------------------------------------------

# analyse the sentiment of the words used
bing_lex <- get_sentiments("nrc")

fn_sentiment <- all_words_interesting %>%
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
    name = "Sentimento",
    guide = guide_colorbar(
      frame.colour = "black",
      frame.linewidth = 1,
      ticks.colour = "black",
      ticks.linewidth = 1,
    )
  ) +
  ggtitle(label = "Sentimento") +
  theme_minimal() +
  xlab("Sentimento") +
  ylab("Frequência") +
  coord_flip() +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5))
p

