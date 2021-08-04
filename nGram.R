library(dplyr)
library(tidytext)
library(tm)



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
  "skui2tu7ts", "19", "curtir", "responder", "pra", "pas", "aqui" , "ainda", 
  "vou", "voc", "tcurtir", "vai", "ser", "silva", "ter", "tkika", "est", "oliveira",
  "souza", "tmaria", "lima", "lá", "tá", "todos", "porque", "gomes", "agora", "vão",
  "assim", "vcs", "tadriana", "tlucio", "paulo", "santos", "tsou", "aí", "rodrigues"
)
stopWords <- c(stopWords, addWords)

# create list of english and portuguese stop words
my_stop_words <- stop_words %>%
  select(-lexicon) %>%
  bind_rows(data.frame(word = stopWords))


# data manipulation
docs <- Corpus(VectorSource(text))

# get data ready for word cloud
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)
rownames(df) <- NULL





d <- tibble(txt = text)

abc <- d %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)


bigrams_separated <- abc %>%
  separate(ngram, c("word1", "word2"), sep = " ")


bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)


bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")







