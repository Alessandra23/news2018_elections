library(tidytext)
library(tidyverse)
library(syuzhet)
library(zoo)


## Read data
all_txts <- list.files(pattern = ".txt$")
all_txts

alltxt <- map_df(all_txts, ~ tibble(txt = read_file(.x)) %>%
  mutate(filename = basename(.x)))

bbc <- alltxt[3, 1]
oglobo <- alltxt[7, 1]
uol <- alltxt[11, 1]

all_txt <- alltxt[c(3, 7, 11), 1]

# Set the text to lowercase
text <- tolower(all_txt)

# Remove mentions, urls, emojis, numbers, punctuations, etc.
text <- gsub("@\\w+", "", text)
text <- gsub("https?://.+", "", text)
text <- gsub("\\d+\\w*\\d*", "", text)
text <- gsub("#\\w+", "", text)

# Turn into individual sentences
s_v <- get_sentences(text)

# Get sentiment of each sentence
s_v_sentiment <- get_sentiment(s_v, method = "nrc", language = "portuguese")

# plot the sentiment of each sentence over time
time <- c(1:length(s_v_sentiment))
ggplot(mapping = aes(x = time, y = s_v_sentiment)) + 
  geom_line(col = "grey", alpha = 0.5) +
  geom_line(aes(y = rollmean(x = s_v_sentiment, k = 20, na.pad = T), color = "")) +
  theme_minimal() +
  xlab("Time") +
  ylab("Sentiment") +
  scale_color_manual(values = c('Average' = 'grey43')) +
  labs(color = '')
 


# Dividing text into 100 chunks and looking at Sentiment of chunks over time
percent_vals <- get_percentage_values(s_v_sentiment, bins = 100)
percentTime <- c(1:length(percent_vals))

ggplot(mapping = aes(x = percentTime, y = percent_vals)) + 
  geom_line(col = "grey", alpha = 0.5) +
  geom_line(aes(y = rollmean(x = percent_vals, k = 20, na.pad = T), color = "")) +
  theme_minimal() +
  xlab("Time") +
  ylab("Sentiment") +
  scale_color_manual(values = c('Average' = 'grey43')) +
  labs(color = '')


# Using Fourier transform
ft_values <- get_transformed_values(
  s_v_sentiment, 
  low_pass_size = 5, 
  x_reverse_len = 200,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = F
)  

ftTime <- c(1:length(ft_values))

ggplot(mapping = aes(x = ftTime, y = ft_values)) + 
  geom_line(col = "black", alpha = 1) +
  theme_minimal() +
  xlab("Time") +
  ylab("Sentiment") 


dct_values <- get_dct_transform(
  s_v_sentiment, 
  low_pass_size = 5, 
  x_reverse_len = 200,
  scale_vals = F,
  scale_range = T
)

# using cosine tranform instead
dctTime <- c(1:length(dct_values))

ggplot(mapping = aes(x = dctTime, y = dct_values)) + 
  geom_line(col = "black", alpha = 1) +
  theme_minimal() +
  xlab("Time") +
  ylab("Sentiment") 

# combining into one plot
simple_plot(s_v_sentiment)







