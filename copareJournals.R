library(tm)


## Read data
all_txts <- list.files(pattern = ".txt$")
all_txts

alltxt <- map_df(all_txts, ~ tibble(txt = read_file(.x)) %>%
  mutate(filename = basename(.x)))

bbc <- alltxt[3, 1]
oglobo <- alltxt[7, 1]
uol <- alltxt[11, 1]

# Set the text to lowercase
bbc <- tolower(bbc)
oglobo <- tolower(oglobo)
uol <- tolower(uol)

# Turn into individual sentences
s_vBBC <- get_sentences(bbc)
s_vOGL <- get_sentences(oglobo)
s_vUOL <- get_sentences(uol)


# get sentiments
svBBC <- get_sentiment(s_vBBC, method = "nrc", language = "portuguese")
svOGL <- get_sentiment(s_vOGL, method = "nrc", language = "portuguese")
svUOL <- get_sentiment(s_vUOL, method = "nrc", language = "portuguese")


# create rolling average
rBBC <- round(length(svBBC) * .1)
rolledBBC <- zoo::rollmean(svBBC, k = rBBC)

rOGL <- round(length(svOGL) * .1)
rolledOGL <- zoo::rollmean(svOGL, k = rOGL)

rUOL <- round(length(svUOL) * .1)
rolledUOL <- zoo::rollmean(svUOL, k = rUOL)

# scale
bbcList <- rescale_x_2(rolledBBC)
OGLList <- rescale_x_2(rolledOGL)
UOLList <- rescale_x_2(rolledUOL)


# Compare sample
bbcSample <- seq(1, length(bbcList$x), length.out = 100)
oglSample <- seq(1, length(OGLList$x), length.out = 100)
uolSample <- seq(1, length(UOLList$x), length.out = 100)

plot(bbcList$x[bbcSample],
  bbcList$z[bbcSample],
  type = "l",
  col = "blue",
  xlab = "Time (sampled)",
  ylab = "Sentiment"
)
lines(OGLList$x[oglSample], OGLList$z[oglSample], col = "red")
lines(UOLList$x[uolSample], UOLList$z[uolSample], col = "black")
legend(0, 1, 
       legend=c("BBC", "oglobo", "uol"),
       col=c("blue", "red", "black"), lty = 1, cex = 0.8,
       text.font = 4, bg = 'lightblue')

# Correlation
c <- cor(cbind(bbcList$z[bbcSample], OGLList$z[oglSample], UOLList$z[uolSample]))
colnames(c) <- rownames(c) <- c("BBC", "oglobo", "uol")
corrplot::corrplot.mixed(c)




