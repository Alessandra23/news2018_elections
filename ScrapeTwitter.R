library(twitteR)
library(tidytext)


consumerKey <- "bB7Mi2NfAb68L27TXcuqdwtth"
consumerSecret <- "IgoIPX9LGPqD4gDfeVu4w2PUYtREhVX9lfd3VBMijHA7EjuaCJ"
accessToken <- "1362592937213906944-d5BK7cHUFccWDP0LYsvXf94ji1qkZa"
accessSecret <- "vEKWjjjWaydcicL8B77HhU24326AcEQLJJQfpwLZDrUSr"

options(httr_oauth_cache = TRUE)
setup_twitter_oauth(
  consumer_key = consumerKey, consumer_secret = consumerSecret,
  access_token = accessToken, access_secret = accessSecret
)


# using just the account 'Radiohead'
RHtweets <- userTimeline("Radiohead", n = 3200)
RHtweets_df <- tbl_df(map_df(RHtweets, as.data.frame))

# using a hashtag
RHtag <- searchTwitter("#radiohead exclude:retweets", n = 3200)
RHtag_df <- tbl_df(map_df(RHtag, as.data.frame))

# tweets sent to a user
Tweets2RH <- searchTwitter("@radiohead exclude:retweets", n = 3200)
Tweets2RH_df <- tbl_df(map_df(Tweets2RH, as.data.frame))
