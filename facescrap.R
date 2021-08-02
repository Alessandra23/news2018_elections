## Scrap facebook


library(devtools)
library('Rfacebook')

fb_oauth = "EAADs08l2dUMBAGoZCnnIX8tcZBiY16hU1kVHKU9BOERHlHuikxvGkKNLy1VLQoZArS3yGa37aMnm18TuyhW28w9uZBasH96vfWPvH7C9J6dJzRpiUesn1HHWF3OjI6u8s8doPsRtQ05Ga11SiDVvENWCsqyAJsFOciFPt1zDFpqJTNLPBV9mOHTYaZAGiukcWInt2ZBhVbWuROrpgsyu0ljaQqUlKoBBBDxYl5zmFDYQZDZD"
save(fb_oauth, file="fb_oauth")
load("fb_oauth")

page <- getPage("2423510210996790", token=fb_oauth, n=10, feed = T,  reactions=TRUE) 

page <- getCommentReplies("2423510210996790", token=fb_oauth, n=10) 



test <- fbOAuth(app_id = "260394362238275", app_secret = "bfdb8afed54e693d8804b9ec55de4d63")
save(test, file = "test")
load(test)


users <- getUsers("287321157949050", token = "test")
fri <- getFriends(token = "test",  simplify = T)


token <- 'EAADs08l2dUMBAGoZCnnIX8tcZBiY16hU1kVHKU9BOERHlHuikxvGkKNLy1VLQoZArS3yGa37aMnm18TuyhW28w9uZBasH96vfWPvH7C9J6dJzRpiUesn1HHWF3OjI6u8s8doPsRtQ05Ga11SiDVvENWCsqyAJsFOciFPt1zDFpqJTNLPBV9mOHTYaZAGiukcWInt2ZBhVbWuROrpgsyu0ljaQqUlKoBBBDxYl5zmFDYQZDZD'
me <- getUsers('287321157949050', token = test)

install.packages('rvest')
library('rvest')

#Specifying the url for desired website to be scraped
url <- 'https://www.facebook.com/correio24horas/posts/2423510210996790?comment_tracking=%7B%22tn%22%3A%22O%22%7D#'
#Reading the HTML code from the website
webpage <- read_html(url)

rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)
