install.packages("twitteR")
install.packages("tm")
install.packages("topicmodels")
install.packages("sentiment140")
install.packages("igraph")

install_github("streamR", "pablobarbera", subdir="streamR")
install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
install_github("smappR", "SMAPPNYU")


accounts <- read.csv("twitter/accounts.csv", stringsAsFactors = F)
accounts <- accounts$Twitter[!is.na(accounts$Twitter)]

## Option 1: retrieve tweets from Twitter

library(twitteR)
library(ROAuth)

## Twitter authentication
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- userTimeline(accounts[1], n = 3200, includeRts=TRUE, excludeReplies=FALSE)
tweets <- twListToDF(tweets)


for(a in 2:length(accounts)){
  res <- userTimeline(accounts[a], n = 3200, includeRts=TRUE, excludeReplies=FALSE)
  res <- twListToDF(res)
  print(paste0("Completed ",accounts[a]," with ",nrow(res)," new tweets at ",Sys.time()))
  tweets <- rbind(tweets,res)
  rm(res)
}

saveRDS(tweets,paste0("twitter/Tweets-",Sys.Date(),".Rds"))


tweets_old <- tweets




## 3200 is the maximum to retrieve


UKGBC <- 
UKGBC <- twListToDF(UKGBC)

BRE_Group <- userTimeline("BRE_Group", n = 3200)
BRE_Group <- twListToDF(BRE_Group)



search <- searchTwitter("@UKGBC", since='2017-01-01', until='2017-07-01')
search <- twListToDF(search)



library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "b8us95Uh9vl9KaNJJnKVK3O7v"
consumerSecret <- "tlSrqprqww9tLIGw7TqjwztVlOHqNeX6rSzaVw0wCN4MKfYcwv"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, 
                             requestURL=requestURL, accessURL=accessURL, authURL=authURL)


my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))



registerTwitterOAuth(pin)
