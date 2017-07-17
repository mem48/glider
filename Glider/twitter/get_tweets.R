accounts <- read.csv("twitter/accounts.csv", stringsAsFactors = F)
accounts <- accounts$Twitter[!is.na(accounts$Twitter)]
accounts <- accounts[nchar(accounts) > 0]

## Option 1: retrieve tweets from Twitter

library(twitteR)
library(ROAuth)

## Twitter authentication
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- userTimeline(accounts[1], n = 3200, includeRts=TRUE, excludeReplies=FALSE)
tweets <- twListToDF(tweets)


for(a in 2:length(accounts)){
  res <- userTimeline(accounts[a], n = 3200, includeRts=TRUE, excludeReplies=FALSE)
  if(length(res) > 0){
    res <- twListToDF(res)
    tweets <- rbind(tweets,res)
    print(paste0("Completed ",accounts[a]," with ",nrow(res)," new tweets at ",Sys.time()))
  }else{
    print(paste0("No Data for ",accounts[a]))
  }
  rm(res)
}

saveRDS(tweets,paste0("twitter/Tweets-",Sys.Date(),".Rds"))
write.csv(tweets,paste0("twitter/Tweets-",Sys.Date(),".csv"))


test <- getUser("TheBRETrust")


us <- userFactory$new(screenName="test", name="Joe Smith")
us$getScreenName()
us$getName()

follow <- test$getFollowers()
followid <- test$getFollowerIDs()

