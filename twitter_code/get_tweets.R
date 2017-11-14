accounts <- read.csv("twitter/accounts_cores.csv", stringsAsFactors = F)
accounts <- accounts$Twitter[!is.na(accounts$Twitter)]
accounts <- accounts[nchar(accounts) > 0]

## Option 1: retrieve tweets from Twitter

library(twitteR)
library(ROAuth)
library(igraph)

## Twitter authentication
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
rm(consumer_key, consumer_secret, access_token, access_secret)


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

#Get rate limits
limits <- getCurRateLimitInfo()
limits <- limits[limits$remaining < limits$limit,]



#Summarise accounts
getaccsum <- function(b){
  usr <- getUser(accounts[b])
  usr <- usr$toDataFrame()
  return(usr)
}

accsum <- lapply(1:length(accounts),getaccsum)
accsum <- do.call("rbind", accsum)
accsum <- accsum[order(accsum$followersCount),]


saveRDS(accsum,paste0("twitter/AccountSummary-",Sys.Date(),".Rds"))
write.csv(accsum,paste0("twitter/AccountSummary-",Sys.Date(),".csv"))


#Get Followers

#acc <- getUser(accounts[1])
#follow <- acc$getFollowers()
#followid <- test$getFollowerIDs()

getnames <- function(n, list){
  #Return the name from a structured list
  val <- list[[n]]
  val2 <- val$screenName
  return(val2)
}


write.csv(connect.all,paste0("twitter/Connections-Big-",Sys.Date(),".csv"))


lim <- getCurRateLimitInfo()
user <- getUser(accsum$screenName[d])
lim2 <- getCurRateLimitInfo()

res <- lim[lim$remaining != lim2$remaining,]

