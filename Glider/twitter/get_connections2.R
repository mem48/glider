library(twitteR)
library(ROAuth)
library(igraph)

## Twitter authentication
source("twitter/secrets.R")
source("twitter/functions.R")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
rm(consumer_key, consumer_secret, access_token, access_secret)

#Get list of Accounts
accsum <- readRDS("twitter/data/Connections-cleaned-2017-08-07.Rds")
accounts <- unique(accsum$screenName)
rm(accsum)

options(warn = 1) #debug

friends <- get.friends(accounts[1])

for(i in 144:length(accounts)){
  res <- get.friends(accounts[i])
  if(!is.null(res)){ #for cases when protected accounts result in a null result
    friends <- rbind(friends,res)
    saveRDS(friends,paste0("twitter/data/friends-livedump-",Sys.Date(),".Rds"))
    print(paste0("Total number of records ",nrow(friends)))
  }
  rm(res)
}

saveRDS(connect.all,paste0("twitter/data/friends-fin-",Sys.Date(),".Rds"))




