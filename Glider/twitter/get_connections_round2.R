library(twitteR)
library(ROAuth)
library(igraph)

## Twitter authentication
source("twitter/secrets.R")
source("twitter/functions.R")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
rm(consumer_key, consumer_secret, access_token, access_secret)


acc.summary <- readRDS("twitter/data/broadAccounts-trim.Rds")
acc.summary <- acc.summary[!duplicated(acc.summary$screenName),]

#Get the friends of the core accounts
#accounts withe large numbers of followers exapnd the network too much
acc.sub.friend <- acc.summary[acc.summary$friendsCount < 10000, ] #Filter out the really large accounts
friends <- get.friends(acc.sub.friend$screenName[1])

for(b in 2:nrow(acc.sub.friend)){
  res <- get.friends(acc.sub.friend$screenName[b])
  if(!is.null(res)){ #for cases when protected accounts result in a null result
    friends <- rbind(friends,res)
    saveRDS(friends,paste0("twitter/data/broad-friends-livedump-",Sys.Date(),".Rds"))
    print(paste0("Total number of records ",nrow(friends)))
  }
  rm(res)
}

message(paste0("Got ",nrow(friends)," from ",nrow(acc.sub.friend),"/",nrow(acc.summary)," accounts"))
saveRDS(friends,paste0("twitter/data/broad-friends-",Sys.Date(),".Rds"))

#

