library(twitteR)
library(ROAuth)
library(igraph)

## Twitter authentication
source("twitter/secrets.R")
source("twitter/functions.R")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
rm(consumer_key, consumer_secret, access_token, access_secret)


#Start with core list of intresting accounts
acc.core <- read.csv("twitter/accounts_cores.csv")
acc.core <- as.character(acc.core$Twitter)
acc.core <- acc.core[acc.core != ""]

#Get some basic stats on these accounts
acc.summary <- getUser(acc.core[1])
acc.summary <- acc.summary$toDataFrame()

#Loop THough all the accounts
for(a in 2:length(acc.core)){
  res <- getUser(acc.core[a])
  res <- res$toDataFrame()
  acc.summary <- rbind(acc.summary,res)
  message(paste0("Done ",acc.core[a]))
  rm(res)
}

#Save Resutls
saveRDS(acc.summary,"twitter/data/coreAccounts.Rds")


#Get the friends of the core accounts
#accounts withe large numbers of followers exapnd the network too much
acc.sub.friend <- acc.summary[acc.summary$friendsCount < 10000, ] #Filter out the really large accounts
friends <- get.friends(acc.sub.friend$screenName[1])

for(b in 2:nrow(acc.sub.friend)){
  res <- get.friends(acc.sub.friend$screenName[b])
  if(!is.null(res)){ #for cases when protected accounts result in a null result
    friends <- rbind(friends,res)
    saveRDS(friends,paste0("twitter/data/core-friends-livedump-",Sys.Date(),".Rds"))
    print(paste0("Total number of records ",nrow(friends)))
  }
  rm(res)
}

message(paste0("Got ",nrow(friends)," from ",nrow(acc.sub.friend),"/",nrow(acc.summary)," accounts"))
saveRDS(friends,paste0("twitter/data/core-friends-",Sys.Date(),".Rds"))

#get followers for accounts with a small number of followers
#accounts withe large numbers of followers exapnd the network too much
acc.sub.follow <- acc.summary[acc.summary$followersCount < 10000, ] #Filter out the really large accounts

followers <- get.followers(acc.sub.follow$screenName[1])

for(b in 2:nrow(acc.sub.follow)){
  res <- get.followers(acc.sub.follow$screenName[b])
  if(!is.null(res)){ #for cases when protected accounts result in a null result
    followers <- rbind(followers,res)
    saveRDS(followers,paste0("twitter/data/core-followers-livedump-",Sys.Date(),".Rds"))
    print(paste0("Total number of records ",nrow(followers)))
  }
  rm(res)
}

message(paste0("Got ",nrow(followers)," from ",nrow(acc.sub.follow),"/",nrow(acc.summary)," accounts"))
saveRDS(followers,paste0("twitter/data/core-followers-",Sys.Date(),".Rds"))

conn.all <- rbind(friends,followers)
saveRDS(conn.all,paste0("twitter/data/core-friend+followers-",Sys.Date(),".Rds"))

#Combine Friends and Followers to get a master list of connections
#Find unique accounts in the list


acc.all <- acc.summary
acc.all$friendof <- NA #needed to match followers frormat
acc.all$followerof <- NA 

#bind togther
acc.all <- rbind(acc.all,conn.all)

#remove duplicates
acc.all <- acc.all[!duplicated(acc.all$screenName),]
acc.all$friendof <- NULL #remove as meaningless
acc.all$followerof <- NULL #remove as meaningless

nrow(acc.all) #Number of accounts in the broard network

saveRDS(acc.all,"twitter/data/broadAccounts.Rds")


