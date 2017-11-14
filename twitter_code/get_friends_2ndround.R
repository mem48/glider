#Second round get friends

library(twitteR)
library(ROAuth)
library(igraph)

## Twitter authentication
source("twitter/secrets.R")


setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
rm(consumer_key, consumer_secret, access_token, access_secret)


# get the list of accounts
verts <- readRDS("twitter/data/secondLevelAccounts.Rds")
verts <- verts[order(-verts$strength.in),]


#Get the friends of the core accounts
#accounts withe large numbers of followers exapnd the network too much
friends.list <- list()
source("twitter/functions.R")

for(b in 1131:nrow(verts)){
  acc.name <- verts$name[b]
  res <- try(get.friends(acc.name), silent = T) #for cases when the account can't be found
  if(class(res) == "try-error"){
    res <- NULL
  }
  if(!is.null(res)){ #for cases when protected accounts result in a null result
    friends.list[[b]] <- res
    if(b %% 10 == 0){ #Save every 10th time, saving takes time for large files so reduced idel
      saveRDS(friends.list,paste0("F:/secondLevel-friend-livedump-",Sys.Date(),".Rds")) #temp dump to SSD
      message(paste0("Saved a copy of the results so far at ",Sys.time()," loop number = ",b))
    }
  }
  rm(res,acc.name)
}

friends.df <- do.call("rbind",friends.list)
saveRDS(friends.df,paste0("twitter/data/secondLevel-friends-first-1130-users-",Sys.Date(),".Rds"))


#Notes This is getting the favorite from a long list of 13375 accounts which is way too many,
# but it is doing them in order of strength so it is starting with the most important accounts
# idealy it needs to get over 1000 accounts before stopping which sould take 4 days
#so stop on Saturday 19th at night

