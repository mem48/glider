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
friends <- get.friends(acc.summary$screenName[1])
friends.list <- list()
friends.list[[1]] <- friends 

for(b in 1917:nrow(acc.summary)){
  acc.name <- acc.summary$screenName[b]
  res <- try(get.friends(acc.name), silent = T) #for cases when the account can't be found
  if(class(res) == "try-error"){
    res <- NULL
  }
  if(!is.null(res)){ #for cases when protected accounts result in a null result
    friends.list[[b]] <- res
    if(b %% 10 == 0){ #Save every 10th time, saving takes time for large files so reduced idel
      saveRDS(friends.list,paste0("F:/broad-friends-livedump-",Sys.Date(),".Rds")) #temp dump to SSD
      message(paste0("Saved a copy of the results so far at ",Sys.time()))
    }
  }
  rm(res,acc.name)
}

friends.df <- do.call("rbind",friends.list)
saveRDS(friends.df,paste0("twitter/data/broad-friends-",Sys.Date(),".Rds"))

