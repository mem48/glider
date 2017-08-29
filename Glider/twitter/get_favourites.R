#Get favorites

library(twitteR)
library(ROAuth)
library(igraph)

## Twitter authentication
source("twitter/secrets.R")
source("twitter/functions.R")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
rm(consumer_key, consumer_secret, access_token, access_secret)


# get the list of accounts
acc.core <- readRDS("twitter/data/coreAccounts.Rds")

#Trim out the massive accounts
#acc.sub <- acc.core[acc.core$friendsCount < 10000,]
#acc.sub <- acc.sub[acc.sub$followersCount < 50000,]

#Do just the massive accounts
acc.sub <- acc.core[acc.core$friendsCount >= 10000 | acc.core$followersCount >= 50000,]

#Get the friends of the core accounts
#accounts withe large numbers of followers exapnd the network too much
favorites <- get.favorites2(acc.sub$screenName[1])
favorites.list <- list()
favorites.list[[1]] <- favorites 

for(b in 2:nrow(acc.sub)){
  acc.name <- acc.sub$screenName[b]
  res <- try(get.favorites(acc.name), silent = T) #for cases when the account can't be found
  if(class(res) == "try-error"){
    res <- NULL
  }
  if(!is.null(res)){ #for cases when protected accounts result in a null result
    favorites.list[[b]] <- res
    if(b %% 10 == 0){ #Save every 10th time, saving takes time for large files so reduced idel
      saveRDS(favorites.list,paste0("F:/corenTvlarge-favorites-livedump-",Sys.Date(),".Rds")) #temp dump to SSD
      message(paste0("Saved a copy of the results so far at ",Sys.time()))
    }
  }
  rm(res,acc.name)
}

favorites.df <- do.call("rbind",favorites.list)
saveRDS(favorites.df,paste0("twitter/data/corenTvlarge-favorites-",Sys.Date(),".Rds"))
