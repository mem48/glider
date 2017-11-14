library(twitteR)
library(ROAuth)
library(igraph)

## Twitter authentication
source("twitter/secrets.R")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
rm(consumer_key, consumer_secret, access_token, access_secret)

accsum <- read.csv("twitter/AccountSummary-2017-08-03.csv")

options(warn = 1) #debug

getconnections <- function(d){
  #Check if rate limit is allowed
  limit <- getCurRateLimitInfo()
  limit <- limit[limit$resource %in% c("/followers/ids", "/friends/ids"),]
  warning(paste0("followers limit is ",limit$remaining[1]," and friends limit is ",limit$remaining[2]))
  
  #For large twitter accounts mulitple requests required, estimate the number and check if a wait is needed
  user <- getUser(accsum$screenName[d])
  nreq <- round(user$followersCount / 4500,0)
  
  if(nreq >= 15){nreq <- 14} #max value to avoid indefinite waiting
  
  if(limit$remaining[1] <= nreq | limit$remaining[2] <= 1){
    #Reaching Limit so wait
    wait1 = limit$reset[1] - Sys.time()
    wait2 = limit$reset[2] - Sys.time()
    #Get the longer wait
    if(wait2 >= wait1){
      wait <- wait2
    }else{
      wait <- wait1
    }
    wait <- as.integer(as.numeric(wait, units = "secs")) + 1 #Convert to number and add one second for safety
    
    #Check for negative
    if(wait <= 0){
      warning("negative wait time")
      stop()
    }
    
    warning(paste0("Pausing for ",wait," seconds at ",Sys.time()))
    Sys.sleep(wait)
    
  }
  
  #Now get on with code execution
  user <- getUser(accsum$screenName[d])
  user.df <- user$toDataFrame()
  
  friends <- user$getFriends() # who this user follows
  friends.df <- do.call("rbind", lapply(friends, as.data.frame))
  friends.df$friendof <- accsum$screenName[d]
  friends.df$followerof <- NA
  
  followers <- user$getFollowers() # this user's followers
  followers.df <- do.call("rbind", lapply(followers, as.data.frame))
  followers.df$followerof <- accsum$screenName[d]
  followers.df$friendof <- NA
  
  connections <- rbind(friends.df,followers.df)
  
  warning(paste0("Done ",accsum$screenName[d]," with ",nrow(followers.df)," followers, and ",nrow(friends.df)," friends at ",Sys.time()))
  return(connections)
}

connect.all <- getconnections(1)


for(i in 37:nrow(accsum)){
  print(paste0("Doing ",accsum$screenName[i]))
  res <- getconnections(i)
  connect.all <- rbind(connect.all,res)
  rm(res) #added later
  saveRDS(connect.all,paste0("twitter/data/Connections-livedump-",Sys.Date(),".Rds"))
  print(paste0("Total number of records ",nrow(connect.all)))
}

saveRDS(connect.all,paste0("twitter/data/Connections-fin-",Sys.Date(),".Rds"))




