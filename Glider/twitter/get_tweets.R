accounts <- read.csv("twitter/accounts.csv", stringsAsFactors = F)
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

user <- getUser(accsum$screenName[1])
friends <- user$getFriends() # who this user follows
followers <- user$getFollowers() # this user's followers

#Get followers
connections <- data.frame(account.name = user$screenName,follower.name = NA, follower.id = names(follower))
connections$follower.name <- sapply(1:length(followers), getnames, list = followers)

#get following
connections2 <- data.frame(account.name = NA,follower.name = user$screenName, account.id = names(friends))
connections2$account.name <- sapply(1:length(friends), getnames, list = friends)

#put togther
connections <- connections[,c("account.name","follower.name")]
connections2 <- connections2[,c("account.name","follower.name")]

rm(user,connections2,connections,followers,friends)

connect.all <- rbind(connections,connections2)

for(d in 2:nrow(accsum)){
  user <- getUser(accsum$screenName[d])
  friends <- user$getFriends() # who this user follows
  followers <- user$getFollowers() # this user's followers
  
  #Get followers
  connections <- data.frame(account.name = user$screenName,follower.name = NA, follower.id = names(followers))
  connections$follower.name <- sapply(1:length(followers), getnames, list = followers)
  
  #get following
  connections2 <- data.frame(account.name = NA,follower.name = user$screenName, account.id = names(friends))
  connections2$account.name <- sapply(1:length(friends), getnames, list = friends)
  
  #put togther
  connections <- connections[,c("account.name","follower.name")]
  connections2 <- connections2[,c("account.name","follower.name")]
  
  connect.all <- rbind(connect.all,connections,connections2)
  print(paste0("Done ",accsum$screenName[d]," with ",nrow(connections)," followers, and ",nrow(connections2)," friends at ",Sys.time()))
  rm(user,friends,followers, connections, connections2)
}

saveRDS(connect.all,paste0("twitter/Connections-",Sys.Date(),".Rds"))
write.csv(connect.all,paste0("twitter/Connections-",Sys.Date(),".csv"))

connect.all <- connect.all[,c("follower.name","account.name")]
test <- connect.all[1:1000,]
graph <- graph_from_data_frame(connect.all, directed = T)
plot(graph)

foo <- degree(graph, mode = "all")
nodes <- unique(connect.all$follower.name)
nodes2 <- unique(connect.all$account.name)
nodes3 <- unique(c(nodes,nodes2))

graph2 <- delete.vertices(graph, which(degree(graph)<6))
plot(graph2)


followers2 <- followers[[1]]$getFollowers() # a follower's followers



test <- getUser("TheBRETrust")


us <- userFactory$new(screenName="test", name="Joe Smith")
us$getScreenName()
us$getName()


