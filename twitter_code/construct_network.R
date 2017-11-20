# Construct a network from the paralleised data

library(igraph)
library(dplyr)


# read in data 

# accounts
accounts1 <- readRDS("F:/Twitter/SecondRoundParReRun/AccountsList-2017-11-08-bs-1.Rds")
accounts2 <- readRDS("F:/Twitter/SecondRoundParReRun/AccountsList-2017-11-13-bs-60.Rds")
accounts3 <- readRDS("F:/Twitter/SecondRoundParReRun/AccountsList-2017-11-20-bs-176.Rds")
accounts1 <- bind_rows(accounts1)
accounts2 <- bind_rows(accounts2)
accounts3 <- bind_rows(accounts3)
accounts.second <- bind_rows(accounts1,accounts2,accounts3)
rm(accounts1,accounts2,accounts3)

# tweets
tweets1 <- readRDS("F:/Twitter/SecondRoundParReRun/TweetsList-2017-11-08-bs-1.Rds")
tweets2 <- readRDS("F:/Twitter/SecondRoundParReRun/TweetsList-2017-11-13-bs-60.Rds")
tweets3 <- readRDS("F:/Twitter/SecondRoundParReRun/TweetsList-2017-11-20-bs-176.Rds")
tweets1 <- bind_rows(tweets1)
tweets2 <- bind_rows(tweets2)
tweets3 <- bind_rows(tweets3)
tweets.second <- bind_rows(tweets1,tweets2,tweets3)
rm(tweets1,tweets2,tweets3)

# likes (favorites)

likes1 <- readRDS("F:/Twitter/SecondRoundParReRun/FavoritesList-2017-11-08-bs-1.Rds")
likes2 <- readRDS("F:/Twitter/SecondRoundParReRun/FavoritesList-2017-11-13-bs-60.Rds")
likes3 <- readRDS("F:/Twitter/SecondRoundParReRun/FavoritesList-2017-11-20-bs-176.Rds")
likes1 <- bind_rows(likes1)
likes2 <- bind_rows(likes2)
likes3 <- bind_rows(likes3)
likes.second <- bind_rows(likes1,likes2,likes3)
rm(likes1,likes2,likes3)

# friends 

friends1 <- readRDS("F:/Twitter/SecondRoundParReRun/FriendsList-2017-11-08-bs-1.Rds")
friends2 <- readRDS("F:/Twitter/SecondRoundParReRun/FriendsList-2017-11-13-bs-60.Rds")
friends3 <- readRDS("F:/Twitter/SecondRoundParReRun/FriendsList-2017-11-20-bs-176.Rds")
friends1 <- bind_rows(friends1)
friends2 <- bind_rows(friends2)
friends3 <- bind_rows(friends3)
friends.second <- bind_rows(friends1,friends2,friends3)
rm(friends1,friends2,friends3)

# read in the first round too

firstround <- readRDS("../twitter_data/parRerun/FirstRoundRawData.Rds") 
accounts.first <- firstround[[1]]
friends.first <- firstround[[2]]
likes.first <- firstround[[3]]
tweets.first <- firstround[[4]]
rm(firstround)

accounts <- bind_rows(accounts.first,accounts.second)
friends <- bind_rows(friends.first,friends.second)
likes <- bind_rows(likes.first,likes.second)
tweets <- bind_rows(tweets.first,tweets.second)

rm(accounts.first, likes.first, friends.first, tweets.first, accounts.second, friends.second, likes.second, tweets.second )

# trim tweets to just retweets
tweets <- tweets[tweets$isRetweet,]

# Get retweet names from the text of the tweet
locs <- gregexpr(": ",tweets$text)
locs <- sapply(locs, function (x) x[1])
tweets$retweetof <- substr(tweets$text,5,locs - 1)
summary(is.na(tweets$retweetof))
rm(locs)


### CHange to weight based on keywords ?




# construct Connections Table
conn.fav <- likes[,c("favOf","screenName")]
names(conn.fav) <- c("from","to")
conn.retweets <- tweets[,c("screenName","retweetof")]
names(conn.retweets) <- c("from","to")
conn.friends <- friends[,c("friendof","screenName")]
names(conn.friends) <- c("from","to")

rm(tweets,likes,friends)

# make the main conections table
conn.all <- bind_rows(conn.fav,conn.friends, conn.retweets)
rm(conn.fav, conn.friends, conn.retweets)

conn.simple <- plyr::count(conn.all, vars = c("from","to"))
names(conn.simple) <- c("from","to","weight")
#object.size(conn.simple)

# Make the graph

g <- graph_from_data_frame(conn.simple, directed = T) #Make the graph
g <- simplify(g, remove.loops = T) #Remove self references
gorder(g)
g <- delete.vertices(g, which(degree(g)<=1)) # Discard the edgees
gorder(g)

saveRDS(g,"twitter/data/parRerun/MidDataColectionGraph.Rds")


