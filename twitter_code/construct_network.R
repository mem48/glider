# Construct a network from the paralleised data

library(igraph)
library(dplyr)
library(tm)
library(tidytext)


# read in data 

# accounts
accounts1 <- readRDS("F:/Twitter/SecondRoundParReRun/AccountsList-2017-11-08-bs-1.Rds")
accounts2 <- readRDS("F:/Twitter/SecondRoundParReRun/AccountsList-2017-11-13-bs-60.Rds")
accounts3 <- readRDS("F:/Twitter/SecondRoundParReRun/AccountsList-2017-11-20-bs-176.Rds")
accounts4 <- readRDS("F:/Twitter/ThirdRound/AccountsList-2017-11-21-bs-1.Rds")
accounts5 <- readRDS("F:/Twitter/ThirdRound/AccountsList-2017-11-23-bs-6.Rds")
accounts6 <- readRDS("F:/Twitter/ThirdRound/AccountsList-2017-11-27-bs-24.Rds")

accounts1 <- bind_rows(accounts1)
accounts2 <- bind_rows(accounts2)
accounts3 <- bind_rows(accounts3)
accounts4 <- bind_rows(accounts4)
accounts5 <- bind_rows(accounts5)
accounts6 <- bind_rows(accounts6)


accounts.second <- bind_rows(accounts1,accounts2,accounts3,accounts4,accounts5,accounts6)
rm(accounts1,accounts2,accounts3,accounts4,accounts5,accounts6)

# tweets
tweets1 <- readRDS("F:/Twitter/SecondRoundParReRun/TweetsList-2017-11-08-bs-1.Rds")
tweets2 <- readRDS("F:/Twitter/SecondRoundParReRun/TweetsList-2017-11-13-bs-60.Rds")
tweets3 <- readRDS("F:/Twitter/SecondRoundParReRun/TweetsList-2017-11-20-bs-176.Rds")
tweets4 <- readRDS("F:/Twitter/ThirdRound/TweetsList-2017-11-21-bs-1.Rds")
tweets5 <- readRDS("F:/Twitter/ThirdRound/TweetsList-2017-11-23-bs-6.Rds")
tweets6 <- readRDS("F:/Twitter/ThirdRound/TweetsList-2017-11-27-bs-24.Rds")

tweets1 <- bind_rows(tweets1)
tweets2 <- bind_rows(tweets2)
tweets3 <- bind_rows(tweets3)
tweets4 <- bind_rows(tweets4)
tweets5 <- bind_rows(tweets5)
tweets6 <- bind_rows(tweets6)

tweets.second <- bind_rows(tweets1,tweets2,tweets3,tweets4,tweets5,tweets6)
rm(tweets1,tweets2,tweets3,tweets4,tweets5,tweets6)

# likes (favorites)

likes1 <- readRDS("F:/Twitter/SecondRoundParReRun/FavoritesList-2017-11-08-bs-1.Rds")
likes2 <- readRDS("F:/Twitter/SecondRoundParReRun/FavoritesList-2017-11-13-bs-60.Rds")
likes3 <- readRDS("F:/Twitter/SecondRoundParReRun/FavoritesList-2017-11-20-bs-176.Rds")
likes4 <- readRDS("F:/Twitter/ThirdRound/FavoritesList-2017-11-21-bs-1.Rds")
likes5 <- readRDS("F:/Twitter/ThirdRound/FavoritesList-2017-11-23-bs-6.Rds")
likes6 <- readRDS("F:/Twitter/ThirdRound/FavoritesList-2017-11-27-bs-24.Rds")

likes1 <- bind_rows(likes1)
likes2 <- bind_rows(likes2)
likes3 <- bind_rows(likes3)
likes4 <- bind_rows(likes4)
likes5 <- bind_rows(likes5)
likes6 <- bind_rows(likes6)

likes.second <- bind_rows(likes1,likes2,likes3,likes4,likes5,likes6)
rm(likes1,likes2,likes3,likes4,likes5,likes6)

# friends 

friends1 <- readRDS("F:/Twitter/SecondRoundParReRun/FriendsList-2017-11-08-bs-1.Rds")
friends2 <- readRDS("F:/Twitter/SecondRoundParReRun/FriendsList-2017-11-13-bs-60.Rds")
friends3 <- readRDS("F:/Twitter/SecondRoundParReRun/FriendsList-2017-11-20-bs-176.Rds")
friends4 <- readRDS("F:/Twitter/ThirdRound/FriendsList-2017-11-21-bs-1.Rds")
friends5 <- readRDS("F:/Twitter/ThirdRound/FriendsList-2017-11-23-bs-6.Rds")
friends6 <- readRDS("F:/Twitter/ThirdRound/FriendsList-2017-11-27-bs-24.Rds")

friends1 <- bind_rows(friends1)
friends2 <- bind_rows(friends2)
friends3 <- bind_rows(friends3)
friends4 <- bind_rows(friends4)
friends5 <- bind_rows(friends5)
friends6 <- bind_rows(friends6)

friends.second <- bind_rows(friends1,friends2,friends3,friends4,friends5,friends6)
rm(friends1,friends2,friends3,friends4,friends5,friends6)

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
message(paste0("We have ",nrow(accounts)," Accounts, ",nrow(friends)," friends, ",nrow(likes)," likes, and ",nrow(tweets)," tweets"))

saveRDS(accounts,"../twitter_data/all/accounts.Rds")
saveRDS(friends,"../twitter_data/all/friends.Rds")
saveRDS(likes,"../twitter_data/all/likes.Rds")
saveRDS(tweets,"../twitter_data/all/tweets.Rds")

accounts <- readRDS("../twitter_data/all/accounts.Rds")
friends <- readRDS("../twitter_data/all/friends.Rds")
likes <- readRDS("../twitter_data/all/likes.Rds")
tweets <- readRDS("../twitter_data/all/tweets.Rds")

# trim tweets to just retweets
tweets <- tweets[tweets$isRetweet,]

# Get retweet names from the text of the tweet
locs <- gregexpr(": ",tweets$text)
locs <- sapply(locs, function (x) x[1])
tweets$retweetof <- substr(tweets$text,5,locs - 1)
summary(is.na(tweets$retweetof))
rm(locs)

#Look for keywords
#keywords <- c("architecture","architects", "architect", "architectural",
#              "building", "builder","built", "buildings",
#              "construction","climate change","climate",
#              "design",
#              "energy efficiency","efficiency","eco-renovation", "eco-retrofit","eco", "electrician", "energy", "environmental",
#              "environment","engineering", 
#              "green", "glazing",
#              "housing", "homes","home", "house",
#              "insulation",
#              "low carbon",
#              "plumber","plumbing","property", "planning", "passivhaus",
#              "windows",
#              "retrofit","rennovation","refurbishment", "renewables", "renewable", "roofing",
#              "smart","sustainable","solar","sustainability")

#matches.tweets <- grep(pattern = paste(keywords,collapse="|"), x = tweets$text, ignore.case = TRUE, value = FALSE)
#matches.tweets <- 1:nrow(tweets) %in% matches.tweets
#summary(matches.tweets)


#matches.likes <- grep(pattern = paste(keywords,collapse="|"), x = likes$text, ignore.case = TRUE, value = FALSE)
#matches.likes <- 1:nrow(likes) %in% matches.likes
#summary(matches.likes)

#tweets$weight <- ifelse(matches.tweets, 1, 0.1)
#likes$weight <- ifelse(matches.likes, 1, 0.1)
#friends$weight <- 1


# construct Connections Table
conn.fav <- likes[,c("favOf","screenName")]
names(conn.fav) <- c("from","to")
conn.retweets <- tweets[,c("screenName","retweetof")]
names(conn.retweets) <- c("from","to")
conn.friends <- friends[,c("friendof","screenName")]
names(conn.friends) <- c("from","to")

#rm(tweets,likes,friends)

# make the main conections table
conn.all <- bind_rows(conn.fav,conn.friends, conn.retweets)
sum(conn.all$weight)




rm(conn.fav, conn.friends, conn.retweets)

#conn.simple <- conn.all %>% group_by(from,to) %>% summarise(weight = sum(weight))
#sum(conn.simple$weight)

conn.simple <- plyr::count(conn.all, vars = c("from","to"))
names(conn.simple) <- c("from","to","weight")
object.size(conn.simple)

# Make the graph

g <- graph_from_data_frame(conn.simple, directed = T) #Make the graph
g <- simplify(g, remove.loops = T) #Remove self references
gorder(g)
g <- delete.vertices(g, which(degree(g)<=1)) # Discard the edgees
gorder(g)

#saveRDS(g,"twitter/data/parRerun/SecondRound_300batches_Graph.Rds")
saveRDS(g,"../twitter_data/parRerun/All_Data.Rds")

