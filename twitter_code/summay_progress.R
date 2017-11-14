# Summaries what we know so far
library(igraph)

# Make a list of accounts
accounts.core <- readRDS("twitter/data/coreAccounts.Rds")

#Get the list of favorites
favorites <- readRDS("twitter/data/coreT-favorites-2017-08-14.Rds")
favorites2 <- readRDS("twitter/data/corenT-favorites-2017-08-15.Rds")
favorites3 <- readRDS("twitter/data/corenTvlarge-favorites-2017-08-15.Rds")
favorites.all <- rbind(favorites,favorites2,favorites3)
conn.fav <- favorites.all[,c("favOf","screenName")]
names(conn.fav) <- c("from","to")


#Get the retweets
retweets <- readRDS("twitter/Tweets-2017-07-10.Rds")
retweets <- retweets[retweets$isRetweet,]
locs <- gregexpr(": ",retweets$text)
locs <- sapply(locs, function (x) x[1])
retweets$retweetof <- substr(retweets$text,5,locs - 1)
rm(locs)

conn.retweets <- retweets[,c("screenName","retweetof")]
names(conn.retweets) <- c("from","to")



#Get the friends
friends <- readRDS("twitter/data/core-friends-2017-08-08.Rds")

conn.friends <- friends[,c("friendof","screenName")]
names(conn.friends) <- c("from","to")


# Make a ful list of connections
conn.all <- rbind(conn.fav,conn.retweets, conn.friends)

#Get weights
conn.simple <- unique(conn.all)
conn.simple$weight <- 0

get.weights <- function(a){
  sub <- conn.all[conn.all$from == conn.simple$from[a] & conn.all$to == conn.simple$to[a],]
  res <- as.integer(nrow(sub))
  return(res)
}

weights <- lapply(1:nrow(conn.simple),get.weights)
weights <- unlist(weights)
conn.simple$weight <- weights

g <- graph_from_data_frame(conn.simple, directed = T) #Made the graph
gorder(g)
g <- simplify(g, remove.loops = T) #Remove self references
gorder(g)

#Remove those with only one connection
g <- delete.vertices(g, which(degree(g)<=1))
gorder(g)
#make a list of these accounts
verts <- as_data_frame(g, what="vertices")
verts$degree.in <- degree(g, mode = "in")
verts$degree.out <- degree(g, mode = "out")
verts$strength.in <- strength(g, mode = "in")
verts$strength.out <- strength(g, mode = "out")

#Cleanup
rm(conn.fav,conn.friends,conn.retweets,conn.all,favorites,favorites.all,favorites2,favorites3,friends,retweets)
saveRDS(verts,"twitter/data/secondLevelAccounts.Rds")

summary(verts$degree.in + verts$degree.out)
summary(verts$strength.in + verts$strength.out)

#Make a trimmed graph
gt <- delete.vertices(g, which(degree(g)<5))
gorder(gt)

#Create High Resolution Plots
svg(filename="Core-fav-RT-follow-5plus.svg", 
    width=15, 
    height=15, 
    pointsize=10)
par(mar = c(1,1,1,1))
plot(gt, 
     edge.width = E(g)$weight/5,
     vertex.size = 5,
     layout = layout_with_fr, 
     rescale = T, 
     axes = F)
dev.off()

verts2 <- as_data_frame(gt, what="vertices")
verts2$degree.in <- degree(gt, mode = "in")
verts2$degree.out <- degree(gt, mode = "out")
verts2$strength.in <- strength(gt, mode = "in")
verts2$strength.out <- strength(gt, mode = "out")

saveRDS(verts2,"twitter/data/secondLevelAccounts-5plus.Rds")

#Get account summary for these accounts
source("twitter/functions.R")

accounts.sec <- get.users(verts$name)

saveRDS(verts,"twitter/data/secondLevelAccountsDetailed.Rds")


g.test <- delete.vertices(g, which(degree(g)>500))
gorder(g.test)
g.test <- delete.vertices(g.test, which(degree(g.test)<=1))
gorder(g.test)

foo <- transitivity(g.test)





#Betweeness Centrality

between <- betweenness(g.test)
close <- closeness(g.test)
summary(between)
summary(close)
V(g.test)$between <- between/max(between)
V(g.test)$close <- close/max(close)
summary(V(g.test)$between)
summary(V(g.test)$close)
V(g.test)$degree <- degree(g.test, mode = "total") / max(V(g.test)$degree)
summary(V(g.test)$degree)

#Floor min values
V(g.test)$degree[V(g.test)$degree == min(V(g.test)$degree)] <- 0
V(g.test)$degree[V(g.test)$degree == min(V(g.test)$degree)] <- 0

#Color scaling function
c_scale <- colorRamp(c('white','red','yellow','cyan','blue'))

#Applying the color scale to edge weights.
#rgb method is to convert colors to a character vector.
V(g.test)$color = apply(c_scale(V(g.test)$degree), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )


#Create High Resolution Plots
svg(filename="test2.svg", 
    width=15, 
    height=15, 
    pointsize=10)
par(mar = c(1,1,1,1))
plot(g.test, 
     edge.width = E(g.test)$weight/5,
     vertex.size = 3,
     vertex.color = V(g.test)$color,
     layout = layout_nicely, 
     rescale = T, 
     axes = F)
dev.off()




stop()





#accounts$acc.type[is.na(accounts$acc.type) & accounts$screenName %in% friends$screenName] <- "Core Friend"
#summary(as.factor(accounts$acc.type))

#Ignore the followers as there are too many and they are often out of scope
missing <- favorites[!favorites$screenName %in% accounts$screenName,]

accounts. <- friends$screenName[!friends$screenName %in% accounts$screenName]

#add an account classification
accounts$acc.type <- NA
accounts$acc.type[accounts$screenName %in% accounts.core$screenName] <- "Core"
summary(as.factor(accounts$acc.type))


accounts$acc.type[is.na(accounts$acc.type) & accounts$screenName %in% favorites$screenName] <- "CoreT Favorite"
summary(as.factor(accounts$acc.type))

accounts$acc.type[is.na(accounts$acc.type) & accounts$screenName %in% retweets$retweetof] <- "Core Retweet"
summary(as.factor(accounts$acc.type))