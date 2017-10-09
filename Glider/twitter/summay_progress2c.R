## Summarise Progress so for

library(parallel)

# read in accoutns list
verts <- readRDS("twitter/data/secondLevelAccounts.Rds")
verts <- verts[order(-verts$strength.in),]

#Add in extra columns
verts$likes <- 0
verts$friends <- 0
verts$retweets <- 0


#get data collected so far
friends <- readRDS("twitter/data/secondLevel-friends-first-1130-users-2017-08-28.Rds")
likes <- readRDS("twitter/data/secondLevel-favorites-first-1276-users-2017-08-18.Rds")
retweets <- readRDS("twitter/data/secondLevel-tweets-first-2490-users-2017-08-25.Rds")
retweets <- retweets[retweets$isRetweet,]

#alos get the first round data
friends.1st <- readRDS("twitter/data/core-friends-2017-08-08.Rds")

likes <- readRDS("twitter/data/corenT-favorites-2017-08-15.Rds")
likes.1st.trim <- readRDS("twitter/data/coreT-favorites-2017-08-14.Rds")
likes.1st.vlarge <- readRDS("twitter/data/corenTvlarge-favorites-2017-08-15.Rds")

retweets.1st <- readRDS("twitter/Tweets-2017-07-10.Rds")
retweets.1st <- retweets.1st[retweets.1st$isRetweet,]


#Bind togther
friends <- rbind(friends,friends.1st)
likes <- rbind(likes,likes,likes.1st.trim,likes.1st.vlarge)
retweets <- rbind(retweets,retweets.1st)

summary(duplicated(friends))
summary(duplicated(likes))
summary(duplicated(retweets))

#Remove Duplicates
friends <- friends[!duplicated(friends),]
likes <- likes[!duplicated(likes),]
retweets <- retweets[!duplicated(retweets),]

rm(friends.1st, likes.1st.trim, likes.1st.vlarge, retweets.1st)

#Summaries the number of data point gathered
count.likes <- function(a){
  id <- verts$name[a]
  res <- length(likes$favOf[likes$favOf == id])
  return(res)
}

count.friends <- function(a){
  id <- verts$name[a]
  res <- length(friends$friendof[friends$friendof == id])
  return(res)
}

count.retweets <- function(a){
  id <- verts$name[a]
  res <- length(retweets$screenName[retweets$screenName == id])
  return(res)
}

verts$likes <- sapply(1:nrow(verts),count.likes)
verts$friends <- sapply(1:nrow(verts),count.friends)
verts$retweets <- sapply(1:nrow(verts),count.retweets)

#Get retweet names
locs <- gregexpr(": ",retweets$text)
locs <- sapply(locs, function (x) x[1])
retweets$retweetof <- substr(retweets$text,5,locs - 1)
rm(locs)

#Make Connections Tables
conn.fav <- likes[,c("favOf","screenName")]
names(conn.fav) <- c("from","to")
conn.retweets <- retweets[,c("screenName","retweetof")]
names(conn.retweets) <- c("from","to")
conn.friends <- friends[,c("friendof","screenName")]
names(conn.friends) <- c("from","to")

# Make a ful list of connections
conn.all <- rbind(conn.fav,conn.retweets, conn.friends)
rm(conn.fav, conn.retweets, conn.friends)

#Get weights
conn.simple <- unique(conn.all)
conn.simple$weight <- 0

get.weights <- function(a){
  id.from <- conn.simple$from[a]
  id.to <- conn.simple$to[a]
  res <- length(conn.all$from[conn.all$from == id.from & conn.all$to == id.to])
  return(res)
}

get.weights.presel <- function(a){
  id.from <- conn.simple$from[a]
  id.to <- conn.simple$to[a]
  res.sub <- conn.all[conn.all$from == id.from,]
  res.sub <- res.sub[res.sub$to == id.to,]
  res <- length(res.sub$from)
  return(res)
}

### Simple
res <- lapply(1:nrow(conn.simple), get.weights.presel)
weights <- unlist(res)


##########################################################
#Parallel
start <- Sys.time()
fun <- function(cl){
  parLapply(cl, 1:nrow(conn.simple), get.weights.presel)
}
cl <- makeCluster(4) #make clusert and set number of cores
clusterExport(cl=cl, varlist=c("conn.simple","conn.all"))
res <- fun(cl)
stopCluster(cl)
end <- Sys.time()
message(paste0("Did ",nrow(conn.simple)," lines in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
rm(cl,start,end)
##########################################################
weights <- unlist(res)

#weights <- sapply(1:nrow(conn.simple),get.weights)
conn.simple$weight <- weights

saveRDS(conn.simple,"twitter/conn-simple-summyprogress2.Rds")

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
saveRDS(verts,"twitter/data/thirdLevelAccounts.Rds")

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

saveRDS(verts2,"twitter/data/thirdLevelAccounts-5plus.Rds")

#Get account summary for these accounts
source("twitter/functions.R")

accounts.sec <- get.users(verts$name)

saveRDS(verts,"twitter/data/thirdLevelAccountsDetailed.Rds")



g.test <- delete.vertices(gt, which(degree(gt)<2000))
gorder(g.test)
g.test <- delete.vertices(g.test, which(degree(g.test)<=1))
gorder(g.test)

#foo <- transitivity(g.test)





#Betweeness Centrality
degree <- degree(g.test, mode = "total")
between <- betweenness(g.test)
close <- closeness(g.test)
summary(between)
summary(close)
V(g.test)$between <- between/max(between)
V(g.test)$close <- close/max(close)
summary(V(g.test)$between)
summary(V(g.test)$close)
V(g.test)$degree <- degree / max(degree, na.rm = T)
summary(V(g.test)$degree)

#Floor min values
#V(g.test)$degree[V(g.test)$degree == min(V(g.test)$degree)] <- 0
#V(g.test)$degree[V(g.test)$degree == min(V(g.test)$degree)] <- 0

#Color scaling function
c_scale <- colorRamp(c('white','red','yellow','cyan','blue'))

#Applying the color scale to edge weights.
#rgb method is to convert colors to a character vector.
V(g.test)$color = apply(c_scale(V(g.test)$degree), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )


#Create High Resolution Plots
svg(filename="wp3demo.svg", 
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
