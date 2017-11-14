#Load in the core functions
source("twitter/functions.R")

#Get the original core accounts
acc.core <- read.csv("twitter/accounts_cores.csv")
acc.core <- as.character(acc.core$Twitter)
acc.core <- acc.core[acc.core != ""]


#Get data
results <- get.SNAdata(acc.core)

saveRDS(results,"twitter/data/parReRun/FirstRoundRawData.Rds")

#Break up the list
accounts <- results[[1]]
friends <- results[[2]]
favorites <- results[[3]]
tweets <- results[[4]]
rm(results)


#get retweets
retweets <- tweets[tweets$isRetweet,]

#Get retweet names
locs <- gregexpr(": ",retweets$text)
locs <- sapply(locs, function (x) x[1])
retweets$retweetof <- substr(retweets$text,5,locs - 1)
rm(locs)

#Make Connections Tables
conn.fav <- favorites[,c("favOf","screenName")]
names(conn.fav) <- c("from","to")
conn.retweets <- retweets[,c("screenName","retweetof")]
names(conn.retweets) <- c("from","to")
conn.friends <- friends[,c("friendof","screenName")]
names(conn.friends) <- c("from","to")

# Make a ful list of connections
conn.all <- rbind(conn.fav,conn.retweets, conn.friends)
rm(conn.fav, conn.retweets, conn.friends)



#Get weights

library(plyr)
conn.simple <- count(conn.all, vars = c("from","to"))
names(conn.simple) <- c("from","to","weight")


#conn.simple <- unique(conn.all)
#conn.simple$weight <- 0

#get.weights.presel <- function(a){
#  id.from <- conn.simple$from[a]
#  id.to <- conn.simple$to[a]
#  res.sub <- conn.all[conn.all$from == id.from,]
#  res.sub <- res.sub[res.sub$to == id.to,]
# # res <- length(res.sub$from)
#  return(res)
#}

### Simple
#res <- lapply(1:nrow(conn.simple), get.weights.presel)
#weights <- unlist(res)

#conn.simple$weight <- weights

saveRDS(conn.simple,"twitter/data/parReRun/FirstRoundConnections.Rds")

library(igraph)

# Make Graph
g <- graph_from_data_frame(conn.simple, directed = T) #Make the graph
g <- simplify(g, remove.loops = T) #Remove self references
gorder(g)

g.trim <- delete.vertices(g, which(degree(g)<=10))
gorder(g.trim)


# Calcualte Values
V(g.trim)$degree.total <- degree(g.trim, mode = "total")
V(g.trim)$degree.in <- degree(g.trim, mode = "in")
V(g.trim)$degree.out <- degree(g.trim, mode = "out")
V(g.trim)$strength.total <- strength(g.trim, mode = "total")
V(g.trim)$strength.in <- strength(g.trim, mode = "in")
V(g.trim)$strength.out <- strength(g.trim, mode = "out")
between <- betweenness(g.trim)
V(g.trim)$between <- between / max(between, na.rm = T)
closeness <- closeness(g.trim)
V(g.trim)$closeness <- closeness / max(closeness)
eigenvector <- eigen_centrality(g.trim, directed = T, scale = F, weights = E(g.trim)$weight)$vector
V(g.trim)$eigenvector <- eigenvector / max(eigenvector)
PageRank <- page_rank(g.trim, directed = TRUE, damping = 0.85, weights = E(g.trim)$weight)$vector
V(g.trim)$PageRank <- PageRank / max(PageRank)


g.test <- delete.vertices(g.trim, which(degree(g.trim)<=10))
gorder(g.test)

#Applying the color scale to edge weights.
#rgb method is to convert colors to a character vector.
c_scale <- colorRamp(c('blue','cyan','yellow','red'))
V(g.test)$color = apply(c_scale(V(g.test)$PageRank), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )

#Create High Resolution Plots
svg(filename="twitter/data/parRerun/firstround.svg", 
    width=25, 
    height=25, 
    pointsize=14)
par(mar = c(1,1,1,1))
plot(g.test, 
     edge.width = sqrt(E(g.test)$weight)/5,
     vertex.size = sqrt(V(g.test)$strength.total)/5 ,
     edge.arrow.size = 0.2,
     edge.curved=0.2,
     vertex.color = V(g.test)$color,
     vertex.label.family= "Helvetica",
     vertex.label.color = "black",
     vertex.frame.color = V(g.test)$color,
     layout = layout_nicely, 
     rescale = T, 
     axes = F)
dev.off()
