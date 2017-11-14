# Post WP3 workshop work
library(igraph)

# Read in the data
conn.simple <- readRDS("twitter/conn-simple-summyprogress2.Rds")

# Make Graph
g <- graph_from_data_frame(conn.simple, directed = T) #Make the graph
g <- simplify(g, remove.loops = T) #Remove self references
gorder(g)

#Remove those with only one connection
g <- delete.vertices(g, which(degree(g)<=10))
gorder(g)

g.trim <- delete.vertices(g, which(degree(g)<=30))
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





# make summary table
# make a list of these accounts
verts <- as_data_frame(g.trim, what="vertices")
verts$degreeStrength.out <-  verts$strength.out / verts$degree.out
verts$degreeStrength.in <-  verts$strength.in / verts$degree.in

#Get detaisl of the user accounts
library(twitteR)
source("twitter/secrets.R")
source("twitter/functions.R")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
rm(consumer_key, consumer_secret, access_token, access_secret)

account.details <- get.users(verts$name)
account.details <- account.details[,c("description","statusesCount","followersCount","favoritesCount","friendsCount","name","created","protected","verified","screenName","location","lang","id")]

saveRDS(account.details,"twitter/TwitterAccountsReview-Detailed.Rds")


# join togther the two datasets





write.csv(verts,"twitter/TwitterAccountsReview.csv", row.names = F)

g.test <- delete.vertices(g.trim, which(degree(g.trim)<=1200))
gorder(g.test)

#Applying the color scale to edge weights.
#rgb method is to convert colors to a character vector.
c_scale <- colorRamp(c('blue','cyan','yellow','red'))
V(g.test)$color = apply(c_scale(V(g.test)$PageRank), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )

#Create High Resolution Plots
svg(filename="gavindemo.svg", 
    width=25, 
    height=25, 
    pointsize=14)
par(mar = c(1,1,1,1))
plot(g.test, 
     edge.width = E(g.test)$weight/ 20,
     vertex.size = V(g.test)$strength.total / 1000 ,
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

# test making a newtork withou the core
core <- read.csv("../Glider/twitter/accounts.csv", stringsAsFactors = F)
core <- core$Twitter
core <- core[!is.na(core)]
core <- core[! core == ""]

#g.test3 <- delete.vertices(gt, V(gt)$name %in% removelist)
g.test3 <- delete.vertices(g.trim, V(g.trim)$name %in% core)
g.test3 <- delete.vertices(g.test3, which(degree(g.test3)<=300))
gorder(g.test3)

#Create High Resolution Plots
svg(filename="coreremoved.svg", 
    width=25, 
    height=25, 
    pointsize=14)
par(mar = c(1,1,1,1))
plot(g.test, 
     edge.width = E(g.test)$weight/ 20,
     vertex.size = V(g.test)$degree.in / 100 ,
     edge.arrow.size = 0,
     vertex.color = V(g.test)$color,
     vertex.label.family= "Helvetica",
     vertex.label.color = "black",
     vertex.frame.color = V(g.test)$color,
     layout = layout_nicely, 
     rescale = T, 
     axes = F)
dev.off()
