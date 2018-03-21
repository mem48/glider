# Plot Graphs new Textmin data
library(igraph)
library(tm)

conn.simple <- readRDS("../twitter_data/all/conn_simple_types_textanal.Rds")
conn.simple$friends <- as.numeric(conn.simple$friends)

g <- graph_from_data_frame(conn.simple, directed = T) #Make the graph
g <- simplify(g, remove.loops = T, remove.multiple = F) #Remove self references
gorder(g)
g <- delete.vertices(g, which(degree(g, mode = "out") == 0)) # DO several time as first removal may create new ones
g <- delete.vertices(g, which(degree(g, mode = "out") == 0))
g <- delete.vertices(g, which(degree(g, mode = "out") == 0))
gorder(g)
ecount(g)

# look for only mutual edges (i.e. from A to B and B to A)
g.trim <- delete.edges(g, which(!which_mutual(g, es = E(g))))
ecount(g.trim)
g.trim <- delete.edges(g.trim, (which(E(g.trim)$weight < 2) ))
ecount(g.trim)
g.trim <- delete.edges(g, which(!which_mutual(g, es = E(g))))
ecount(g.trim)
g.trim <- delete.vertices(g.trim, which(degree(g.trim, mode = "total") == 0))
gorder(g.trim)

V(g.trim)$strength.total <- strength(g.trim, mode = "total")

# clustering
clus = cluster_infomap(g.trim, e.weights = E(g.trim)$weight, nb.trials = 10)
V(g.trim)$cluster <- membership(clus)

clus.friends = cluster_infomap(g.trim, e.weights = E(g.trim)$friends, nb.trials = 10)
V(g.trim)$cluster.friends <- membership(clus.friends)

clus.retweets = cluster_infomap(g.trim, e.weights = E(g.trim)$retweets, nb.trials = 10)
V(g.trim)$cluster.retweets <- membership(clus.retweets)

clus.mentions = cluster_infomap(g.trim, e.weights = E(g.trim)$mentions, nb.trials = 10)
V(g.trim)$cluster.mentions <- membership(clus.mentions)

clus.likes = cluster_infomap(g.trim, e.weights = E(g.trim)$likes, nb.trials = 10)
V(g.trim)$cluster.likes <- membership(clus.likes)


verts <- igraph::as_data_frame(g.trim, what="vertices")
clusters.table <- as.data.frame(table(verts$cluster))
clusters.table$Var1 <- as.integer(as.character(clusters.table$Var1))
largeclusters <- clusters.table[clusters.table$Freq >= 50,]

#assign colours
colours.all = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
#colours = sample ( rainbow ( max ( V(g.trim)$cluster, na.rm= T )  + 1) )
colours = sample (colours.all, max ( V(g.trim)$cluster, na.rm= T ), replace = T )
V(g.trim)$colours = colours[V(g.trim)$cluster]

colours.friends = sample (colours.all, max ( V(g.trim)$cluster.friends, na.rm= T ), replace = T )
V(g.trim)$colours.friends = colours[V(g.trim)$cluster.friends]

colours.retweets = sample (colours.all, max ( V(g.trim)$cluster.retweets, na.rm= T ), replace = T )
V(g.trim)$colours.retweets = colours[V(g.trim)$cluster.retweets]

colours.mentions = sample (colours.all, max ( V(g.trim)$cluster.mentions, na.rm= T ), replace = T )
V(g.trim)$colours.mentions = colours[V(g.trim)$cluster.mentions]

colours.likes = sample (colours.all, max ( V(g.trim)$cluster.likes, na.rm= T ), replace = T )
V(g.trim)$colours.likes = colours[V(g.trim)$cluster.likes]


#make small clusters black
V(g.trim)$colours[!V(g.trim)$cluster %in% largeclusters$Var1] <- "#000000"
V(g.trim)$colours.likes[V(g.trim)$cluster.likes %in% largeclusters$Var1] <- "#000000"


layout.nosimmer =  layout_with_drl(g.trim, weights = E(g.trim)$weight, options=list(simmer.attraction=0), dim = 2)
g.plot <- delete.edges(g.trim, (which(E(g.trim)$weight < 50) ))

png(filename=paste0("../twitter_plots/connect_types/","all_reciprocal_nosimmer",".png"), width=10, height=10, units = 'in', res = 600, pointsize=6)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.plot,
                                          edge.width =  E(g.plot)$weight/ 200,
                                          vertex.size = ifelse((V(g.plot)$strength.total / 3000) < 15, (V(g.plot)$strength.total / 3000) ,15 ),
                                          edge.arrow.size = 0.1,
                                          edge.curved=0.2,
                                          vertex.color = V(g.trim)$colours,
                                          #vertex.label = ifelse(V(g.plot)$strength.total > 9000, V(g.plot)$name, NA),
                                          vertex.label = NA,
                                          vertex.label.family= "arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = V(g.trim)$colours,
                                          layout = layout.nosimmer, 
                                          rescale = T, 
                                          axes = F); dev.off()




g.plot <- delete.edges(g.trim, (which(E(g.trim)$likes < 50) ))

png(filename=paste0("../twitter_plots/connect_types/","likes_reciprocal_nosimmer",".png"), width=10, height=10, units = 'in', res = 600, pointsize=6)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.plot,
                                          edge.width =  E(g.plot)$likes/ 200,
                                          vertex.size = ifelse((V(g.plot)$strength.total / 3000) < 15, (V(g.plot)$strength.total / 3000) ,15 ),
                                          edge.arrow.size = 0.1,
                                          edge.curved=0.2,
                                          vertex.color = V(g.trim)$colours.likes,
                                          #vertex.label = ifelse(V(g.plot)$strength.total > 9000, V(g.plot)$name, NA),
                                          vertex.label = NA,
                                          vertex.label.family= "arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = V(g.trim)$colours.likes,
                                          layout = layout.nosimmer, 
                                          rescale = T, 
                                          axes = F); dev.off()

g.plot <- delete.edges(g.trim, (which(E(g.trim)$retweets < 50) ))

png(filename=paste0("../twitter_plots/connect_types/","retweets_reciprocal_nosimmer",".png"), width=10, height=10, units = 'in', res = 600, pointsize=6)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.plot,
                                          edge.width =  E(g.plot)$retweets/ 200,
                                          vertex.size = ifelse((V(g.plot)$strength.total / 3000) < 15, (V(g.plot)$strength.total / 3000) ,15 ),
                                          edge.arrow.size = 0.1,
                                          edge.curved=0.2,
                                          vertex.color = V(g.trim)$colours.retweets,
                                          #vertex.label = ifelse(V(g.plot)$strength.total > 9000, V(g.plot)$name, NA),
                                          vertex.label = NA,
                                          vertex.label.family= "arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = V(g.trim)$colours.retweets,
                                          layout = layout.nosimmer, 
                                          rescale = T, 
                                          axes = F); dev.off()


g.plot <- delete.edges(g.trim, (which(E(g.trim)$mentions < 50) ))

png(filename=paste0("../twitter_plots/connect_types/","mentions_reciprocal_nosimmer",".png"), width=10, height=10, units = 'in', res = 600, pointsize=6)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.plot,
                                          edge.width =  E(g.plot)$mentions/ 200,
                                          vertex.size = ifelse((V(g.plot)$strength.total / 3000) < 15, (V(g.plot)$strength.total / 3000) ,15 ),
                                          edge.arrow.size = 0.1,
                                          edge.curved=0.2,
                                          vertex.color = V(g.trim)$colours.mentions,
                                          #vertex.label = ifelse(V(g.plot)$strength.total > 9000, V(g.plot)$name, NA),
                                          vertex.label = NA,
                                          vertex.label.family= "arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = V(g.trim)$colours.mentions,
                                          layout = layout.nosimmer, 
                                          rescale = T, 
                                          axes = F); dev.off()


g.plot <- delete.edges(g.trim, (which(E(g.trim)$friends < 50) ))

png(filename=paste0("../twitter_plots/connect_types/","friends_reciprocal_nosimmer",".png"), width=10, height=10, units = 'in', res = 600, pointsize=6)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.plot,
                                          edge.width =  E(g.plot)$friends/ 200,
                                          vertex.size = ifelse((V(g.plot)$strength.total / 3000) < 15, (V(g.plot)$strength.total / 3000) ,15 ),
                                          edge.arrow.size = 0.1,
                                          edge.curved=0.2,
                                          vertex.color = V(g.trim)$colours.friends,
                                          #vertex.label = ifelse(V(g.plot)$strength.total > 9000, V(g.plot)$name, NA),
                                          vertex.label = NA,
                                          vertex.label.family= "arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = V(g.trim)$colours.friends,
                                          layout = layout.nosimmer, 
                                          rescale = T, 
                                          axes = F); dev.off()


# get the variaibles

g.trim$friends.out <- strength(g.trim, weights = E(g.trim)$friends, mode = "out")
g.trim$mentions.out <- strength(g.trim, weights = E(g.trim)$mentions, mode = "out")
g.trim$mentions.out <- strength(g.trim, weights = E(g.trim)$mentions, mode = "out")

summary(g.trim$friends.out)
summary(g.trim$mentions.out)


# Summarise the Clusters
tweets <- readRDS("../twitter_data/all/tweets.Rds") # get the tweets
tweets <- tweets[,c("screenName","text")]
names(tweets)

myStopwords <- c(stopwords('english'),"rt","can","now","must","twitter","website",
                 "tweet","dm","days","got","done","th","says","uk","years",
                 "got","going","latest","s","amp","looking","join","blog","getting")
extrastops <- read.delim("../twitter_code/stopwords.txt", header = F, stringsAsFactors = F)
extrastops <- extrastops$V1[!extrastops$V1 %in% myStopwords]
myStopwords <- c(myStopwords, extrastops)
myStopwords <- unique(myStopwords)

clusterres <- list()

for(i in largeclusters$Var1){
  verts_sub <- verts[verts$cluster == i,]
  verts_sub <- verts_sub[order(-verts_sub$strength.total),]
  message(paste0(Sys.time()," The Top members of cluster ",i," are ",paste(verts_sub$name[1:5], collapse = " ")," it has ",nrow(verts_sub)," members"))
  tweets_sub <- tweets[tweets$screenName %in% verts_sub$name,]
  
  # build a corpus, and specify the source to be character vectors
  tweetCorpus <- Corpus(VectorSource(tweets_sub$text))
  # remove URLs
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  tweetCorpus <- tm_map(tweetCorpus, content_transformer(removeURL))
  # remove anything other than English letters or space
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  tweetCorpus <- tm_map(tweetCorpus, content_transformer(removeNumPunct))
  # convert to lower case
  tweetCorpus <- tm_map(tweetCorpus, content_transformer(tolower))
  # remove stopwords
  
  tweetCorpus <- tm_map(tweetCorpus, removeWords, myStopwords)
  # remove extra whitespace
  tweetCorpus <- tm_map(tweetCorpus, stripWhitespace)
  #Make Term Doument Matrix
  tdm <- TermDocumentMatrix(tweetCorpus, control = list(wordLengths = c(1, Inf)))
  
  #Look for Frequent Terms
  freq.terms <- findFreqTerms(tdm, lowfreq = 1000)
  
  m <- as.matrix(tdm[freq.terms,])
  msums <- rowSums(m)
  msums <- msums[order(-msums)]
  
  message(paste0(Sys.time()," The Top keywords ",i," are ",paste(names(msums)[1:5], collapse = " ")))
  
  result <- list()
  result[[1]] <- verts_sub
  result[[2]] <- msums
  clusterres[[i]] <- result
}

saveRDS(clusterres,"../twitter_data/cluster_summaries.Rds")
rm(tweets)

g.contract <- delete.vertices(g.trim, which(!V(g.trim)$cluster %in% largeclusters$Var1))

g.contract <- delete_vertex_attr(g.contract, "strength.total")

g.contract <- contract(g.contract, V(g.contract)$cluster , vertex.attr.comb=toString) 
gorder(g.contract)



#Remove the Duplicated Values
for(i in 1:gorder(g.contract)){
  if(grepl(",",V(g.contract)$cluster[i])){
    V(g.contract)$cluster[i] <- substr(V(g.contract)$cluster[i],1,(regexpr(",",(V(g.contract)$cluster[i]))[[1]] - 1))
  }
  if(grepl(",",V(g.contract)$colours[i])){
    V(g.contract)$colours[i] <- substr(V(g.contract)$colours[i],1,(regexpr(",",(V(g.contract)$colours[i]))[[1]] - 1))
  }
}

g.contract <- simplify(g.contract, remove.loops = T, remove.multiple = T, edge.attr.comb = "sum")
g.contract <- delete.edges(g.contract, (which(E(g.contract)$weight < 500) ))
g.contract <- delete.vertices(g.contract, which(degree(g.contract, mode = "total") < 1))

gorder(g.contract)
ecount(g.contract)

V(g.contract)$degree.total <- degree(g.contract, mode = "total")
V(g.contract)$degree.in <- degree(g.contract, mode = "in")
V(g.contract)$degree.out <- degree(g.contract, mode = "out")
V(g.contract)$strength.total <- strength(g.contract, mode = "total")
V(g.contract)$strength.in <- strength(g.contract, mode = "in")
V(g.contract)$strength.out <- strength(g.contract, mode = "out")

gorder(g.contract)
ecount(g.contract)


ecount(g.contract)


#change the names to top 3 accounts

for(i in V(g.contract)$cluster){
  inumb <- as.numeric(i)
  names <- clusterres[[inumb]][[1]]
  names <- names[1:3,"name"]
  names <- paste(names, collapse = " ")
  V(g.contract)$name[V(g.contract)$cluster == inumb] <- names
}


verts.contract <- igraph::as_data_frame(g.contract, what="vertices")

foo <- igraph::as_data_frame(g.contract, what="edges")
#colours.contract = sample ( rainbow ( max ( V(g.contract)$clus.likes, na.rm= T )  + 1) )
#V(g.likes)$colours.likes = colours.likes[V(g.likes)$clus.likes +1]

V(g.contract)$strength.total <- strength(g.contract, mode = "total")
V(g.contract)$strength.in <- strength(g.contract, mode = "in")
V(g.contract)$strength.out <- strength(g.contract, mode = "out")


layout.contract <- layout_with_drl(g.contract, weights = E(g.contract)$weight, options=list(simmer.attraction=0), dim = 2)

png(filename=paste0("../twitter_plots/connect_types/","contracted_cluster",".png"), width=10, height=10, units = 'in', res = 600, pointsize=4)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.contract,
                                          edge.width = E(g.contract)$weight/ 1000,
                                          vertex.size = V(g.contract)$strength.total / 10000 ,
                                          edge.arrow.size = 0.5,
                                          edge.curved=0.1,
                                          vertex.color = V(g.contract)$colours,
                                          vertex.label = V(g.contract)$name ,
                                          vertex.label.family= "Arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = "grey",
                                          layout = layout_nicely, 
                                          rescale = T, 
                                          axes = F); dev.off()


#########
# Summarise the clusters
library(dplyr)
cluster.summary <- list()
for(i in largeclusters$Var1){
  clus.names <- clusterres[[i]][[1]]
  clus.names <- clus.names$name[1:5]
  clus.names <- paste(clus.names, collapse = " ")
  clus.keyword <- clusterres[[i]][[2]]
  clus.keyword <- names(clus.keyword)[1:5]
  clus.keyword <- paste(clus.keyword, collapse = " ")
  
  result <- data.frame(clusNo = i, accounts = clus.names, keywords = clus.keyword)
  cluster.summary[[i]] <- result
  rm(result,clus.names,clus.keyword)
}
cluster.summary <- bind_rows(cluster.summary)

