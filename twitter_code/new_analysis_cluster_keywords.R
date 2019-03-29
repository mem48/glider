# Describe clusters
library(igraph)
library(tm)
library(slam)
library(dplyr)


g.trim <- readRDS("../twitter_data/all/graph_advanced.Rds")

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



verts <- igraph::as_data_frame(g.trim, what="vertices")
clusters.table <- as.data.frame(table(verts$cluster.friends))
clusters.table$Var1 <- as.integer(as.character(clusters.table$Var1))
largeclusters <- clusters.table[clusters.table$Freq >= 50,]







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
  msums <-slam::row_sums(tdm, na.rm = T)
  msums <- msums[order(-msums)]
 
  message(paste0(Sys.time()," The Top keywords ",i," are ",paste(names(msums)[1:5], collapse = " ")))
  
  result <- list()
  result[[1]] <- verts_sub
  result[[2]] <- msums
  clusterres[[i]] <- result
}

saveRDS(clusterres,"../twitter_data/cluster_summaries_advanced.Rds")
rm(tweets)

clusterres.summary <- data.frame(id = largeclusters$Var1)

clusterres.summary$keywords <- NA
clusterres.summary$accounts <- NA
clusterres.summary$members  <- NA

for(i in largeclusters$Var1){
  acc <- clusterres[[i]][[1]]
  memb <- nrow(acc)
  acc <- acc$name[1:20]
  acc <- paste(acc, collapse = " ")
  keywds <- clusterres[[i]][[2]]
  keywds <- names(keywds)[1:20]
  keywds <- paste(keywds, collapse = " ")
  clusterres.summary$keywords[i] <- keywds
  clusterres.summary$accounts[i] <- acc
  clusterres.summary$members[i]  <- memb
}

#write.csv(clusterres.summary,"../twitter_data/cluster_summaries_advanced.csv", row.names = F)
clustenames <- read.csv("../twitter_data/cluster_summaries_advanced.csv", stringsAsFactors = F)
clustenames <- clustenames[,c("id","name")]


g.contract <- readRDS("../twitter_data/all/graph_contract_advanced_keywords.Rds")
verts.contract <- igraph::as_data_frame(g.contract, "vertices")

verts.contract$name <- NULL
clustenames$id <- as.character(clustenames$id)
verts.contract <- left_join(verts.contract, clustenames, by = c("cluster.friends" = "id"))
V(g.contract)$name <- verts.contract$name

layout.contract <- readRDS("../twitter_data/all/layout_contract_advanced.Rds")

png(filename=paste0("../twitter_plots/advanced/","contracted_cluster",".png"), width=10, height=10, units = 'in', res = 600, pointsize=8)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.contract,
                                          edge.width = E(g.contract)$weight/ 10000,
                                          vertex.size = V(g.contract)$strength.total / 20000 ,
                                          edge.arrow.size = 0.5,
                                          edge.curved=0.2,
                                          vertex.color = V(g.contract)$colours.friends,
                                          vertex.label = ifelse((V(g.contract)$cluster.friends %in% c(as.character(1:26))), (V(g.contract)$name ) ,NA ),
                                          vertex.label.family= "Arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = V(g.contract)$colours.friends,
                                          layout = layout.contract, 
                                          rescale = T, 
                                          axes = F); dev.off()


png(filename=paste0("../twitter_plots/advanced/","contracted_cluster",".png"), width=10, height=10, units = 'in', res = 600, pointsize=8)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.contract,
                                          edge.width = E(g.contract)$weight/ 10000,
                                          vertex.size = V(g.contract)$strength.total / 20000 ,
                                          edge.arrow.size = 0.5,
                                          edge.curved=0.2,
                                          vertex.color = V(g.contract)$colours.friends,
                                          vertex.label = ifelse((V(g.contract)$cluster.friends %in% c(as.character(1:26))), (V(g.contract)$name ) ,NA ),
                                          vertex.label.family= "Arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = V(g.contract)$colours.friends,
                                          layout = layout.contract, 
                                          rescale = T, 
                                          axes = F); dev.off()

