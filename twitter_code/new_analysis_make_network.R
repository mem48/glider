# Plot Graphs new Textmin data
library(igraph)
library(colorRamps)
library(tm)
library(dplyr)

conn.simple <- readRDS("../twitter_data/all/conn_simple_advanced.Rds")
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

clus.friends = cluster_infomap(g.trim, e.weights = E(g.trim)$friends, nb.trials = 10)
V(g.trim)$cluster.friends <- membership(clus.friends)


verts <- igraph::as_data_frame(g.trim, what="vertices")
clusters.table <- as.data.frame(table(verts$cluster.friends))
clusters.table$Var1 <- as.integer(as.character(clusters.table$Var1))
largeclusters <- clusters.table[clusters.table$Freq >= 50,]

largeclus <- function(type, size){
  largeclusters.tmp <- as.data.frame(table(verts[,paste0("cluster.",type)]))
  largeclusters.tmp$Var1 <- as.integer(as.character(largeclusters.tmp$Var1))
  largeclusters.tmp <- largeclusters.tmp[largeclusters.tmp$Freq >= size,]
  return(largeclusters.tmp$Var1)
}

largeclusters.friends <- largeclus("friends",50)
layout.nosimmer =  layout_with_drl(g.trim, weights = E(g.trim)$weight, options=list(simmer.attraction=0), dim = 2)
saveRDS(g.trim, )
saveRDS(layout.nosimmer, "../twitter_data/all/graph_layout_advanced.Rds")

#assign colours
colours <- c("green","red","cyan","yellow","darkviolet",
             "dodgerblue","chartreuse","cadetblue","violetred",
             "brown","darkgreen","darkorange","blueviolet",
             "hotpink","orangered","deepskyblue",
             "mediumpurple","coral","gold","firebrick",
             "greenyellow","springgreen","aquamarine","cornflowerblue",
             "chocolate")


par(mar = rep(2, 4))
plot(NULL, xlim=c(0,length(colours)), ylim=c(0,1),
     xlab=length(colours), ylab="", xaxt="n", yaxt="n")
rect(0:(length(colours)-1), 0, 1:length(colours), 1, col=colours)

colours <- c(colours, rep("black",max(V(g.trim)$cluster.friends)-length(colours)))

V(g.trim)$colours.friends = colours[V(g.trim)$cluster.friends]


g.plot <- delete.edges(g.trim, (which(E(g.trim)$weight < 50) ))

png(filename=paste0("../twitter_plots/advanced/","all_friends_cluster",".png"), width=10, height=10, units = 'in', res = 600, pointsize=6)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.plot,
                                          edge.width =  E(g.plot)$weight/ 200,
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
rm(extrastops)

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
  freq.terms <- findFreqTerms(tdm, lowfreq = 20000)
  
  #m <- as.matrix(tdm[freq.terms,])
  m <- as.matrix(tdm[freq.terms,])
  msums <- rowSums(m)
  msums <- msums[order(-msums)]
  
  message(paste0(Sys.time()," The Top keywords ",i," are ",paste(names(msums)[1:5], collapse = " ")))
  
  result <- list()
  result[[1]] <- verts_sub
  result[[2]] <- msums
  clusterres[[i]] <- result
}

saveRDS(clusterres,"../twitter_data/all/cluster_summaries_advanced.Rds")
rm(tweets)


#g.contract <- delete.vertices(g.trim, which(!V(g.trim)$cluster.friends %in% largeclusters$Var1))
g.contract <- g.trim
g.contract <- delete_vertex_attr(g.contract, "strength.total")

g.contract <- contract(g.contract, V(g.contract)$cluster.friends , vertex.attr.comb=toString) 
gorder(g.contract)



#Remove the Duplicated Values
for(i in 1:gorder(g.contract)){
  if(grepl(",",V(g.contract)$cluster.friends[i])){
    V(g.contract)$cluster.friends[i] <- substr(V(g.contract)$cluster.friends[i],1,(regexpr(",",(V(g.contract)$cluster.friends[i]))[[1]] - 1))
  }
  if(grepl(",",V(g.contract)$colours.friends[i])){
    V(g.contract)$colours.friends[i] <- substr(V(g.contract)$colours.friends[i],1,(regexpr(",",(V(g.contract)$colours.friends[i]))[[1]] - 1))
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

for(i in V(g.contract)$cluster.friends){
  inumb <- as.numeric(i)
  names <- clusterres[[inumb]][[1]]
  names <- names[1:3,"name"]
  names <- paste(names, collapse = " ")
  V(g.contract)$name[V(g.contract)$cluster.friends == inumb] <- names
}



#layout.contract2 <- layout_with_fr(g.contract, weights = E(g.contract)$weight, niter = 10000, dim = 2)

layout.contract <- matrix(ncol = 2, nrow = gorder(g.contract))
for(i in 1:gorder(g.contract)){
  clusterid <- as.numeric(V(g.contract)$cluster.friends[i])
  print(paste0(i," ",clusterid))
  layout.sub <- layout.nosimmer[V(g.trim)$cluster.friends == clusterid,]
  if(class(layout.sub) == "matrix"){
    layout.contract[i,1] <-  median(layout.sub[,1])
    layout.contract[i,2] <-  median(layout.sub[,2])
  }else{
    layout.contract[i,1] <-  median(layout.sub[1])
    layout.contract[i,2] <-  median(layout.sub[2])
  }
  
  
  
}

saveRDS(layout.contract,"../twitter_data/all/layout_contract_advanced.Rds")
saveRDS(g.contract,"../twitter_data/all/graph_contract_advanced.Rds")

png(filename=paste0("../twitter_plots/advanced/","contracted_cluster",".png"), width=10, height=10, units = 'in', res = 600, pointsize=4)   
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


# Plot keyword maps
verts <- igraph::as_data_frame(g.trim, what="vertices")
accounts.summary <- readRDS("../twitter_data/all/accounts_keywords_advanced.Rds")

verts.keywords <- left_join(verts, accounts.summary, by = c("name" = "screenName"))
verts.keywords[is.na(verts.keywords)] <- 0

keyword.types <- names(accounts.summary)
keyword.types <- keyword.types[!keyword.types %in% c("screenName","tweetscount")]

V(g.trim)$affordable    <- verts.keywords$affordable
V(g.trim)$architect     <- verts.keywords$architect
V(g.trim)$bed           <- verts.keywords$bed
V(g.trim)$bim           <- verts.keywords$bim
V(g.trim)$build         <- verts.keywords$build
V(g.trim)$carbonandclimate <- verts.keywords$carbonandclimate
V(g.trim)$CIBSE         <- verts.keywords$CIBSE
V(g.trim)$CIH           <- verts.keywords$CIH
V(g.trim)$construction  <- verts.keywords$construction
V(g.trim)$design        <- verts.keywords$design
V(g.trim)$doorsandwindows <- verts.keywords$doorsandwindows
V(g.trim)$eco             <- verts.keywords$eco
V(g.trim)$economy         <- verts.keywords$economy
V(g.trim)$efficiency      <- verts.keywords$efficiency
V(g.trim)$electric        <- verts.keywords$electric
V(g.trim)$energy          <- verts.keywords$energy
V(g.trim)$engineering     <- verts.keywords$engineering
V(g.trim)$environment     <- verts.keywords$environment
V(g.trim)$flood           <- verts.keywords$flood
V(g.trim)$floor           <- verts.keywords$floor
V(g.trim)$fuel            <- verts.keywords$fuel
V(g.trim)$gas             <- verts.keywords$gas
V(g.trim)$green           <- verts.keywords$green
V(g.trim)$heating         <- verts.keywords$heating
V(g.trim)$homeless        <- verts.keywords$homeless
V(g.trim)$house           <- verts.keywords$house
V(g.trim)$infrastructure  <- verts.keywords$infrastructure
V(g.trim)$installer       <- verts.keywords$installer
V(g.trim)$insulation      <- verts.keywords$insulation
V(g.trim)$JRF             <- verts.keywords$JRF
V(g.trim)$land            <- verts.keywords$land
V(g.trim)$landlord        <- verts.keywords$landlord
V(g.trim)$natfed          <- verts.keywords$natfed
V(g.trim)$neighbourhood   <- verts.keywords$neighbourhood
V(g.trim)$passivehouse    <- verts.keywords$passivehouse
V(g.trim)$plumbing        <- verts.keywords$plumbing
V(g.trim)$poverty         <- verts.keywords$poverty
V(g.trim)$property        <- verts.keywords$property
V(g.trim)$renewables      <- verts.keywords$renewables
V(g.trim)$rent            <- verts.keywords$rent
V(g.trim)$residential     <- verts.keywords$residential
V(g.trim)$retrofit        <- verts.keywords$retrofit
V(g.trim)$RIBA            <- verts.keywords$RIBA
V(g.trim)$RICS            <- verts.keywords$RICS
V(g.trim)$roof            <- verts.keywords$roof
V(g.trim)$skills          <- verts.keywords$skills
V(g.trim)$smart           <- verts.keywords$smart
V(g.trim)$surveying       <- verts.keywords$surveying
V(g.trim)$sustainability  <- verts.keywords$sustainability
V(g.trim)$taxandbenefits  <- verts.keywords$taxandbenefits
V(g.trim)$timber          <- verts.keywords$timber
V(g.trim)$trades          <- verts.keywords$trades
V(g.trim)$keywords.all    <- verts.keywords$keywords.all



map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}


pal <- colorRampPalette( c("red","yellow","springgreen","royalblue"))( 10 )
pal <- c("#D3D3D3",pal)


plot.keywords <- function(keyword,limit){
  png(filename=paste0("../twitter_plots/advanced/keyword_",keyword,".png"), width=4, height=4, units = 'in', res = 600, pointsize=4)   
  par(mar = c(0.01,0.01,0.01,0.01));   plot(g.plot,
                                            edge.width = E(g.plot)$weight/ 4000,
                                            vertex.size = ifelse(vertex_attr(graph = g.plot, name = keyword, index = V(g.plot)) < limit/11, 0 ,V(g.plot)$strength.total / 2000 ),
                                            edge.arrow.size = 0.001,
                                            edge.curved = 0.2,
                                            vertex.color = map2color( vertex_attr(graph = g.plot, name = keyword, index = V(g.plot)) , pal, c(0,limit)),
                                            vertex.label = NA,
                                            vertex.label.family= "Arial",
                                            vertex.label.color = "black",
                                            vertex.frame.color = map2color( vertex_attr(graph = g.plot, name = keyword, index = V(g.plot)) , pal, c(0,limit)),
                                            layout = layout.nosimmer, 
                                            rescale = T, 
                                            axes = F); dev.off()
}

g.plot <- delete.edges(g.trim, (which(E(g.trim)$weight < 200) ))

keyword.types <- keyword.types[!keyword.types %in% c("keywords.all")]

for(i in 1:length(keyword.types)){
  print(paste0(Sys.time()," ",keyword.types[i]))
  plot.keywords(keyword.types[i], 0.1)
}

plot.keywords("keywords.all", 1)
plot.keywords("affordable", 0.1)
