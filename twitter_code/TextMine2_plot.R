# Plot Graphs new Textmin data
library(igraph)

conn.simple <- readRDS("../twitter_data/all/conn_simple_types_textanal.Rds")
conn.simple$friends <- as.numeric(conn.simple$friends)

g <- graph_from_data_frame(conn.simple, directed = T) #Make the graph
g <- simplify(g, remove.loops = T, remove.multiple = F) #Remove self references
gorder(g)
g <- delete.vertices(g, which(degree(g, mode = "out") == 0)) # DO several time as first removal may create new ones
g <- delete.vertices(g, which(degree(g, mode = "out") == 0))
g <- delete.vertices(g, which(degree(g, mode = "out") == 0))
gorder(g)

#g.trim <- delete.vertices(g, which(degree(g) < 200))
g.trim <- g
gorder(g.trim)

#g.trim <- as.undirected(g.trim, mode = "collapse")

layout =  layout_with_drl(g.trim, weights = E(g.trim)$weight, options=list(simmer.attraction=0.001), dim = 2)
layout.nosimmer =  layout_with_drl(g.trim, weights = E(g.trim)$weight, options=list(simmer.attraction=0), dim = 2)

ecount(g.trim)
g.friends <- delete.edges(g.trim, which(E(g.trim)$friends == 0))
ecount(g.friends)
g.likes <- delete.edges(g.trim, which(E(g.trim)$likes == 0))
ecount(g.likes)
g.mentions <- delete.edges(g.trim, which(E(g.trim)$mentions == 0))
ecount(g.mentions)
g.retweets <- delete.edges(g.trim, which(E(g.trim)$retweets == 0))
ecount(g.retweets)
g.weight <- delete.edges(g.trim, which(E(g.trim)$weight == 0))
ecount(g.weight)

clus.friends = cluster_infomap(g.friends, e.weights = E(g.friends)$friends, nb.trials = 20)
clus.likes = cluster_infomap(g.likes, e.weights = E(g.likes)$likes, nb.trials = 20)
clus.mentions = cluster_infomap(g.mentions, e.weights = E(g.mentions)$mentions, nb.trials = 20)
clus.retweets = cluster_infomap(g.retweets, e.weights = E(g.retweets)$retweets, nb.trials = 20)
clus.weight = cluster_infomap(g.weight, e.weights = E(g.weight)$weight, nb.trials = 20)

V(g.friends)$clus.friends <- membership(clus.friends)
V(g.likes)$clus.likes <- membership(clus.likes)
V(g.mentions)$clus.mentions <- membership(clus.mentions)
V(g.retweets)$clus.retweets <- membership(clus.retweets)
V(g.weight)$clus.weight <- membership(clus.weight)

colours.friends = sample ( rainbow ( max ( V(g.friends)$clus.friends, na.rm= T )  + 1) )
V(g.friends)$colours.friends = colours.friends[V(g.friends)$clus.friends +1]

colours.likes = sample ( rainbow ( max ( V(g.likes)$clus.likes, na.rm= T )  + 1) )
V(g.likes)$colours.likes = colours.likes[V(g.likes)$clus.likes +1]

colours.mentions = sample ( rainbow ( max ( V(g.mentions)$clus.mentions, na.rm= T )  + 1) )
V(g.mentions)$colours.mentions = colours.mentions[V(g.mentions)$clus.mentions +1]

colours.retweets = sample ( rainbow ( max ( V(g.retweets)$clus.retweets, na.rm= T )  + 1) )
V(g.retweets)$colours.retweets = colours.retweets[V(g.retweets)$clus.retweets +1]

colours.weight = sample ( rainbow ( max ( V(g.weight)$clus.weight, na.rm= T )  + 1) )
V(g.weight)$colours.weight = colours.weight[V(g.weight)$clus.weight +1]

V(g.weight)$strength.weight <- strength(g.weight, mode = "total" , weights = E(g.weight)$weight)
V(g.friends)$strength.friends <- strength(g.friends, mode = "total" , weights = E(g.friends)$friends)
V(g.mentions)$strength.mentions <- strength(g.mentions, mode = "total" , weights = E(g.mentions)$mentions)
V(g.retweets)$strength.retweets <- strength(g.retweets, mode = "total" , weights = E(g.retweets)$retweets)
V(g.likes)$strength.likes <- strength(g.likes, mode = "total" , weights = E(g.likes)$likes)


png(filename=paste0("../twitter_plots/connect_types/","weight_nosimmer",".png"), width=10, height=10, units = 'in', res = 600, pointsize=4)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.weight,
                                            edge.width = E(g.weight)$weight/ 500,
                                            vertex.size = ifelse((V(g.weight)$strength.weight / 2000) < 5, (V(g.weight)$strength.weight / 2000) ,5 ),
                                            edge.arrow.size = 0.2,
                                            edge.curved=0.2,
                                            vertex.color = V(g.weight)$colours.weight,
                                            vertex.label = ifelse(V(g.weight)$strength.weight > 5000, V(g.weight)$name, NA),
                                            vertex.label.family= "Arial",
                                            vertex.label.color = "black",
                                            vertex.frame.color = V(g.weight)$colours.weight,
                                            layout = layout.nosimmer, 
                                            rescale = T, 
                                            axes = F); dev.off()


png(filename=paste0("../twitter_plots/connect_types/","friends_nosimmer",".png"), width=10, height=10, units = 'in', res = 600, pointsize=4)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.friends,
                                          edge.width = E(g.friends)$friends/ 500,
                                          vertex.size = ifelse((V(g.friends)$strength.friends / 2000) < 5, (V(g.friends)$strength.friends / 2000) ,5 ),
                                          edge.arrow.size = 0.2,
                                          edge.curved=0.2,
                                          vertex.color = V(g.friends)$colours.friends,
                                          vertex.label = ifelse(V(g.friends)$strength.friends > 5000, V(g.friends)$name, NA),
                                          vertex.label.family= "Arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = V(g.friends)$colours.friends,
                                          layout = layout.nosimmer, 
                                          rescale = T, 
                                          axes = F); dev.off()

png(filename=paste0("../twitter_plots/connect_types/","likes_nosimmer",".png"), width=10, height=10, units = 'in', res = 600, pointsize=4)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.likes,
                                          edge.width = E(g.likes)$likes/ 500,
                                          vertex.size = ifelse((V(g.likes)$strength.likes / 2000) < 5, (V(g.likes)$strength.likes / 2000) ,5 ),
                                          edge.arrow.size = 0.2,
                                          edge.curved=0.2,
                                          vertex.color = V(g.likes)$colours.likes,
                                          vertex.label = ifelse(V(g.likes)$strength.likes > 5000, V(g.likes)$name, NA),
                                          vertex.label.family= "Arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = V(g.likes)$colours.likes,
                                          layout = layout.nosimmer, 
                                          rescale = T, 
                                          axes = F); dev.off()

png(filename=paste0("../twitter_plots/connect_types/","retweets_nosimmer",".png"), width=10, height=10, units = 'in', res = 600, pointsize=4)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.retweets,
                                          edge.width = E(g.retweets)$retweets/ 500,
                                          vertex.size = ifelse((V(g.retweets)$strength.retweets / 2000) < 5, (V(g.retweets)$strength.retweets / 2000) ,5 ),
                                          edge.arrow.size = 0.2,
                                          edge.curved=0.2,
                                          vertex.color = V(g.retweets)$colours.retweets,
                                          vertex.label = ifelse(V(g.retweets)$strength.retweets > 5000, V(g.retweets)$name, NA),
                                          vertex.label.family= "Arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = V(g.retweets)$colours.retweets,
                                          layout = layout.nosimmer, 
                                          rescale = T, 
                                          axes = F); dev.off()


png(filename=paste0("../twitter_plots/connect_types/","mentions_nosimmer",".png"), width=10, height=10, units = 'in', res = 600, pointsize=4)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.mentions,
                                          edge.width = E(g.mentions)$mentions/ 500,
                                          vertex.size = ifelse((V(g.mentions)$strength.mentions / 2000) < 5, (V(g.mentions)$strength.mentions / 2000) ,5 ),
                                          edge.arrow.size = 0.2,
                                          edge.curved=0.2,
                                          vertex.color = V(g.mentions)$colours.mentions,
                                          vertex.label = ifelse(V(g.mentions)$strength.mentions > 5000, V(g.mentions)$name, NA),
                                          vertex.label.family= "Arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = V(g.mentions)$colours.mentions,
                                          layout = layout.nosimmer, 
                                          rescale = T, 
                                          axes = F); dev.off()



stop()


# Clacualte Slow values
#V(g.trim)$closeness <- closeness(g.trim)
#V(g.trim)$eigenvector <-  eigen_centrality(g.trim, directed = T, scale = F, weights = E(g.trim)$weight)$vector
#V(g.trim)$PageRank <- page_rank(g.trim, directed = TRUE, damping = 0.85, weights = E(g.trim)$weight)$vector
#V(g.trim)$between <- betweenness(g.trim)

#clus.info = cluster_infomap(g.trim, e.weights = E(g.trim)$weight, modularity = FALSE, nb.trials = 3) # Used both directed and weight info
#clus.between = cluster_edge_betweenness(g.trim, weights = E(g.trim)$weight, directed = TRUE)
#https://bommaritollc.com/2012/06/17/summary-community-detection-algorithms-igraph-0-6/
#clus.walk = cluster_walktrap(g.trim, weights = E(g.trim)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)



g.trim <- as.undirected(g.trim, mode = "collapse")
#clus.louvain = cluster_louvain(g.trim, weights = E(g.trim)$weight)
clus.greedy = cluster_fast_greedy(g.trim, weights = E(g.trim)$weight)

#V(g.trim)$member.info <- membership(clus.info)
#V(g.trim)$member.between <- membership(clus.between)
#V(g.trim)$member.walk <- membership(clus.walk)
#V(g.trim)$member.louvain <- membership(clus.louvain)
V(g.trim)$member.greedy <- membership(clus.greedy)

#Produce Summary Table
verts <- igraph::as_data_frame(g.trim, what="vertices")

# Make a slimed down version for plotting
ecount(g.trim)
gorder(g.trim)
g.plot <- delete.vertices(g.trim, which(degree(g.trim) < 100))
g.plot <- delete.edges(g.plot, which(E(g.plot)$weight <10))
g.plot <- delete.vertices(g.plot, which(degree(g.plot) < 1))
ecount(g.plot)
gorder(g.plot)


layout =  layout_with_drl(g.plot, weights = E(g.plot)$weight, options=list(simmer.attraction=0.001), dim = 2)

colours = sample ( rainbow ( max ( V(g.plot)$member.greedy, na.rm= T )  + 1) )
barplot(rep(1,length(colours)), col= colours)
V(g.plot)$color = colours[V(g.plot)$member.greedy +1]


#svg(filename="../twitter_plots/MidParReRun-Clusters3.svg", width=30, height=15, pointsize=4)
png(filename="../twitter_plots/All_textmine_greedy.png", width=15, height=15, units = 'in', res = 1200, pointsize=2)   
par(mar = c(0.01,0.01,0.01,0.01)); plot(g.plot,
                                        edge.width = E(g.plot)$weight/ 100,
                                        vertex.size = ifelse((V(g.plot)$strength.total / 2000) < 5, (V(g.plot)$strength.total / 2000) ,5 ),
                                        edge.arrow.size = 0.2,
                                        edge.curved=0.2,
                                        vertex.color = V(g.plot)$color,
                                        vertex.label = ifelse(V(g.plot)$strength.total > 5000, V(g.plot)$name, NA),
                                        vertex.label.family= "Arial",
                                        vertex.label.color = "black",
                                        vertex.frame.color = V(g.plot)$color,
                                        layout = layout, 
                                        rescale = T, 
                                        axes = F); dev.off()



#Differetn Graphs for each type of connections
conn.metions <- conn.simple[,c("from","to","mentions")]
names(conn.metions) <- c("from","to","weight")

g.mentions <- graph_from_data_frame(conn.metions, directed = T) #Make the graph
g.mentions <- simplify(g.mentions, remove.loops = T) #Remove self references
gorder(g.retweet)


V(g.mentions)$degree.in <- degree(g.mentions, mode = "in")
V(g.mentions)$degree.out <- degree(g.mentions, mode = "out")

gorder(g.mentions)
g.trim <- delete.vertices(g.mentions, which(V(g.mentions)$degree.out == 0))
gorder(g.trim)

# look for only mutual edges (i.e. from A to B and B to A)
ecount(g.trim)
g.trim <- delete.edges(g.trim, (1:ecount(g.trim))[!which_mutual(g.trim, es = E(g.trim))] ) 
ecount(g.trim)
g.trim <- delete.edges(g.trim, (which(E(g.trim)$weight < 2) ))
ecount(g.trim)
g.trim <- delete.edges(g.trim, (1:ecount(g.trim))[!which_mutual(g.trim, es = E(g.trim))] ) 
ecount(g.trim)
edges <- igraph::as_data_frame(g.trim, "edges")



V(g.trim)$degree.total <- degree(g.trim, mode = "total")
V(g.trim)$degree.in <- degree(g.trim, mode = "in")
V(g.trim)$degree.out <- degree(g.trim, mode = "out")
V(g.trim)$strength.total <- strength(g.trim, mode = "total")
V(g.trim)$strength.in <- strength(g.trim, mode = "in")
V(g.trim)$strength.out <- strength(g.trim, mode = "out")



#g.trim <- as.undirected(g.trim, mode = "collapse")
#clus.louvain = cluster_louvain(g.trim, weights = E(g.trim)$weight)
#clus.greedy = cluster_fast_greedy(g.trim, weights = E(g.trim)$weight)
clus.between = cluster_edge_betweenness(g.trim, weights = E(g.trim)$weight, directed = T)

#V(g.trim)$member.info <- membership(clus.info)
V(g.trim)$member.between <- membership(clus.between)
#V(g.trim)$member.walk <- membership(clus.walk)
#V(g.trim)$member.louvain <- membership(clus.louvain)
#V(g.trim)$member.greedy <- membership(clus.greedy)

#Produce Summary Table
verts <- igraph::as_data_frame(g.trim, what="vertices")

# Make a slimed down version for plotting
ecount(g.trim)
gorder(g.trim)
#g.plot <- g.trim
g.plot <- delete.vertices(g.trim, which(degree(g.trim) < 1))
#g.plot <- delete.edges(g.plot, which(E(g.plot)$weight <10))
#g.plot <- delete.vertices(g.plot, which(degree(g.plot) < 1))
ecount(g.plot)
gorder(g.plot)


layout =  layout_with_drl(g.plot, weights = E(g.plot)$weight, options=list(simmer.attraction=0.001), dim = 2)

clus_summ <- as.data.frame(table(V(g.plot)$member.between))
clus_summ <- clus_summ[order(-clus_summ$Freq),]
cols <- c("red","dodgerblue","gold","violet","green4","wheat","firebrick","darkorange","greenyellow","lightblue","darkorchid")
cols <- c(cols,rep("black",nrow(clus_summ)-length(cols)))
clus_summ$colour <- cols

V(g.plot)$colour <- "white"
for(i in 1:gorder(g.plot)){
  V(g.plot)$colour[i] <- clus_summ$colour[clus_summ$Var1 == V(g.plot)$member.between[i] ]
}
summary(as.factor(V(g.plot)$colour))





#colours = sample ( rainbow ( max ( V(g.plot)$member.greedy, na.rm= T )  + 1) )
#barplot(rep(1,length(colours)), col= colours)
#V(g.plot)$color = colours[V(g.plot)$member.greedy +1]

labes = sapply(1:gorder(g.plot),
               function(i){
                 if(V(g.plot)$strength.total[i] > 
                    quantile(V(g.plot)$strength.total[V(g.plot)$member.between == V(g.plot)$member.between[i]], probs = 0.9)){
                   return(V(g.plot)$name[i])
                 }else{
                   return(NA)
                 }
               }
)

V(g.plot)$label = labes

#svg(filename="../twitter_plots/MidParReRun-Clusters3.svg", width=30, height=15, pointsize=4)
png(filename="../twitter_plots/reciprocalmentions_textmine_between.png", width=15, height=15, units = 'in', res = 1200, pointsize=2)   
par(mar = c(0.01,0.01,0.01,0.01)); plot(g.plot,
                                        edge.width = E(g.plot)$weight/ 100,
                                        vertex.size = ifelse((V(g.plot)$strength.total / 2000) < 5, (V(g.plot)$strength.total / 2000) ,5 ),
                                        edge.arrow.size = 0.2,
                                        edge.curved=0.2,
                                        vertex.color = V(g.plot)$colour,
                                        vertex.label = V(g.plot)$label,
                                        vertex.label.family= "Arial",
                                        vertex.label.color = "black",
                                        vertex.frame.color = V(g.plot)$colour,
                                        layout = layout, 
                                        rescale = T, 
                                        axes = F); dev.off()

cluster.summ <- as.data.frame(table(verts$member.greedy))
cluster.summ <- cluster.summ[order(cluster.summ$Freq , decreasing = T),]

# summarise  the top clusters
for(i in 1:30){
  clusno = cluster.summ$Var1[i]
  
  verts.tmp <- verts[verts$member.greedy == clusno,]
  verts.tmp <- verts.tmp[order(verts.tmp$strength.total , decreasing = T),]
  message(paste0("Doing Cluster ",clusno," rank is ",i," order is ",nrow(verts.tmp)," colour is ",colours[clusno]))
  print(verts.tmp$name[1:5])
}


