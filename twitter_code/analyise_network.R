library(igraph)
library(dplyr)

g <- readRDS("../twitter_data/parRerun/All_Data.Rds")
gorder(g)

#Remove the Junk Accounts
common <- readxl::read_xlsx("../twitter_data/accounts_fulldata_trim.xlsx", sheet =  "accounts_fulldata")
common <- common[,c("name","Account Category","Account Type")]
common <- common[!is.na(common$`Account Category`),]
remove <- common$name[common$`Account Category` %in% c("Junk","Media","Government")]
length(remove)

#Add on account types

verts.all <- igraph::as_data_frame(g, what="vertices")
verts.all <- left_join(verts.all, common, by = c("name" = "name"))
V(g)$type <- verts.all$`Account Category`
rm(verts.all, common)

g.trim <- delete.vertices(g, which(V(g)$name %in% remove)) # Discard the junk
summary(degree(g))
g.trim <- delete.vertices(g.trim, which(degree(g.trim) < 200)) # Trim down to a manageable amount

message(paste0("Trimming removed ",100 - round(gorder(g.trim)/gorder(g)*100,2),"% of accounts and ",100 - round(ecount(g.trim)/ecount(g)*100,2),"% of connections" ))


# Calcualte Values quick values
V(g.trim)$degree.total <- degree(g.trim, mode = "total")
V(g.trim)$degree.in <- degree(g.trim, mode = "in")
V(g.trim)$degree.out <- degree(g.trim, mode = "out")
V(g.trim)$strength.total <- strength(g.trim, mode = "total")
V(g.trim)$strength.in <- strength(g.trim, mode = "in")
V(g.trim)$strength.out <- strength(g.trim, mode = "out")


# Clacualte Slow values
#V(g.trim)$closeness <- closeness(g.trim)
#V(g.trim)$eigenvector <-  eigen_centrality(g.trim, directed = T, scale = F, weights = E(g.trim)$weight)$vector
#V(g.trim)$PageRank <- page_rank(g.trim, directed = TRUE, damping = 0.85, weights = E(g.trim)$weight)$vector
V(g.trim)$between <- betweenness(g.trim)

clus.info = cluster_infomap(g.trim, e.weights = E(g.trim)$weight, modularity = FALSE, nb.trials = 3) # Used both directed and weight info
clus.between = cluster_edge_betweenness(g.trim, weights = E(g.trim)$weight, directed = TRUE)
#https://bommaritollc.com/2012/06/17/summary-community-detection-algorithms-igraph-0-6/
clus.walk = cluster_walktrap(g.trim, weights = E(g.trim)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)


V(g.trim)$member.info <- membership(clus.info)
V(g.trim)$member.between <- membership(clus.between)
V(g.trim)$member.walk <- membership(clus.walk)

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


layout =  layout_with_drl(g.plot, weights = E(g.plot)$weight, options=list(simmer.attraction=0), dim = 2)

colours = sample ( rainbow ( max ( V(g.plot)$member.between, na.rm= T )  + 1) )
barplot(rep(1,length(colours)), col= colours)
V(g.plot)$color = colours[V(g.plot)$member.between +1]


#svg(filename="../twitter_plots/MidParReRun-Clusters3.svg", width=30, height=15, pointsize=4)
png(filename="../twitter_plots/All_removenewJunk.png", width=15, height=15, units = 'in', res = 1200, pointsize=2)   
par(mar = c(0.01,0.01,0.01,0.01)); plot(g.plot,
     edge.width = E(g.plot)$weight/ 100,
     vertex.size = ifelse((V(g.plot)$strength.total / 2000) < 5, (V(g.plot)$strength.total / 2000) ,5 ),
     edge.arrow.size = 0.2,
     edge.curved=0.2,
     vertex.color = V(g.plot)$color,
     vertex.label = ifelse(V(g.plot)$between > 100000, V(g.plot)$name, NA),
     vertex.label.family= "Arial",
     vertex.label.color = "black",
     vertex.frame.color = V(g.plot)$color,
     layout = layout, 
     rescale = T, 
     axes = F); dev.off()



# summarise the clusters
clus_summ <- as.data.frame(table(V(g.trim)$member))
clus_summ <- clus_summ[order(-clus_summ$Freq),]


g.clus1 <- delete.vertices(g.trim, which(V(g.trim)$member != clus_summ$Var1[1]))
gorder(g.clus1)
layout.clus1 =  layout_with_drl(g.clus1, weights = E(g.clus1)$weight, options=list(simmer.attraction=0), dim = 2)
#colours.clus1 = sample ( rainbow ( max ( V(g.clus1)$member, na.rm= T )  + 1) )
#V(g.clus1)$color = colours[V(g.clus1)$member +1]


png(filename="../twitter_plots/All_LargestCluster.png", width=15, height=15, units = 'in', res = 1200, pointsize=2)   
par(mar = c(0.01,0.01,0.01,0.01)); plot(g.clus1,
                                        edge.width = E(g.clus1)$weight/ 100,
                                        vertex.size = ifelse((V(g.clus1)$strength.total / 2000) < 5, (V(g.clus1)$strength.total / 2000) ,5 ),
                                        edge.arrow.size = 0.2,
                                        edge.curved=0.2,
                                        vertex.color = V(g.clus1)$color,
                                        vertex.label = ifelse(V(g.clus1)$between > 10000, V(g.clus1)$name, NA),
                                        vertex.label.family= "Arial",
                                        vertex.label.color = "black",
                                        vertex.frame.color = V(g.clus1)$color,
                                        layout = layout.clus1, 
                                        rescale = T, 
                                        axes = F); dev.off()


verts.cluster <- igraph::as_data_frame(g.clus1, what="vertices")



g.clus2 <- delete.vertices(g.trim, which(V(g.trim)$member != clus_summ$Var1[2]))
gorder(g.clus2)
layout.clus2 =  layout_with_drl(g.clus2, weights = E(g.clus2)$weight, options=list(simmer.attraction=0), dim = 2)
#colours.clus2 = sample ( rainbow ( max ( V(g.clus2)$member, na.rm= T )  + 1) )
#V(g.clus2)$color = colours[V(g.clus2)$member +1]


png(filename="../twitter_plots/All_2ndLargestCluster.png", width=15, height=15, units = 'in', res = 1200, pointsize=2)   
par(mar = c(0.01,0.01,0.01,0.01)); plot(g.clus2,
                                        edge.width = E(g.clus2)$weight/ 100,
                                        vertex.size = ifelse((V(g.clus2)$strength.total / 2000) < 5, (V(g.clus2)$strength.total / 2000) ,5 ),
                                        edge.arrow.size = 0.2,
                                        edge.curved=0.2,
                                        vertex.color = V(g.clus2)$color,
                                        vertex.label = ifelse(V(g.clus2)$between > 10000, V(g.clus2)$name, NA),
                                        vertex.label.family= "Arial",
                                        vertex.label.color = "black",
                                        vertex.frame.color = V(g.clus2)$color,
                                        layout = layout.clus2, 
                                        rescale = T, 
                                        axes = F); dev.off()


verts.cluster2 <- igraph::as_data_frame(g.clus2, what="vertices")


#Have a look at the non construction accounts
common <- readxl::read_xlsx("../twitter_data/accounts_fulldata_trim.xlsx", sheet =  "accounts_fulldata")
common <- common[,c("name","Account Category","Account Type")]
common <- common[!is.na(common$`Account Category`),]
remove2 <- common$name[common$`Account Category` %in% c("Junk","Media","Government","Keep")]

g.nokeep <- delete.vertices(g, which(V(g)$name %in% remove2)) # Discard the junk and the keeps
g.nokeep <- delete.vertices(g.nokeep, which(degree(g.nokeep) < 200)) # Trim down to a manageable amount

message(paste0("Trimming removed ",100 - round(gorder(g.nokeep)/gorder(g)*100,2),"% of accounts and ",100 - round(ecount(g.nokeep)/ecount(g)*100,2),"% of connections" ))


# Calcualte Values quick values
#V(g.nokeep)$degree.total <- degree(g.nokeep, mode = "total")
#V(g.nokeep)$degree.in <- degree(g.nokeep, mode = "in")
#V(g.nokeep)$degree.out <- degree(g.nokeep, mode = "out")
V(g.nokeep)$strength.total <- strength(g.nokeep, mode = "total")
#V(g.nokeep)$strength.in <- strength(g.nokeep, mode = "in")
#V(g.nokeep)$strength.out <- strength(g.nokeep, mode = "out")


# Clacualte Slow values
#V(g.nokeep)$closeness <- closeness(g.nokeep)
#V(g.nokeep)$eigenvector <-  eigen_centrality(g.nokeep, directed = T, scale = F, weights = E(g.nokeep)$weight)$vector
#V(g.nokeep)$PageRank <- page_rank(g.nokeep, directed = TRUE, damping = 0.85, weights = E(g.nokeep)$weight)$vector
V(g.nokeep)$between <- betweenness(g.nokeep)
V(g.nokeep)$between01 <- V(g.nokeep)$between / max(V(g.nokeep)$between)
#V(g.nokeep)$alpha <- alpha_centrality(g.nokeep, nodes = V(g.nokeep), alpha = 1, loops = FALSE, exo = 1, weights = E(g.nokeep)$weight, tol = 1e-07, sparse = TRUE)$vector
#V(g.nokeep)$hub <- hub_score(g.nokeep, scale = TRUE, weights = E(g.nokeep)$weight, options = arpack_defaults)$vector


#clus = cluster_infomap(g.nokeep, e.weights = E(g.nokeep)$weight, modularity = FALSE, nb.trials = 3) # Used both directed and weight info
#clus = cluster_edge_betweenness(g.nokeep, weights = E(g.nokeep)$weight, directed = TRUE)
#https://bommaritollc.com/2012/06/17/summary-community-detection-algorithms-igraph-0-6/
#clus = cluster_walktrap(g.nokeep, weights = E(g.nokeep)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
#V(g.nokeep)$member <- membership(clus)


#Produce Summary Table
verts.nokeep <- igraph::as_data_frame(g.nokeep, what="vertices")



# Make a slimed down version for plotting

ecount(g.nokeep)
gorder(g.nokeep)
g.nokeep <- delete.vertices(g.nokeep, which(degree(g.nokeep) < 100))
g.nokeep <- delete.edges(g.nokeep, which(E(g.nokeep)$weight <10))
g.nokeep <- delete.vertices(g.nokeep, which(degree(g.nokeep) < 1))
ecount(g.nokeep)
gorder(g.nokeep)

#clus = cluster_edge_betweenness(g.plot, weights = E(g.plot)$weight, directed = TRUE)
#clus = cluster_walktrap(g.plot, weights = E(g.plot)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
#V(g.plot)$member <- membership(clus)

layout.nokeep =  layout_with_drl(g.nokeep, weights = E(g.nokeep)$weight, options=list(simmer.attraction=0), dim = 2)

#colours = sample ( rainbow ( max ( V(g.nokeep)$between, na.rm= T )  + 1) )
#(rep(1,length(colours)), col= colours)
#V(g.nokeep)$color = colours[V(g.plot)$member +1]
#Color scaling function
c_scale <- colorRamp(c('white','red','yellow','cyan','blue'))
V(g.nokeep)$color = apply(c_scale(V(g.nokeep)$between01), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )

#svg(filename="../twitter_plots/MidParReRun-Clusters3.svg", width=30, height=15, pointsize=4)
png(filename="../twitter_plots/NonConstructionAccounts.png", width=15, height=15, units = 'in', res = 1200, pointsize=2)   
par(mar = c(0.01,0.01,0.01,0.01)); plot(g.nokeep,
                                        edge.width = E(g.nokeep)$weight/ 100,
                                        vertex.size = V(g.nokeep)$strength.total / 2000 ,
                                        edge.arrow.size = 0.2,
                                        edge.curved=0.2,
                                        vertex.color = V(g.nokeep)$color,
                                        vertex.label = ifelse(V(g.nokeep)$between01 > 0.6, V(g.nokeep)$name, NA),
                                        vertex.label.family= "Arial",
                                        vertex.label.color = "black",
                                        vertex.frame.color = V(g.nokeep)$color,
                                        layout = layout.nokeep, 
                                        rescale = T, 
                                        axes = F); dev.off()
# this is needed for some reason
dev.off()
