#test Plotting
library(igraph)

conn.simple <- readRDS("../twitter_data/all/conn_simple_types_textanal.Rds")

g <- graph_from_data_frame(conn.simple, directed = T) #Make the graph
g <- simplify(g, remove.loops = T) #Remove

# look for only mutual edges (i.e. from A to B and B to A)
g <- delete.edges(g, (1:ecount(g))[!which_mutual(g, es = E(g))] ) 
g <- delete.edges(g, (which(E(g)$weight < 2) ))
g <- delete.edges(g, (1:ecount(g))[!which_mutual(g, es = E(g))] ) 
g <- delete.vertices(g, which(degree(g) < 20))
g <- delete.vertices(g, which(degree(g) < 1))
g <- delete.vertices(g, which(degree(g) < 1))
summary(degree(g))
gorder(g)

V(g)$strength.total <- strength(g, mode = "total")
clus = cluster_walktrap(g, weights = E(g)$weight)
V(g)$member <- membership(clus)

colours = sample ( rainbow ( max ( V(g)$member, na.rm= T )  + 1) )
barplot(rep(1,length(colours)), col= colours)
V(g)$color = colours[V(g)$member +1]

plotgraph <-function(name = "", layoutname = NA){
  png(filename=paste0("../twitter_plots/layouts/",name,".png"), width=10, height=10, units = 'in', res = 600, pointsize=4)   
  par(mar = c(0.01,0.01,0.01,0.01));   plot(g,
                                            edge.width = E(g)$weight/ 500,
                                            vertex.size = ifelse((V(g)$strength.total / 2000) < 5, (V(g)$strength.total / 2000) ,5 ),
                                            edge.arrow.size = 0.2,
                                            edge.curved=0.2,
                                            vertex.color = V(g)$color,
                                            vertex.label = ifelse(V(g)$strength.total > 5000, V(g)$name, NA),
                                            vertex.label.family= "Arial",
                                            vertex.label.color = "black",
                                            vertex.frame.color = V(g)$color,
                                            layout = layoutname, 
                                            rescale = T, 
                                            axes = F); dev.off()
}



layout_drl <- layout_with_drl(g, options=list(simmer.attraction=0)) # O(|V|^2) # best layoout
plotgraph(name = "drl", layoutname = layout_drl)

layout_fr <- layout_with_fr(g, grid = "grid", weights = E(g)$weight) # few seconds
plotgraph(name = "fr", layoutname = layout_fr)

layout_lgl <- layout_with_lgl(g) # O(dia*maxit*(|V|+|E|)) #alterative layout that is usefule
plotgraph(name = "lgl", layoutname = layout_lgl)



layout_kk <- layout_with_kk(g) #bad layout shoing no pattern
plotgraph(name = "kk", layoutname = layout_kk)

layout_grap <- layout_with_graphopt(g) # O(n (|V|^2+|E|) ) # bad layout shoing not pattern
plotgraph(name = "grap", layoutname = layout_grap)

layout_mds <- layout_with_mds(g) # O(|V|^2 dim). #alterative laough showinng some clusters
plotgraph(name = "mds", layoutname = layout_mds)

#layout_dh <- layout_with_dh(g) #run for days no good
#plotgraph(name = "dh", layoutname = layout_dh)

layout_gem <- layout_with_gem(g) #takes a long time # bad layout shoing no pattern
plotgraph(name = "gem", layoutname = layout_gem)

#layout_sig <- layout_with_sugiyama(g) #crashes
#plotgraph(name = "sig", layoutname = layout_sig)



