library(igraph)

g <- readRDS("../twitter_data/parRerun/MidDataColectionGraph.Rds")
gorder(g)

#Remove the Junk Accounts
common <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP3 Institutional incumbents/TwitterAccountsReview_withDescAnal_malcolm.xlsx", sheet =  "TwitterAccountsReview_withDescA")
common <- common[,c("name","Account Category","Account Type")]
common <- common[!is.na(common$`Account Category`),]
remove <- common$name[common$`Account Category` %in% c("Junk","Media","Government")]


gorder(g)
g.trim <- delete.vertices(g, which(V(g)$name %in% remove)) # Discard the junk
g.trim <- delete.vertices(g.trim, which(degree(g.trim) < 100)) # Trim down to a manageable amount
gorder(g.trim)


# Calcualte Values quick values
V(g.trim)$degree.total <- degree(g.trim, mode = "total")
V(g.trim)$degree.in <- degree(g.trim, mode = "in")
V(g.trim)$degree.out <- degree(g.trim, mode = "out")
V(g.trim)$strength.total <- strength(g.trim, mode = "total")
V(g.trim)$strength.in <- strength(g.trim, mode = "in")
V(g.trim)$strength.out <- strength(g.trim, mode = "out")


# Clacualte Slow values
V(g.trim)$closeness <- closeness(g.trim)
V(g.trim)$eigenvector <-  eigen_centrality(g.trim, directed = T, scale = F, weights = E(g.trim)$weight)$vector
V(g.trim)$PageRank <- page_rank(g.trim, directed = TRUE, damping = 0.85, weights = E(g.trim)$weight)$vector
V(g.trim)$between <- betweenness(g.trim)

#Produce Summary Table
verts <- igraph::as_data_frame(g.trim, what="vertices")

#clus = cluster_infomap(g.trim, e.weights = E(g.trim)$weight, modularity = FALSE, nb.trials = 3) # Used both directed and weight info
#clus = cluster_edge_betweenness(g.trim, weights = E(g.trim)$weight, directed = TRUE)
#https://bommaritollc.com/2012/06/17/summary-community-detection-algorithms-igraph-0-6/
clus = cluster_walktrap(g.trim, weights = E(g.trim)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
V(g.trim)$member <- membership(clus)

# Make a slimed down version for plotting

ecount(g.trim)
gorder(g.trim)
g.plot <- delete.vertices(g.trim, which(degree(g.trim) < 100))
g.plot <- delete.edges(g.plot, which(E(g.plot)$weight <10))
g.plot <- delete.vertices(g.plot, which(degree(g.plot) < 1))
ecount(g.plot)
gorder(g.plot)

#clus = cluster_edge_betweenness(g.plot, weights = E(g.plot)$weight, directed = TRUE)
#clus = cluster_walktrap(g.plot, weights = E(g.plot)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
#V(g.plot)$member <- membership(clus)

layout =  layout_with_drl(g.plot, weights = E(g.plot)$weight, options=list(simmer.attraction=0), dim = 2)

colours = sample ( rainbow ( max ( V(g.plot)$member, na.rm= T )  + 1) )
V(g.plot)$color = colours[V(g.plot)$member +1]


#svg(filename="../twitter_plots/MidParReRun-Clusters3.svg", width=30, height=15, pointsize=4)
jpeg(filename="../twitter_plots/MidParReRun-Clusters3.jpeg", width=15, height=15, units = 'in', res = 1200, pointsize=2)   
par(mar = c(0.01,0.01,0.01,0.01)); plot(g.plot,
     edge.width = E(g.plot)$weight/ 100,
     vertex.size = ifelse((V(g.plot)$strength.total / 2000) < 5, (V(g.plot)$strength.total / 2000) ,5 ),
     edge.arrow.size = 0.2,
     edge.curved=0.2,
     vertex.color = V(g.plot)$color,
     vertex.label = ifelse(V(g.plot)$between > 10000, V(g.plot)$name, NA),
     vertex.label.family= "Arial",
     vertex.label.color = "black",
     vertex.frame.color = V(g.plot)$color,
     layout = layout, 
     rescale = T, 
     axes = F); dev.off()

