g <- readRDS("twitter/data/parRerun/MidDataColectionGraph.Rds")
gorder(g)

#Remove the Junk Accounts
common <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP3 Institutional incumbents/TwitterAccountsReview_withDescAnal_malcolm.xlsx", sheet =  "TwitterAccountsReview_withDescA")
common <- common[,c("name","Account Category","Account Type")]
common <- common[!is.na(common$`Account Category`),]
remove <- common$name[common$`Account Category` %in% c("Junk","Media")]


g.trim <- delete.vertices(g, which(V(g.trim)$name %in% remove)) # Discard the junk
g.trim <- delete.vertices(g, which(degree(g.trim) < 1000)) # Discard the junk
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

g.test <- delete.vertices(g.trim, which(V(g.trim)$degree.out == 0))
gorder(g.test)
g.test <- delete.vertices(g.test, which(V(g.test)$degree.total < 2000 ))
gorder(g.test)


clus = cluster_walktrap(g.test, weights = E(g.test)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
V(g.test)$mebership <- membership(clus)
layout =  layout_with_drl(g.test, weights = E(g.test)$weight, options=list(simmer.attraction=0), dim = 2)

colours = sample ( rainbow ( max ( V(g.test)$mebership )  + 1) )
V(g.test)$color2 = colours[V(g.test)$mebership +1]

ecount(g.test)
g.plot <- delete.edges(g.test, which(E(g.test)$weight <5))
ecount(g.plot)

svg(filename="MidParReRun-Clusters.svg", 
    width=30, 
    height=15, 
    pointsize=8)
par(mar = c(1,1,1,1))
plot(g.plot, 
     edge.width = E(g.plot)$weight/ 50,
     vertex.size = V(g.plot)$strength.in / 2000 ,
     edge.arrow.size = 0.2,
     edge.curved=0.2,
     vertex.color = V(g.plot)$color2,
     vertex.label = ifelse(V(g.plot)$strength.in > 2000, V(g.plot)$name, NA),
     vertex.label.family= "Arial Bold",
     vertex.label.color = "black",
     vertex.frame.color = V(g.plot)$color2,
     layout = layout, 
     rescale = T, 
     axes = F)
dev.off()
