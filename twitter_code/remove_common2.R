# Remove Common Accoutns
#Usingin detaield post workshop data

# Post WP3 workshop work
library(igraph)

# Read in the data
conn.simple <- readRDS("twitter/conn-simple-summyprogress2.Rds")
common <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP3 Institutional incumbents/TwitterAccountsReview_withDescAnal_malcolm.xlsx", sheet =  "TwitterAccountsReview_withDescA")
common <- common[,c("name","Account Category","Account Type")]
common.withCat <- common[!is.na(common$`Account Category`),]

common.junk <- common.withCat[common.withCat$`Account Category` == "Junk",]
common.media <- common.withCat[common.withCat$`Account Category` == "Media",]
common.gov <- common.withCat[common.withCat$`Account Category` == "Government",]

# Make Graph
g <- graph_from_data_frame(conn.simple, directed = T) #Make the graph
g <- simplify(g, remove.loops = T) #Remove self references
gorder(g)

#Remove the known Junk
g.nojunk <- delete.vertices(g, V(g)$name %in% common.junk$name)
gorder(g.nojunk)
ecount(g.nojunk)

#Remove the Media
g.nomedia <- delete.vertices(g.nojunk, V(g.nojunk)$name %in% common.media$name)
gorder(g.nomedia)
ecount(g.nomedia)

#Remove Government and politics
g.nogov <- delete.vertices(g.nomedia, V(g.nomedia)$name %in% common.gov$name)
gorder(g.nogov)
ecount(g.nogov)


#Remove Low Degree Values
#g.trim <- g.nogov
g.trim <- delete.edges(g.nogov, which(E(g.nogov)$weight<=2))
ecount(g.trim)
g.trim <- delete.vertices(g.trim, which(degree(g.trim)<=1))
gorder(g.trim)
ecount(g.trim)



#g.trim <- delete.vertices(g.nogov, which(degree(g.nogov)<=10))
#gorder(g.trim)


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

#Try removing the edges of low strenght

g.test <- delete.vertices(g.trim, which(V(g.trim)$strength.in < 300))
gorder(g.test)
g.test <- delete.vertices(g.test, which(degree(g.test)<=1))
gorder(g.test)


#Applying the color scale to edge weights.
#rgb method is to convert colors to a character vector.
c_scale <- colorRamp(c('blue','cyan','yellow','red'))
V(g.test)$color = apply(c_scale(V(g.test)$between), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )

#Create High Resolution Plots
svg(filename="removejunkmediagov.svg", 
    width=25, 
    height=20, 
    pointsize=12)
par(mar = c(1,1,1,1))
plot(g.test, 
     edge.width = E(g.test)$weight/ 40,
     vertex.size = V(g.test)$strength.total / 1000 ,
     edge.arrow.size = 0.2,
     edge.curved=0.2,
     vertex.color = V(g.test)$color,
     vertex.label.family= "Helvetica",
     vertex.label.color = "black",
     vertex.frame.color = V(g.test)$color,
     layout = layout_with_dh, 
     rescale = T, 
     axes = F)
dev.off()


#try using drl layout
layout =  layout_with_drl(g.test, weights = E(g.test)$weight, options=list(simmer.attraction=0), dim = 2)
svg(filename="removejunkmediagov-drl-all.svg", 
    width=30, 
    height=15, 
    pointsize=8)
par(mar = c(1,1,1,1))
plot(g.test, 
     edge.width = E(g.test)$weight/ 50,
     vertex.size = V(g.test)$strength.in / 500 ,
     edge.arrow.size = 0.2,
     edge.curved=0.2,
     vertex.color = V(g.test)$color,
     vertex.label = ifelse(V(g.test)$strength.in > 700, V(g.test)$name, NA),
     vertex.label.family= "Arial Bold",
     vertex.label.color = "black",
     vertex.frame.color = V(g.test)$color,
     layout = layout, 
     rescale = T, 
     axes = F)
dev.off()

# calcualte clusters 

#components = clusters(g.test)$membership
clus = cluster_walktrap(g.test, weights = E(g.test)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
V(g.test)$mebership <- membership(clus)


#components = components(g.test, mode = "strong")
colours = sample ( rainbow ( max ( V(g.test)$mebership )  + 1) )
V(g.test)$color2 = colours[V(g.test)$mebership +1]

svg(filename="removejunkmediagov-drl-cluster.svg", 
    width=30, 
    height=15, 
    pointsize=8)
par(mar = c(1,1,1,1))
plot(g.test, 
     edge.width = E(g.test)$weight/ 50,
     vertex.size = V(g.test)$strength.in / 500 ,
     edge.arrow.size = 0.2,
     edge.curved=0.2,
     vertex.color = V(g.test)$color2,
     vertex.label = ifelse(V(g.test)$strength.in > 700, V(g.test)$name, NA),
     vertex.label.family= "Arial Bold",
     vertex.label.color = "black",
     vertex.frame.color = V(g.test)$color2,
     layout = layout, 
     rescale = T, 
     axes = F)
dev.off()

