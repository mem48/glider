# Plots For Paper

# Plots hsoing the data cleaning process

library(igraph)
library(dplyr)

g <- readRDS("../twitter_data/parRerun/All_Data.Rds")
gorder(g)
g.trim <- delete.vertices(g, which(degree(g) < 200))
gorder(g.trim)


#Add on Categorisation
common <- readxl::read_xlsx("../twitter_data/accounts_fulldata_trim.xlsx", sheet =  "accounts_fulldata")
common <- common[,c("name","Account Category","Account Type")]
common <- common[!is.na(common$`Account Category`),]

verts <- igraph::as_data_frame(g.trim, what="vertices")
verts <- left_join(verts, common, by = c("name" = "name"))

#add to graph

V(g.trim)$type <- verts$`Account Category`

#Colour Code the 
V(g.trim)$color=V(g.trim)$type

V(g.trim)$color=gsub("Keep","green",V(g.trim)$color) 
V(g.trim)$color=gsub("Government","blue",V(g.trim)$color)
V(g.trim)$color=gsub("Junk","red",V(g.trim)$color) 
V(g.trim)$color=gsub("Media","orange",V(g.trim)$color) 
V(g.trim)$color[is.na(V(g.trim)$color)] = "grey"


# Calcualte Values quick values
V(g.trim)$strength.in <- strength(g.trim, mode = "in")
layout =  layout_with_drl(g.trim, weights = E(g.trim)$weight, options=list(simmer.attraction=0), dim = 2)

ecount(g.trim)
g.trim <- delete.edges(g.trim, which(E(g.trim)$weight < 10))
ecount(g.trim)

#svg(filename="../twitter_plots/MidParReRun-Clusters3.svg", width=30, height=15, pointsize=4)
png(filename="../twitter_plots/Trim_with_categories.png", width=15, height=15, units = 'in', res = 600, pointsize=12)   
par(mar = c(0.01,0.01,0.01,0.01)); plot(g.trim,
                                        edge.width = E(g.trim)$weight/ 500,
                                        vertex.size = V(g.trim)$strength.in / 2000,
                                        edge.arrow.size = 0.2,
                                        edge.curved=0.2,
                                        vertex.color = V(g.trim)$color,
                                        vertex.label = ifelse(V(g.trim)$strength.in > 10000, V(g.trim)$name, NA),
                                        vertex.label.family= "arial",
                                        vertex.label.color = "black",
                                        vertex.frame.color = "dark grey",
                                        layout = layout, 
                                        rescale = T, 
                                        axes = F); dev.off()



remove <- common$name[common$`Account Category` %in% c("Junk","Media","Government")]
g.trim <- delete.vertices(g, which(V(g)$name %in% remove)) # Discard the junk

# Calcualte Values quick values
V(g.trim)$strength.in <- strength(g.trim, mode = "in")

# Clacualte Slow values
V(g.trim)$between <- betweenness(g.trim)

#clus.info = cluster_infomap(g.trim, e.weights = E(g.trim)$weight, modularity = FALSE, nb.trials = 3) # Used both directed and weight info
#clus.between = cluster_edge_betweenness(g.trim, weights = E(g.trim)$weight, directed = TRUE)
#https://bommaritollc.com/2012/06/17/summary-community-detection-algorithms-igraph-0-6/
clus.walk = cluster_walktrap(g.trim, weights = E(g.trim)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)


#V(g.trim)$member.info <- membership(clus.info)
#V(g.trim)$member.between <- membership(clus.between)
V(g.trim)$member.walk <- membership(clus.walk)

# Make a slimed down version for plotting
ecount(g.trim)
gorder(g.trim)
g.plot <- delete.vertices(g.trim, which(degree(g.trim) < 10))
g.plot <- delete.edges(g.plot, which(E(g.plot)$weight <10))
g.plot <- delete.vertices(g.plot, which(degree(g.plot) < 1))
ecount(g.plot)
gorder(g.plot)


layout =  layout_with_drl(g.plot, weights = E(g.plot)$weight, options=list(simmer.attraction=0), dim = 2)

clus_summ <- as.data.frame(table(V(g.plot)$member))
clus_summ <- clus_summ[order(-clus_summ$Freq),]
clus_summ$colour <- c("red","dodgerblue","gold","violet","green4","wheat","firebrick","darkorange","greenyellow","lighblue","darkorchid")


colours = sample ( rainbow ( max ( V(g.plot)$member.between, na.rm= T )  + 1) )
barplot(rep(1,length(colours)), col= colours)
V(g.plot)$color = colours[V(g.plot)$member.between +1]


#svg(filename="../twitter_plots/MidParReRun-Clusters3.svg", width=30, height=15, pointsize=4)
png(filename="../twitter_plots/Plot2_MainNewtork_NoJunKMediaGov.png", width=15, height=15, units = 'in', res = 1200, pointsize=2)   
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

