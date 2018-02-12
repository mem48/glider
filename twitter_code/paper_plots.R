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



#Remove the Junk Accounts
common <- readxl::read_xlsx("../twitter_data/accounts_fulldata_trim.xlsx", sheet =  "accounts_fulldata")
common <- common[,c("name","Account Category","Account Type")]
common <- common[!is.na(common$`Account Category`),]
remove <- common$name[common$`Account Category` %in% c("Junk","Media","Government")]
length(remove)


g.trim <- delete.vertices(g, which(V(g)$name %in% remove)) # Discard the junk
summary(degree(g))
g.trim <- delete.vertices(g.trim, which(degree(g.trim) < 200)) # Trim down to a manageable amount

message(paste0("Trimming removed ",100 - round(gorder(g.trim)/gorder(g)*100,2),"% of accounts and ",100 - round(ecount(g.trim)/ecount(g)*100,2),"% of connections" ))


# Calcualte Values quick values
V(g.trim)$strength.in <- strength(g.trim, mode = "in")

clus.walk = cluster_walktrap(g.trim, weights = E(g.trim)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)

V(g.trim)$member.walk <- membership(clus.walk)


clus_summ <- as.data.frame(table(V(g.trim)$member.walk))
clus_summ <- clus_summ[order(-clus_summ$Freq),]
cols <- c("red","dodgerblue","gold","violet","green4","wheat","firebrick","darkorange","greenyellow","lightblue","darkorchid")
cols <- c(cols,rep("black",nrow(clus_summ)-length(cols)))
clus_summ$colour <- cols

V(g.trim)$colour <- "white"
for(i in 1:gorder(g.trim)){
  V(g.trim)$colour[i] <- clus_summ$colour[clus_summ$Var1 == V(g.trim)$member.walk[i] ]
}
summary(as.factor(V(g.trim)$colour))

# Make a slimed down version for plotting
ecount(g.trim)
gorder(g.trim)
g.plot <- delete.vertices(g.trim, which(degree(g.trim) < 100))
g.plot <- delete.edges(g.plot, which(E(g.plot)$weight <10))
g.plot <- delete.vertices(g.plot, which(degree(g.plot) < 1))
ecount(g.plot)
gorder(g.plot)




layout =  layout_with_drl(g.plot, weights = E(g.plot)$weight, options=list(simmer.attraction=0), dim = 2)




#svg(filename="../twitter_plots/MidParReRun-Clusters3.svg", width=30, height=15, pointsize=4)
png(filename="../twitter_plots/Plot2_MainNewtork_NoJunKMediaGov.png", width=15, height=10, units = 'in', res = 1200, pointsize=2)   
par(mar = c(0.01,0.01,0.01,0.01)); plot(g.plot,
                                        edge.width = E(g.plot)$weight/ 200,
                                        vertex.size = ifelse(V(g.plot)$strength.in / 1000  > 10, 10, V(g.plot)$strength.in / 1000)  ,
                                        edge.arrow.size = 0.2,
                                        edge.curved=0.2,
                                        vertex.color = V(g.plot)$colour,
                                        vertex.label = ifelse(V(g.plot)$strength.in > 1000, V(g.plot)$name, NA),
                                        vertex.label.family= "Arial",
                                        vertex.label.color = "black",
                                        vertex.frame.color = V(g.plot)$colour,
                                        layout = layout, 
                                        rescale = T, 
                                        axes = F); dev.off()

g.sub <- delete.vertices(g.trim, which(V(g.trim)$colour != "lightblue"))
verts.sub <- igraph::as_data_frame(g.sub, what="vertices")

