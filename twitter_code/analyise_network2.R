# Clustering Analysis
library(igraph)
library(dplyr)

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

