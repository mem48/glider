# How to Make test data

library(igraph)

conn.simple <- readRDS("../glider/twitter/conn-simple-summyprogress2.Rds")
g <- graph_from_data_frame(conn.simple, directed = T) #Make the graph
g <- simplify(g, remove.loops = T)
g <- delete.vertices(g, which(degree(g)<=1))

saveRDS(g, "twitterNetwork.Rds")

