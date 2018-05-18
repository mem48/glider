
# keyword matrix
library(igraph)


g.contract <- readRDS("../twitter_data/all/graph_contract_advanced_keywords.Rds")






verts.contract <- igraph::as_data_frame(g.contract, "vertices")
edges.contract <- igraph::as_data_frame(g.contract, "edges")




verts.contract$name <- NULL
clustenames$id <- as.character(clustenames$id)
verts.contract <- left_join(verts.contract, clustenames, by = c("cluster.friends" = "id"))
V(g.contract)$name <- verts.contract$name