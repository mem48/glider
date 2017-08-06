library(igraph)

connect.all <- readRDS("twitter/data/Connections-livedump-2017-08-03.Rds")

#Get a list of unique twitter accounts
vertices1 <- as.character(unique(connect.all$screenName))
vertices2 <- as.character(unique(connect.all$friendof))
vertices3 <- as.character(unique(connect.all$followerof))
vertices <- as.character(unique(c(vertices1,vertices2,vertices3)))

vertices <- vertices[!is.na(vertices)]

vertices <- data.frame(id = vertices, core = 0)
for(c in 1:nrow(vertices)){
  if(vertices$id[c] %in% vertices2){
    vertices$core[c] <- 1
  }
}
rm(vertices1,vertices2,vertices3)

connections <- connect.all[,c("screenName","friendof","followerof")]
connections$friendof <- as.character(connections$friendof)
connections$from <- NA
connections$to <- NA


#takes ages
for(a in 1:nrow(connections)){
  if(is.na(connections$friendof[a])){
    connections$from[a] <- connections$screenName[a]
    connections$to[a] <- connections$followerof[a]
  }else{
    connections$from[a] <- connections$friendof[a]  
    connections$to[a] <- connections$screenName[a]
  }
}

getfrom <- function(d){
  if(is.na(connections$friendof[d])){
    return(connections$screenName[d])
  }else{
    return(connections$friendof[d])
  }
}

getto <- function(d){
  if(is.na(connections$friendof[d])){
    return(connections$followerof[d])
  }else{
    return(connections$screenName[d])
  }
}

connections$from <- lapply(1:nrow(connections),getfrom)
connections$to <- lapply(1:nrow(connections),getto)


connections <- connections[,c("from","to")]

saveRDS(connections,paste0("twitter/data/connectionslist-dump-",Sys.Date(),".RDs"))

summary(!is.na(connections$from)) #should all be true
summary(!is.na(connections$to))#should all be true
connections$weight <- 1

core <- as.character(unique(connect.all$friendof))
core <- core[!is.na(core)]
#increase weight of core accounts friends
for(b in 1:nrow(connections)){
  if(connections$from[b] %in% core){
    connections$weight[b] <- 2
  }
}

graph <- graph_from_data_frame(connections, directed = T, vertices = vertices)
gorder(graph)
graph <- delete.vertices(graph, which(degree(graph)<=1))
gorder(graph)

graph.top <- delete.vertices(graph, which(degree(graph)<=40))
gorder(graph.top)
acc.top <- as.data.frame(V(graph.top)$name)


plot(graph.top, layout=layout.fruchterman.reingold, vertex.size = 3)

V(graph)$color <- ifelse(V(graph)$core == 1, "red", "lightblue")


plot(graph,layout=layout.fruchterman.reingold,edge.width=E(graph)$weight, vertex.label = NA, vertex.size = 2)





test <- unique(connections$to[!(connections$to %in% vertices)])











#test <- connect.all[1:1000,]
graph <- graph_from_data_frame(connect.all, directed = T)
gorder(graph)

graph2 <- delete.vertices(graph, which(degree(graph)<=1))
gorder(graph2)

graph3 <- delete.vertices(graph2, which(degree(graph2)<=2))
gorder(graph3)


plot(graph2, vertex.size = 3,vertex.label = NA)



comm <- walktrap.community(graph2)

#Collapse the graph by communities.  This insight is due to this post http://stackoverflow.com/questions/35000554/collapsing-graph-by-clusters-in-igraph/35000823#35000823
graph.comm <- simplify(contract(graph2, membership(comm))) 
gorder(graph.comm)
plot(graph.comm, vertex.size = 3, vertex.label = NA)

test <- membership(comm)


comm2 <- cluster_edge_betweenness(graph2)



unique(connect.all$account.name)


connect.all <- connect.all[,c("follower.name","account.name")]
test <- connect.all[1:1000,]
graph <- graph_from_data_frame(connect.all, directed = T)
plot(graph)

foo <- degree(graph, mode = "all")
nodes <- unique(connect.all$follower.name)
nodes2 <- unique(connect.all$account.name)
nodes3 <- unique(c(nodes,nodes2))

graph2 <- delete.vertices(graph, which(degree(graph)<6))
plot(graph2)


followers2 <- followers[[1]]$getFollowers() # a follower's followers



test <- getUser("TheBRETrust")


us <- userFactory$new(screenName="test", name="Joe Smith")
us$getScreenName()
us$getName()