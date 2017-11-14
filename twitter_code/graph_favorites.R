#Graph favorites
favorites <- readRDS("twitter/data/coreT-favorites-2017-08-14.Rds")
conn <- favorites[,c("favOf","screenName")]
names(conn) <- c("from","to")

conn.simple <- unique(conn)
conn.simple$weight <- 1

get.weights <- function(a){
  sub <- conn[conn$from == conn.simple$from[a] & conn$to == conn.simple$to[a],]
  res <- as.integer(nrow(sub))
  return(res)
}

weights <- lapply(1:nrow(conn.simple),get.weights)
weights <- unlist(weights)
conn.simple$weight <- weights

g <- graph_from_data_frame(conn.simple, directed = T) #Made the graph
g <- simplify(g, remove.loops = T) #Remove self references
gorder(g)
g <- delete.vertices(g, which(degree(g)<=3))
#gorder(g)
#g <- delete.vertices(g, which(degree(g)<=3)) #Do this muliple time to remove stragglers
gorder(g)

#Create High Resolution Plots
svg(filename="CoreT-favorites.svg", 
    width=15, 
    height=15, 
    pointsize=10)
par(mar = c(1,1,1,1))
plot(g, 
     edge.width = E(g)$weight/2,
     vertex.size = 5,
     layout = layout_with_fr, 
     rescale = T, 
     axes = F)
dev.off()

verts <- as_data_frame(g, what="vertices")
verts$order <- degree(g, mode = "total")
foo <- degree(g, mode = "total")

foo <- favorites[favorites$screenName %in% c("PassivhausTrust","ElrondBurrell"),]
