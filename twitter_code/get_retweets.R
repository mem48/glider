#Find RT

retweets <- readRDS("twitter/Tweets-2017-07-10.Rds")
retweets <- retweets[retweets$isRetweet,]

#Add retweet column
locs <- gregexpr(": ",retweets$text)
locs <- sapply(locs, function (x) x[1])
retweets$retweetof <- substr(retweets$text,5,locs - 1)


conn <- retweets[,c("screenName","retweetof")]
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
conn.simple$weight <- weights/mean(weights)


g <- graph_from_data_frame(conn.simple, directed = T) #Made the graph
g <- simplify(g, remove.loops = T) #Remove self references


plot(g)










library(igraph)

graph <- graph_from_data_frame(conn.simple, directed = T)
gorder(graph)
graph.top <- delete.vertices(graph, which(degree(graph)<=10))
gorder(graph.top)
plot(graph.top, edge.width=E(graph.top)$weight/2, layout = layout_with_dh)

conn.noself <- conn.simple[conn.simple$from != conn.simple$to,]
graph.noself <- graph_from_data_frame(conn.noself, directed = T)
gorder(graph.noself)
graph.noself.top <- delete.vertices(graph.noself, which(degree(graph.noself)<=10))
gorder(graph.noself.top)

#tiff("Plot2.tiff", res = 300)

svg(filename="Std_SVG.svg", 
    width=15, 
    height=15, 
    pointsize=10)
par(mar = c(1,1,1,1))
plot(g, 
     edge.width = E(g)$weight/2,
     vertex.size = 5,
     layout = layout_with_fr, 
     rescale = T, 
     #xlim = c(0,5),
     #ylim = c(-1,4),
     axes = F)
dev.off()


svg(filename="Std_SVG.svg", 
    width=10, 
    height=20, 
    pointsize=12)
my_sc_plot(data)
dev.off()
