#Create quick network and trim it down
acc.clean <- readRDS("twitter/data/broadAccounts-clean.Rds")
conn.all <- readRDS("twitter/data/core-friend+followers-2017-08-08.Rds")
acc.summary <- readRDS("twitter/data/coreAccounts.Rds")

acc.keep <- rbind(acc.summary,acc.clean)
keep <- acc.keep$screenName
keep <- unique(keep)

connections <- conn.all[,c("screenName","friendof","followerof")]
connections$friendof <- as.character(connections$friendof)
connections$from <- NA
connections$to <- NA

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
nrow(connections)

#only keep the cleand accounts
connections <- connections[connections$from %in% keep | connections$to %in% keep,]
nrow(connections)


nrow(connections)
connections <- unique(connections)
nrow(connections)


graph <- graph_from_data_frame(connections)
gorder(graph)
graph <- delete.vertices(graph, which(degree(graph)<=1))
gorder(graph)

#save the trimmed list of accounts
trim <- V(graph.top)$name

acc.trim <- acc.keep[acc.keep$screenName %in% trim,]


saveRDS(acc.trim,"twitter/data/broadAccounts-trim.Rds")
