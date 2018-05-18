# get cluster summaries
library(dplyr)
library(tidyr)
library(igraph)

g.trim <- readRDS("../twitter_data/all/graph_advanced.Rds")
verts <- igraph::as_data_frame(g.trim, what="vertices")
edges <- igraph::as_data_frame(g.trim, what="edges")

g.contract <- readRDS("../twitter_data/all/graph_contract_advanced_keywords.Rds")
verts.contract <- igraph::as_data_frame(g.contract, "vertices")


# Cont the degree and strenght of 

clus.groups = verts[,c("name", "cluster.friends")]
names(clus.groups) = c("name","clus")

edges = left_join(edges, clus.groups, by = c("from" = "name"))
names(edges) = c(names(edges)[1:7],"clus.from")
edges = left_join(edges, clus.groups, by = c("to" = "name"))
names(edges) = c(names(edges)[1:8],"clus.to")

count_nonzero = function(x){
  x = x[x>0]
  return(length(x))
}
count_nonzero(c(1,3,5,0,1,0))

edges.summary = edges %>%
                group_by(clus.from, clus.to) %>%
                summarise(friends.weight = sum(friends),   
                          likes.weight = sum(likes),   
                          mentions.weight = sum(weight), 
                          retweets.weight = sum(weight), 
                          all.weight = sum(weight),
                          friends.degree = count_nonzero(friends),   
                          likes.degree = count_nonzero(likes),   
                          mentions.degree = count_nonzero(weight), 
                          retweets.degree = count_nonzero(weight), 
                          all.degree = count_nonzero(weight))

# combine all external into a single value
edges.summary$intext <- ifelse(edges.summary$clus.from == edges.summary$clus.to,"internal","external")

edges.summary = edges.summary %>%
  group_by(clus.from, intext) %>%
  summarise(friends.weight = sum(friends.weight),   
            likes.weight = sum(likes.weight),   
            mentions.weight = sum(mentions.weight), 
            retweets.weight = sum(retweets.weight), 
            all.weight = sum(all.weight),
            friends.degree = sum(friends.degree),   
            likes.degree = sum(likes.degree),   
            mentions.degree = sum(mentions.degree), 
            retweets.degree = sum(retweets.degree), 
            all.degree = sum(all.degree))

edges.summary = edges.summary[edges.summary$clus.from <= 26,]

edges.summary.int = edges.summary[edges.summary$intext == "internal",]
edges.summary.ext = edges.summary[edges.summary$intext == "external",]
names(edges.summary.int) = paste0("int_",names(edges.summary.int))
names(edges.summary.ext) = paste0("ext_",names(edges.summary.ext))
edges.summary.int <- as.data.frame(edges.summary.int)
edges.summary.ext <- as.data.frame(edges.summary.ext)

edges.summary = left_join(edges.summary.int, edges.summary.ext, by = c("int_clus.from" = "ext_clus.from"))

# calcualte ratios
edges.summary$rat_friends.weight = edges.summary$int_friends.weight / edges.summary$ext_friends.weight
edges.summary$rat_likes.weight = edges.summary$int_likes.weight / edges.summary$ext_likes.weight
edges.summary$rat_mentions.weight = edges.summary$int_mentions.weight / edges.summary$ext_mentions.weight
edges.summary$rat_retweets.weight = edges.summary$int_retweets.weight / edges.summary$ext_retweets.weight
edges.summary$rat_all.weight = edges.summary$int_all.weight / edges.summary$ext_all.weight
edges.summary$rat_friends.degree = edges.summary$int_friends.degree / edges.summary$ext_friends.degree
edges.summary$rat_likes.degree = edges.summary$int_likes.degree / edges.summary$ext_likes.degree
edges.summary$rat_mentions.degree = edges.summary$int_mentions.degree / edges.summary$ext_mentions.degree
edges.summary$rat_retweets.degree = edges.summary$int_retweets.degree / edges.summary$ext_retweets.degree
edges.summary$rat_all.degree = edges.summary$int_all.degree / edges.summary$ext_all.degree


verts.summary = verts %>%
                group_by(cluster.friends) %>%
                summarise(accounts = n())

verts.summary <- as.data.frame(verts.summary)
class(verts.summary)
verts.summary = verts.summary[verts.summary$cluster.friends <= 26,]
verts.summary$maxconnections <- (verts.summary$accounts * (verts.summary$accounts  - 1))

clusters = left_join(edges.summary, verts.summary, by = c("int_clus.from" = "cluster.friends"))
clusters$density = clusters$int_all.degree / clusters$maxconnections


foo = diameter(g.trim, directed = T, weights = E(g.trim)$weight)
foo2 = diameter(g.trim, directed = T)

cluster_propperties = list()
for(i in 1:27){
  if(i == 27){
    #Do whole Graph
    g.clus =  g.trim
  }else{
    #Do Cluster
    g.clus =  delete.vertices(g.trim, which(V(g.trim)$cluster.friends != i))
  }
  
  
  
  message(paste0(Sys.time()," doing ",i," of size ",gorder(g.clus)))
  
  #make graphs of each connections type
  g.clus.rt = delete.edges(g.clus, which(E(g.clus)$retweets == 0))
  g.clus.mn = delete.edges(g.clus, which(E(g.clus)$mentions == 0))
  g.clus.lk = delete.edges(g.clus, which(E(g.clus)$likes == 0))
  g.clus.fr = delete.edges(g.clus, which(E(g.clus)$friends == 0))
  
  #diamiter
  dia.all = diameter(g.clus, directed = T, weights = rep(1,ecount(g.clus)))
  dia.retweets = diameter(g.clus.rt, directed = T, weights = rep(1,ecount(g.clus.rt)))
  dia.friends = diameter(g.clus.fr, directed = T, weights = rep(1,ecount(g.clus.fr)))
  dia.mentions = diameter(g.clus.mn, directed = T, weights = rep(1,ecount(g.clus.mn)))
  dia.likes = diameter(g.clus.lk, directed = T, weights = rep(1,ecount(g.clus.lk)))
  
  #radius
  rad.all = radius(g.clus, mode = "all")
  rad.retweets = radius(g.clus.rt, mode = "all")
  rad.friends = radius(g.clus.fr, mode = "all")
  rad.mentions = radius(g.clus.mn, mode = "all")
  rad.likes = radius(g.clus.lk, mode = "all")
  
  #girth
  girth.all = girth(g.clus, circle = F)
  girth.retweets = girth(g.clus.rt, circle = F)
  girth.friends = girth(g.clus.fr, circle = F)
  girth.mentions = girth(g.clus.mn, circle = F)
  girth.likes = girth(g.clus.lk, circle = F)
  
  #density
  connect.max = gorder(g.clus) * (gorder(g.clus) - 1)
  density.all = ecount(g.clus) / connect.max
  density.retweets = ecount(g.clus.rt) / connect.max
  density.friends = ecount(g.clus.fr) / connect.max
  density.mentions = ecount(g.clus.mn) / connect.max
  density.likes = ecount(g.clus.lk) / connect.max
  
  res = data.frame(clus = i,
                   order = gorder(g.clus),
                   edges = ecount(g.clus),
                   diameter.all = dia.all,
                   diameter.retweets = dia.retweets,
                   diameter.friends = dia.friends,
                   diameter.mentions = dia.mentions,
                   diameter.likes = dia.likes,
                   radius.all = rad.all,
                   radius.retweets = rad.retweets,
                   radius.friends = rad.friends,
                   radius.mentions = rad.mentions,
                   radius.likes = rad.likes,
                   girth.all = girth.all$girth,
                   girth.retweets = girth.retweets$girth,
                   girth.friends = girth.friends$girth,
                   girth.mentions = girth.mentions$girth,
                   girth.likes = girth.likes$girth,
                   density.all = density.all,
                   density.retweets = density.retweets,
                   density.friends = density.friends,
                   density.mentions = density.mentions,
                   density.likes = density.likes
                   
                   )
  cluster_propperties[[i]] = res
}

cluster_propperties2 = bind_rows(cluster_propperties)



write.csv(cluster_propperties2,"../twitter_data/all/cluster_stats2.csv")




