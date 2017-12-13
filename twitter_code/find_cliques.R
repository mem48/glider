# Look For Cliques

library(igraph)

g <- readRDS("../twitter_data/parRerun/All_Data.Rds")
gorder(g)

#Remove the Junk Accounts
common <- readxl::read_xlsx("../twitter_data/accounts_fulldata_trim.xlsx", sheet =  "accounts_fulldata")
common <- common[,c("name","Account Category","Account Type")]
common <- common[!is.na(common$`Account Category`),]
remove <- common$name[common$`Account Category` %in% c("Junk","Media","Government")]


length(remove)

g.trim <- delete.vertices(g, which(V(g)$name %in% remove)) # Discard the junk

min.degree = 100
for(i in 1:100){
  g.trim <- delete.vertices(g.trim, which(degree(g.trim) < min.degree)) # Trim down to a manageable amount
  print(gorder(g.trim))
  if(min(degree(g.trim)) == min.degree){break()}
}

cliques <- cliques(g.trim, min = 100, max = 100)


summary(degree(g.trim))
gorder(g.trim)

message(paste0("Trimming removed ",100 - round(gorder(g.trim)/gorder(g)*100,2),"% of accounts and ",100 - round(ecount(g.trim)/ecount(g)*100,2),"% of connections" ))
gorder(g.trim)

#Find Cliques
max.cliques <- largest.cliques(g.trim) #get the size of the largest cliques to set the standard for clique size




cliques <- maximal.cliques(g.trim, min = 15)
cliques.df <- data.frame(id = 1:length(cliques), size = lengths(cliques), members = NA)
cliques.df$members <- sapply(1:nrow(cliques.df), function(x){as.vector(cliques[[x]])})

#For Each Vert Find which cliques it is in
verts <- igraph::as_data_frame(g.trim, what="vertices")
verts$id <- 1:nrow(verts)
verts.cliques <- list()

for(i in 1:nrow(verts)){
  match <-  sapply(1:nrow(cliques.df),function(x){i %in% cliques.df$members[[x]]})
  match.id <- (cliques.df$id)[match]
  if(length(match.id) > 0){
    verts.cliques[[i]] <- match.id
  }else{
    verts.cliques[[i]] <- NA
  }
}
rm(i)
verts$cliques <- verts.cliques
verts$ncliques <- lengths(verts$cliques)


lengths(cliques)

head(cliques)
foo <- cliques[[1]]
foo <- as.list(foo)
?igraph.vs

foo <- as.list(cliques)
head(foo)

bar <- membership(cliques)

