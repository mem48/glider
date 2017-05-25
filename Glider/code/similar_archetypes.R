# To find siliar artchitypes
comb <- readRDS("data/combined_2013_arch.Rds")
sel <- comb[,c("aacode","aagpd1213","archcode","energytyp","generaltyp","rooftyp","solartyp","walltyp","wintyp","floortyp")]

sel <- sel[with(sel, order(rooftyp,solartyp,walltyp,wintyp,floortyp)),]
uni <- as.data.frame(unique(sel[,c("archcode","energytyp","generaltyp","rooftyp","solartyp","walltyp","wintyp","floortyp")]))


comp <- matrix(nrow = nrow(uni), ncol = nrow(uni))
comp <- as.data.frame(comp)
names(comp) <- uni$archcode
rownames(comp) <- uni$archcode

nrow <- length(uni$archcode)
ncell <-  nrow ** 2

y_last = 1

for(i in 1:ncell){
  x <- if(i %% nrow == 0){nrow}else{i %% nrow}
  y <- ((i-1) %/% nrow) + 1
  if(y != y_last){print(paste0("Doing column ",y," of ",nrow," at ",Sys.time()))}
  yid <- colnames(comp)[y]
  xid <- rownames(comp)[x]
  xval <- uni[uni$archcode == xid,]
  yval <- uni[uni$archcode == yid,]
  comp[x,y] <- sum(xval$energytyp == yval$energytyp, xval$generaltyp == yval$generaltyp, xval$rooftyp == yval$rooftyp, xval$solartyp == yval$solartyp, xval$walltyp == yval$walltyp, xval$wintyp == yval$wintyp, xval$floortyp == yval$floortyp)
  y_last <- y
}

#comp$V1858 <- NULL #Don't know why this happend
saveRDS(comp,"data/archetypes_comparion2.Rds")
rm(nrow,ncell,x,y,yid,xid,xval,yval,y_last, uni, sel, i)
#stop()

#Sort by count then group with simialr architypes
rowcount <- data.frame(id = names(comp), sum=rowSums(comp)) #This one for editing
groups <- data.frame(id = names(comp), sum=rowSums(comp) ,group=NA, ndwel=NA) # this one to store
rowcount <- rowcount[order(-rowcount$sum),] # Sort
groups$id <- as.character(groups$id)

#Add in the numebr of dwellings
for(b in 1:nrow(groups)){
  groups$ndwel[b] <- sum(comb$aagpd1213[comb$archcode == groups$id[b]]) 
}
rm(b)

#Group the Architypes
for(a in 1:99999){
  id <- as.character(rowcount$id[1])
  sub <- as.data.frame(comp[,id, drop = FALSE])
  names(sub) <- c("res")
  sub <- sub[sub$res >=6,,drop = FALSE]
  getids <- rownames(sub)
  groups$group[groups$id %in% getids] <- a
  rowcount <- rowcount[!(rowcount$id %in% getids),]
  if(nrow(rowcount) == 0){
    break
  }
}
rm(a, id,sub, getids)




#Count the number of dwellings in each group
groupsums <- data.frame(group = groups$group[!duplicated(groups$group)], ndwel = NA)
for(c in 1:nrow(groupsums)){
  groupsums$ndwel[c] <- sum(groups$ndwel[groups$group == groupsums$group[c]])
}
rm(c)

groupsums <- groupsums[order(-groupsums$ndwel),]

csum <- cumsum(groupsums$ndwel)
plot(csum, ylab = "Number of Houses", xlab = "Number of Archetype Groups")

#Add in architype codes
groupsums$arch <- NA
for(d in 1:nrow(groupsums)){
  groupsums$arch[d] <- groups$id[groups$group == groupsums$group[d]][1]
}


#uni$count <- NA
#uni$countsamp <- NA
#uni$conf <- NA
#for (f in 1:nrow(uni)){
#  uni$count[f] <- sum(sel$aagpd1213[sel$archcode == uni$archcode[f] ])
#  uni$countsamp[f] <- nrow(sel[sel$archcode == uni$archcode[f]  , ])
#  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
#}





