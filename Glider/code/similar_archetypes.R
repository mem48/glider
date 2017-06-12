library(parallel)

# To find siliar artchitypes
comb <- readRDS("data/combined_2013_arch.Rds")
sel <- comb[,c("aacode","aagpd1213","archcode","energytyp","generaltyp","rooftyp","solartyp","walltyp","wintyp","floortyp")]
rm(comb)

sel <- sel[with(sel, order(rooftyp,solartyp,walltyp,wintyp,floortyp)),]
sel$generaltyp2 <- substr(sel$generaltyp,1,1)
#uni <- as.data.frame(unique(sel[,c("energytyp","rooftyp","solartyp","walltyp","wintyp","floortyp")]))
#uni$subcode <- paste0(uni$rooftyp,uni$solartyp,uni$walltyp,uni$wintyp,uni$floortyp)
#sel$subcode <- paste0(sel$walltyp,sel$floortyp,sel$rooftyp,sel$solartyp,sel$wintyp,sel$energytyp)
sel$subcode <- paste0(sel$generaltyp2,sel$walltyp,sel$floortyp,sel$rooftyp,sel$solartyp,sel$wintyp)


#test <- sel[sel$generaltyp == "I",]
test <- sel
grp <- data.frame(id = unique(test$subcode), nhouse = NA)
grp$id <- as.character(grp$id)

for(b in 1:nrow(grp)){
  grp$nhouse[b] <- sum(test$aagpd1213[test$subcode == grp$id[b]]) 
}

grp <- grp[order(-grp$nhouse),]
csum <- cumsum(grp$nhouse)
check <- csum < sum(grp$nhouse) * 0.8
plot(csum)
grp_sub <- grp[check,]
csum2 <- cumsum(grp_sub$nhouse)
points(csum2, col = "Red")
grp_sub$strip <- substr(grp_sub$id,2,6)

grp2 <- data.frame(id = unique(grp_sub$strip), nhouse = NA)
for(b in 1:nrow(grp2)){
  grp2$nhouse[b] <- sum(grp_sub$nhouse[grp_sub$strip == grp2$id[b]]) 
} 

#compare <- data.frame(constr = grp2$id, semi = NA, detached = NA, mid = NA, end = NA, bungalow = NA, converted = NA, low = NA, high = NA)
sel$strip <- paste0(sel$walltyp,sel$floortyp,sel$rooftyp,sel$solartyp,sel$wintyp)



#comp <- matrix(nrow = nrow(uni), ncol = nrow(uni))
#comp <- as.data.frame(comp)
#names(comp) <- uni$archcode
#rownames(comp) <- uni$archcode

row <- grp2$id
row <- as.character(row)
col <- unique(sel$generaltyp2)


nrow <- length(grp2$id)
ncell <-  nrow * 8

#Do as lapply
countarch <- function(i){
  x <- if(i %% 8 == 0){8}else{i %% 8}
  y <- ((i-1) %/% nrow) + 1
  yid <- row[y]
  xid <- col[x]
  res <- sum(sel$aagpd1213[sel$generaltyp2 == xid & sel$strip == yid])
  return(res)
}


compare <- lapply(1:ncell,countarch)
compare <- matrix(unlist(compare), ncol = 8, byrow = FALSE)
compare <- as.data.frame(compare)
names(compare) <- col
rownames(compare) <- row

##### Something gone wrong as totals add up to more than the number of housing in england








#Do as lapply
countsimilar <- function(i){
  x <- if(i %% nrow == 0){nrow}else{i %% nrow}
  y <- ((i-1) %/% nrow) + 1
  yid <- uni$subcode[y]
  xid <- uni$subcode[x]
  xval <- uni[uni$subcode == xid,]
  yval <- uni[uni$subcode == yid,]
  res <- sum(xval$energytyp == yval$energytyp, xval$rooftyp == yval$rooftyp, xval$solartyp == yval$solartyp, xval$walltyp == yval$walltyp, xval$wintyp == yval$wintyp, xval$floortyp == yval$floortyp)
  return(res)
}

compare <- lapply(1:ncell,countsimilar)
compare <- matrix(unlist(compare), ncol = nrow, byrow = FALSE)
compare <- as.data.frame(compare)
names(compare) <- uni$subcode
rownames(compare) <- uni$subcode

saveRDS(compare,"data/archetypes_comparion3.Rds")
rm(nrow,ncell,x,y,yid,xid,xval,yval, uni, i)


#Sort by count then group with simialr architypes
rowcount <- data.frame(id = names(compare), sum=rowSums(compare)) #This one for editing
groups <- data.frame(id = names(compare), sum=rowSums(compare) ,group=NA, ndwel=NA) # this one to store
rowcount <- rowcount[order(-rowcount$sum),] # Sort
groups$id <- as.character(groups$id)

#Add in the numebr of dwellings

for(b in 1:nrow(groups)){
  groups$ndwel[b] <- sum(sel$aagpd1213[sel$subcode == groups$id[b]]) 
}


#Group the Architypes
for(a in 1:99999){
  id <- as.character(rowcount$id[1])
  sub <- as.data.frame(compare[,id, drop = FALSE])
  names(sub) <- c("res")
  sub <- sub[sub$res >=5,,drop = FALSE]
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





