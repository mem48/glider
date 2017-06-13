#library(parallel)

# To find siliar artchitypes
comb <- readRDS("data/combined_2013_arch.Rds")
sel <- comb[,c("aacode","aagpd1213","archcode","energytyp","generaltyp","rooftyp","solartyp","walltyp","wintyp","floortyp")]
rm(comb)

sel <- sel[with(sel, order(rooftyp,solartyp,walltyp,wintyp,floortyp)),]
sel$generaltyp2 <- substr(sel$generaltyp,1,1)
sel$subcode <- paste0(sel$generaltyp2,sel$walltyp,sel$floortyp,sel$rooftyp,sel$solartyp,sel$wintyp, sel$energytyp)

sum(sel$aagpd1213) #23253781

grp <- data.frame(id = unique(sel$subcode), nhouse = NA)
grp$id <- as.character(grp$id)

for(b in 1:nrow(grp)){
  grp$nhouse[b] <- sum(sel$aagpd1213[sel$subcode == grp$id[b]]) 
}

sum(sel$aagpd1213) #23253781


grp <- grp[order(-grp$nhouse),]
csum <- cumsum(grp$nhouse)
check <- csum < sum(grp$nhouse) * 0.8
plot(csum)

sum(sel$aagpd1213[sel$subcode %in% grp$id]) #23253781
sum(grp$nhouse)#23253781

grp_sub <- grp[check,]
csum2 <- cumsum(grp_sub$nhouse)
points(csum2, col = "Red")

sum(sel$aagpd1213[sel$subcode %in% grp_sub$id]) #18541922

grp_sub$strip <- substr(grp_sub$id,2,7)

grp2 <- data.frame(id = unique(grp_sub$strip), nhouse = NA)
for(b in 1:nrow(grp2)){
  grp2$nhouse[b] <- sum(grp_sub$nhouse[grp_sub$strip == grp2$id[b]]) 
} 

sum(grp2$nhouse) #18541922

sel$strip <- paste0(sel$walltyp,sel$floortyp,sel$rooftyp,sel$solartyp,sel$wintyp,sel$energytyp)


row <- grp2$id #To do without energy
row <- as.character(row)
col <- unique(sel$generaltyp2)


nrow <- length(row)
ncell <-  nrow * 8

#Do as lapply
countarch <- function(i){
  x <- if(i %% nrow == 0){nrow}else{i %% nrow}
  y <- ((i-1) %/% nrow) + 1
  yid <- col[y]
  xid <- row[x]
  res <- sum(sel$aagpd1213[sel$generaltyp2 == yid & sel$strip == xid]) # With out energy
  #res <- sum(sel$aagpd1213[sel$generaltyp2 == yid & sel$subcode == xid]) # With energy
  return(res)
}


compare <- lapply(1:ncell,countarch)
compare <- matrix(unlist(compare), ncol = 8, byrow = FALSE)
compare <- as.data.frame(compare)
names(compare) <- col
rownames(compare) <- row
sum(compare) 

compare$total <- rowSums(compare)



stop()

###########################################################################





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





