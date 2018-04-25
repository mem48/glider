combined_2013 <- readRDS("../jobs_data/combined_2013_base.Rds")


###########################################################################
#Construct Energy Architypes
############################################################################

sel <- combined_2013[,c("aacode","aagpd1213","Finchtyp","mainfuel","watersys","tank","Finmhboi")]
sel <- sel[with(sel, order(Finchtyp,mainfuel,watersys,tank,Finmhboi)),]
uni <- unique(sel[,c("Finchtyp","mainfuel","watersys","tank","Finmhboi")])

uni$count <- NA
uni$countsamp <- NA
uni$conf <- NA
for (f in 1:nrow(uni)){
  uni$count[f] <- sum(sel$aagpd1213[sel$Finchtyp == uni$Finchtyp[f] & sel$mainfuel == uni$mainfuel[f] & sel$watersys == uni$watersys[f] & sel$tank == uni$tank[f] & sel$Finmhboi == uni$Finmhboi[f] ])
  uni$countsamp[f] <- nrow(sel[sel$Finchtyp == uni$Finchtyp[f] & sel$mainfuel == uni$mainfuel[f] & sel$watersys == uni$watersys[f] & sel$tank == uni$tank[f] & sel$Finmhboi == uni$Finmhboi[f], ])
  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
}

write.csv(uni,"data/energy_archetype_13_raw.csv")


###########################################################################
#Construct General Architypes - Version 1
############################################################################

sel <- combined_2013[,c("aacode","aagpd1213","type","dwage9x")]
sel <- sel[with(sel, order(type,dwage9x)),]
uni <- unique(sel[,c("type","dwage9x")])

uni$count <- NA
uni$countsamp <- NA
uni$conf <- NA
for (f in 1:nrow(uni)){
  uni$count[f] <- sum(sel$aagpd1213[sel$type == uni$type[f]  & sel$dwage9x == uni$dwage9x[f] ])
  uni$countsamp[f] <- nrow(sel[sel$type == uni$type[f]  & sel$dwage9x == uni$dwage9x[f] , ])
  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
}

write.csv(uni,"data/general_architype_2013_raw.csv")


###########################################################################
#Construct General Architypes - Other Version
############################################################################

sel <- combined_2013[,c("aacode","aagpd1213","dwtypenx","dwage9x")]
sel <- sel[with(sel, order(dwtypenx,aagpd1213,dwage9x)),]
uni <- unique(sel[,c("dwtypenx","dwage9x")])

uni$count <- NA
uni$countsamp <- NA
uni$conf <- NA
for (f in 1:nrow(uni)){
  uni$count[f] <- sum(sel$aagpd1213[sel$dwtypenx == uni$dwtypenx[f]  & sel$dwage9x == uni$dwage9x[f] ])
  uni$countsamp[f] <- nrow(sel[sel$dwtypenx == uni$dwtypenx[f]  & sel$dwage9x == uni$dwage9x[f] , ])
  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
}

write.csv(uni,"data/general_archetypes_13_raw.csv")


###########################################################################
#Construct Roof Architypes
############################################################################

sel <- combined_2013[,c("aacode","aagpd1213","typerstr","attic","LoftIns")]
sel <- sel[with(sel, order(typerstr,attic,LoftIns)),]
uni <- unique(sel[,c("typerstr","attic","LoftIns")])

uni$count <- NA
uni$countsamp <- NA
uni$conf <- NA
for (f in 1:nrow(uni)){
  uni$count[f] <- sum(sel$aagpd1213[sel$typerstr == uni$typerstr[f] & sel$attic == uni$attic[f]  & sel$LoftIns == uni$LoftIns[f]  ])
  uni$countsamp[f] <- nrow(sel[sel$typerstr == uni$typerstr[f] & sel$attic == uni$attic[f]  & sel$LoftIns == uni$LoftIns[f] , ])
  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
}

write.csv(uni,"data/roof_archetypes_13_raw.csv")


###########################################################################
#Construct Solar Architypes
############################################################################

sel <- combined_2013[,c("aacode","aagpd1213","SolarSuit","PV","Solar")]
sel <- sel[with(sel, order(SolarSuit,PV,Solar)),]
uni <- unique(sel[,c("SolarSuit","PV","Solar")])

uni$count <- NA
uni$countsamp <- NA
uni$conf <- NA
for (f in 1:nrow(uni)){
  uni$count[f] <- sum(sel$aagpd1213[sel$SolarSuit == uni$SolarSuit[f] & sel$PV == uni$PV[f] & sel$Solar == uni$Solar[f]  ])
  uni$countsamp[f] <- nrow(sel[sel$SolarSuit == uni$SolarSuit[f] & sel$PV == uni$PV[f] & sel$Solar == uni$Solar[f] , ])
  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
}

write.csv(uni,"data/solar_archetypes_13_raw.csv")


###########################################################################
#Construct Wall Architypes
############################################################################

sel <- combined_2013[,c("aacode","aagpd1213","typewstr2","wallinsy")]
sel <- sel[with(sel, order(typewstr2,wallinsy)),]
uni <- as.data.frame(unique(sel[,c("typewstr2","wallinsy")]))
names(uni) <- "typewstr2"

uni$count <- NA
uni$countsamp <- NA
uni$conf <- NA
for (f in 1:nrow(uni)){
  uni$count[f] <- sum(sel$aagpd1213[sel$typewstr2 == uni$typewstr2[f] &  sel$wallinsy == uni$wallinsy[f] ])
  uni$countsamp[f] <- nrow(sel[sel$typewstr2 == uni$typewstr2[f] &  sel$wallinsy == uni$wallinsy[f] , ])
  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
}

write.csv(uni,"data/wall_archetypes_13_raw.csv")

######################################################################
#Construct Windows and Doors Architypes
#####################################################################

sel <- combined_2013[,c("aacode","aagpd1213","winage", "dblglaz4")]
sel <- sel[with(sel, order(winage,dblglaz4)),]
uni <- as.data.frame(unique(sel[,c("winage","dblglaz4")]))

uni$count <- NA
uni$countsamp <- NA
uni$conf <- NA
for (f in 1:nrow(uni)){
  uni$count[f] <- sum(sel$aagpd1213[sel$winage == uni$winage[f] &  sel$dblglaz4 == uni$dblglaz4[f] ])
  uni$countsamp[f] <- nrow(sel[sel$winage == uni$winage[f] &  sel$dblglaz4 == uni$dblglaz4[f] , ])
  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
}

write.csv(uni,"data/window_archetypes_13_raw.csv")


#####################################################################
#Construct Floor Archetypes
#####################################################################

sel <- combined_2013[,c("aacode","aagpd1213","floorcons")]
sel <- sel[with(sel, order(floorcons)),]
uni <- as.data.frame(unique(sel[,c("floorcons")]))
names(uni) <- "floorcons"

uni$count <- NA
uni$countsamp <- NA
uni$conf <- NA
for (f in 1:nrow(uni)){
  uni$count[f] <- sum(sel$aagpd1213[sel$floorcons == uni$floorcons[f] ])
  uni$countsamp[f] <- nrow(sel[sel$floorcons == uni$floorcons[f] , ])
  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
}

write.csv(uni,"data/floor_archetypes_13_raw.csv")
