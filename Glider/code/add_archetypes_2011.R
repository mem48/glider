#Add Archteypes

combined_2011 <- readRDS("data/combined_2011_base.Rds")

###########################################################################
#Construct Energy Architypes
############################################################################

sel <- combined_2011[,c("aacode","aagpd1213","Finchtyp","mainfuel","watersys","tank","Finmhboi")]
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

write.csv(uni,"data/energy_archetype_11.csv")

energy_13 <- read.csv("data/energy_archetype_13.csv")

#Fix this loop tomorrow

uni$match <- NA
for(a in 1:nrow(uni)){
  if(nrow(uni[uni$Finchtyp[a] == energy_13$Finchtyp & uni$mainfuel[a] == energy_13$mainfuel & uni$tank[a] == energy_13$tank & uni$watersys[a] == energy_13$watersys,]) == 1){
    uni$match[a] <- TRUE
  }else{
    uni$match[a] <- FALSE
  }
}



###########################################################################
#Construct General Architypes - Version 1
############################################################################

#sel <- combined_2011[,c("aacode","aagpd1213","type","dwage9x")]
#sel <- sel[with(sel, order(type,dwage9x)),]
#uni <- unique(sel[,c("type","dwage9x")])

#uni$count <- NA
#uni$countsamp <- NA
#uni$conf <- NA
#for (f in 1:nrow(uni)){
#  uni$count[f] <- sum(sel$aagpd1213[sel$type == uni$type[f]  & sel$dwage9x == uni$dwage9x[f] ])
#  uni$countsamp[f] <- nrow(sel[sel$type == uni$type[f]  & sel$dwage9x == uni$dwage9x[f] , ])
#  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
#}

#write.csv(uni,"data/general_architype_2011.csv")


###########################################################################
#Construct General Architypes - Other Version
############################################################################

#sel <- combined_2011[,c("aacode","aagpd1213","dwtypenx","dwage9x")]
#sel <- sel[with(sel, order(dwtypenx,aagpd1213,dwage9x)),]
#uni <- unique(sel[,c("dwtypenx","dwage9x")])

#uni$count <- NA
#uni$countsamp <- NA
#uni$conf <- NA
#for (f in 1:nrow(uni)){
#  uni$count[f] <- sum(sel$aagpd1213[sel$dwtypenx == uni$dwtypenx[f]  & sel$dwage9x == uni$dwage9x[f] ])
#  uni$countsamp[f] <- nrow(sel[sel$dwtypenx == uni$dwtypenx[f]  & sel$dwage9x == uni$dwage9x[f] , ])
#  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
#}

#write.csv(uni,"data/general_archetypes_13.csv")


###########################################################################
#Construct Roof Architypes
############################################################################

#sel <- combined_2011[,c("aacode","aagpd1213","typerstr","attic","LoftIns")]
#sel <- sel[with(sel, order(typerstr,attic,LoftIns)),]
#uni <- unique(sel[,c("typerstr","attic","LoftIns")])

#uni$count <- NA
#uni$countsamp <- NA
#uni$conf <- NA
#for (f in 1:nrow(uni)){
#  uni$count[f] <- sum(sel$aagpd1213[sel$typerstr == uni$typerstr[f] & sel$attic == uni$attic[f]  & sel$LoftIns == uni$LoftIns[f]  ])
#  uni$countsamp[f] <- nrow(sel[sel$typerstr == uni$typerstr[f] & sel$attic == uni$attic[f]  & sel$LoftIns == uni$LoftIns[f] , ])
#  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
#}

#write.csv(uni,"data/roof_archetypes_13.csv")


###########################################################################
#Construct Solar Architypes
############################################################################

#sel <- combined_2011[,c("aacode","aagpd1213","SolarSuit","PV","Solar")]
#sel <- sel[with(sel, order(SolarSuit,PV,Solar)),]
#uni <- unique(sel[,c("SolarSuit","PV","Solar")])

#uni$count <- NA
#uni$countsamp <- NA
#uni$conf <- NA
#for (f in 1:nrow(uni)){
#  uni$count[f] <- sum(sel$aagpd1213[sel$SolarSuit == uni$SolarSuit[f] & sel$PV == uni$PV[f] & sel$Solar == uni$Solar[f]  ])
#  uni$countsamp[f] <- nrow(sel[sel$SolarSuit == uni$SolarSuit[f] & sel$PV == uni$PV[f] & sel$Solar == uni$Solar[f] , ])
#  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
#}

#write.csv(uni,"data/solar_archetypes_13.csv")


###########################################################################
#Construct Wall Architypes
############################################################################

#sel <- combined_2011[,c("aacode","aagpd1213","typewstr2","wallinsy")]
#sel <- sel[with(sel, order(typewstr2,wallinsy)),]
#uni <- as.data.frame(unique(sel[,c("typewstr2","wallinsy")]))
#names(uni) <- "typewstr2"

#uni$count <- NA
#uni$countsamp <- NA
#uni$conf <- NA
#for (f in 1:nrow(uni)){
#  uni$count[f] <- sum(sel$aagpd1213[sel$typewstr2 == uni$typewstr2[f] &  sel$wallinsy == uni$wallinsy[f] ])
#  uni$countsamp[f] <- nrow(sel[sel$typewstr2 == uni$typewstr2[f] &  sel$wallinsy == uni$wallinsy[f] , ])
#  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
#}

#write.csv(uni,"data/wall_archetypes_13.csv")



#Asign Energy Architype
energy_arch <- read.csv("data/energy_archetype_13.csv", stringsAsFactors = FALSE)

combined_2011$energytyp <- NA
combined_2011$Finchtyp <- as.character(combined_2011$Finchtyp)
combined_2011$mainfuel <- as.character(combined_2011$mainfuel)
combined_2011$watersys <- as.character(combined_2011$watersys)
combined_2011$tank <- as.character(combined_2011$tank)

for(h in 1:nrow(combined_2011)){
  combined_2011$energytyp[h] <- energy_arch$EnergyType[energy_arch$Finchtyp == combined_2011$Finchtyp[h] & energy_arch$mainfuel == combined_2011$mainfuel[h] & energy_arch$watersys == combined_2011$watersys[h] & energy_arch$tank == combined_2011$tank[h] & energy_arch$Finmhboi == combined_2011$Finmhboi[h]]
}

combined_2011$energytyp <- as.factor(combined_2011$energytyp)
summary(combined_2011$energytyp)

#General Architype
general_arch <- read.csv("data/general_archetypes_13.csv", stringsAsFactors = FALSE)

combined_2011$generaltyp <- NA
combined_2011$type <- as.character(combined_2011$type)
combined_2011$dwage9x <- as.character(combined_2011$dwage9x)
combined_2011$dwtypenx <- as.character(combined_2011$dwtypenx)

#for(i in 1:nrow(combined_2011)){
#  combined_2011$generaltyp[i] <- general_arch$GeneralType[general_arch$type == combined_2011$type[i] & general_arch$dwage9x == combined_2011$dwage9x[i] ]
#}

for(i in 1:nrow(combined_2011)){
 combined_2011$generaltyp[i] <- general_arch$GeneralType[general_arch$dwtypenx == combined_2011$dwtypenx[i] & general_arch$dwage9x == combined_2011$dwage9x[i] ]
}

combined_2011$generaltyp <- as.factor(combined_2011$generaltyp)
summary(combined_2011$generaltyp)

#Roof Architype
roof_arch <- read.csv("data/roof_archetypes_13.csv", stringsAsFactors = FALSE)

combined_2011$rooftyp <- NA
combined_2011$typerstr <- as.character(combined_2011$typerstr)
combined_2011$attic <- as.character(combined_2011$attic)
combined_2011$LoftIns <- as.character(combined_2011$LoftIns)

for(i in 1:nrow(combined_2011)){
  combined_2011$rooftyp[i] <- roof_arch$RoofType[roof_arch$typerstr == combined_2011$typerstr[i] & roof_arch$attic == combined_2011$attic[i] & roof_arch$LoftIns == combined_2011$LoftIns[i]]
}

combined_2011$rooftyp <- as.factor(combined_2011$rooftyp)
summary(combined_2011$rooftyp)

#Solar Architype
solar_arch <- read.csv("data/solar_archetypes_13.csv", stringsAsFactors = FALSE)

combined_2011$solartyp <- NA
combined_2011$SolarSuit <- as.character(combined_2011$SolarSuit)
combined_2011$PV <- as.character(combined_2011$PV)
combined_2011$Solar <- as.character(combined_2011$Solar)

for(i in 1:nrow(combined_2011)){
  combined_2011$solartyp[i] <- solar_arch$SolarType[solar_arch$SolarSuit == combined_2011$SolarSuit[i] & solar_arch$PV == combined_2011$PV[i] & solar_arch$Solar == combined_2011$Solar[i] ]
}

combined_2011$solartyp <- as.factor(combined_2011$solartyp)
summary(combined_2011$solartyp)

#Wall Architype
wall_arch <- read.csv("data/wall_archetypes_13.csv", stringsAsFactors = FALSE)

combined_2011$walltyp <- NA
combined_2011$typewstr2 <- as.character(combined_2011$typewstr2)
combined_2011$wallinsy <- as.character(combined_2011$wallinsy)


for(i in 1:nrow(combined_2011)){
  combined_2011$walltyp[i] <- wall_arch$WallType[wall_arch$typewstr2 == combined_2011$typewstr2[i] & wall_arch$wallinsy == combined_2011$wallinsy[i] ]
}

combined_2011$walltyp <- as.factor(combined_2011$walltyp)
summary(combined_2011$walltyp)

#Combine togther archetypes
#General-Energy-Wall-Roof-Solar
combined_2011$archcode <- paste0(combined_2011$generaltyp,"-",combined_2011$rooftyp,"-",combined_2011$walltyp,"-",combined_2011$energytyp,"-",combined_2011$solartyp)
#combined_2011$archcode <- paste0(combined_2011$energytyp,"-",combined_2011$walltyp,"-",combined_2011$rooftyp,"-",combined_2011$solartyp)
sel <- combined_2011[,c("aacode","aagpd1213","archcode")]
sel <- sel[with(sel, order(archcode)),]
uni <- as.data.frame(unique(sel[,c("archcode")]))
names(uni) <- "archcode"

uni$count <- NA
uni$countsamp <- NA
uni$conf <- NA
for (f in 1:nrow(uni)){
  uni$count[f] <- sum(sel$aagpd1213[sel$archcode == uni$archcode[f] ])
  uni$countsamp[f] <- nrow(sel[sel$archcode == uni$archcode[f]  , ])
  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
}
nrow(uni)
#Count of arch with only one sample
sum(uni$count[uni$countsamp == 1])
length(uni$count[uni$countsamp == 1])
#Coutn of arch with many samples
sum(uni$count[uni$countsamp >= 10])
length(uni$count[uni$countsamp >= 10])
plot(uni$count[order(-uni$count)])

sel <- uni[uni$countsamp >= 10,]
sel <- sel[sel$count >= 10000,]
plot(sel$count[order(-sel$count)])
sum(sel$count)


saveRDS(combined_2011,"data/combined_2011_arch.Rds")
