#Add Archteypes

combined_2013 <- readRDS("data/combined_2013_base.Rds")

###########################################################################
#Construct Energy Architypes
############################################################################

#sel <- combined_2013[,c("aacode","aagpd1213","Finchtyp","mainfuel","watersys","tank","Finmhboi")]
#sel <- sel[with(sel, order(Finchtyp,mainfuel,watersys,tank,Finmhboi)),]
#uni <- unique(sel[,c("Finchtyp","mainfuel","watersys","tank","Finmhboi")])

#uni$count <- NA
#uni$countsamp <- NA
#uni$conf <- NA
#for (f in 1:nrow(uni)){
#  uni$count[f] <- sum(sel$aagpd1213[sel$Finchtyp == uni$Finchtyp[f] & sel$mainfuel == uni$mainfuel[f] & sel$watersys == uni$watersys[f] & sel$tank == uni$tank[f] & sel$Finmhboi == uni$Finmhboi[f] ])
#  uni$countsamp[f] <- nrow(sel[sel$Finchtyp == uni$Finchtyp[f] & sel$mainfuel == uni$mainfuel[f] & sel$watersys == uni$watersys[f] & sel$tank == uni$tank[f] & sel$Finmhboi == uni$Finmhboi[f], ])
#  uni$conf[f] <- uni$count[f] / uni$countsamp[f]
#}

#write.csv(uni,"data/energy_archetype_13.csv")


###########################################################################
#Construct General Architypes
############################################################################

#sel <- combined_2013[,c("aacode","aagpd1213","type","dwage9x")]
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

#write.csv(uni,"data/general_architype_2013.csv")


###########################################################################
#Construct Roof Architypes
############################################################################

#sel <- combined_2013[,c("aacode","aagpd1213","typerstr","attic","LoftIns")]
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

#sel <- combined_2013[,c("aacode","aagpd1213","SolarSuit","PV","Solar")]
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

#sel <- combined_2013[,c("aacode","aagpd1213","typewstr2","wallinsy")]
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

combined_2013$energytyp <- NA
combined_2013$Finchtyp <- as.character(combined_2013$Finchtyp)
combined_2013$mainfuel <- as.character(combined_2013$mainfuel)
combined_2013$watersys <- as.character(combined_2013$watersys)
combined_2013$tank <- as.character(combined_2013$tank)

for(h in 1:nrow(combined_2013)){
  combined_2013$energytyp[h] <- energy_arch$EnergyType[energy_arch$Finchtyp == combined_2013$Finchtyp[h] & energy_arch$mainfuel == combined_2013$mainfuel[h] & energy_arch$watersys == combined_2013$watersys[h] & energy_arch$tank == combined_2013$tank[h] & energy_arch$Finmhboi == combined_2013$Finmhboi[h]]
}

combined_2013$energytyp <- as.factor(combined_2013$energytyp)
summary(combined_2013$energytyp)

#General Architype
general_arch <- read.csv("data/general_archetypes_13.csv", stringsAsFactors = FALSE)

combined_2013$generaltyp <- NA
combined_2013$type <- as.character(combined_2013$type)
combined_2013$dwage9x <- as.character(combined_2013$dwage9x)

for(i in 1:nrow(combined_2013)){
  combined_2013$generaltyp[i] <- general_arch$GeneralType[general_arch$type == combined_2013$type[i] & general_arch$dwage9x == combined_2013$dwage9x[i] ]
}

combined_2013$generaltyp <- as.factor(combined_2013$generaltyp)
summary(combined_2013$generaltyp)

#Roof Architype
roof_arch <- read.csv("data/roof_archetypes_13.csv", stringsAsFactors = FALSE)

combined_2013$rooftyp <- NA
combined_2013$typerstr <- as.character(combined_2013$typerstr)
combined_2013$attic <- as.character(combined_2013$attic)
combined_2013$LoftIns <- as.character(combined_2013$LoftIns)

for(i in 1:nrow(combined_2013)){
  combined_2013$rooftyp[i] <- roof_arch$RoofType[roof_arch$typerstr == combined_2013$typerstr[i] & roof_arch$attic == combined_2013$attic[i] & roof_arch$LoftIns == combined_2013$LoftIns[i]]
}

combined_2013$rooftyp <- as.factor(combined_2013$rooftyp)
summary(combined_2013$rooftyp)

#Solar Architype
solar_arch <- read.csv("data/solar_archetypes_13.csv", stringsAsFactors = FALSE)

combined_2013$solartyp <- NA
combined_2013$SolarSuit <- as.character(combined_2013$SolarSuit)
combined_2013$PV <- as.character(combined_2013$PV)
combined_2013$Solar <- as.character(combined_2013$Solar)

for(i in 1:nrow(combined_2013)){
  combined_2013$solartyp[i] <- solar_arch$SolarType[solar_arch$SolarSuit == combined_2013$SolarSuit[i] & solar_arch$PV == combined_2013$PV[i] & solar_arch$Solar == combined_2013$Solar[i] ]
}

combined_2013$solartyp <- as.factor(combined_2013$solartyp)
summary(combined_2013$solartyp)

#Wall Architype
wall_arch <- read.csv("data/wall_archetypes_13.csv", stringsAsFactors = FALSE)

combined_2013$walltyp <- NA
combined_2013$typewstr2 <- as.character(combined_2013$typewstr2)
combined_2013$wallinsy <- as.character(combined_2013$wallinsy)


for(i in 1:nrow(combined_2013)){
  combined_2013$walltyp[i] <- wall_arch$WallType[wall_arch$typewstr2 == combined_2013$typewstr2[i] & wall_arch$wallinsy == combined_2013$wallinsy[i] ]
}

combined_2013$walltyp <- as.factor(combined_2013$walltyp)
summary(combined_2013$walltyp)

#Combine togther archetypes
#General-Energy-Wall-Roof-Solar
combined_2013$archcode <- paste0(combined_2013$generaltyp,"-",combined_2013$rooftyp,"-",combined_2013$walltyp,"-",combined_2013$energytyp,"-",combined_2013$solartyp)
#combined_2013$archcode <- paste0(combined_2013$energytyp,"-",combined_2013$walltyp,"-",combined_2013$rooftyp,"-",combined_2013$solartyp)
sel <- combined_2013[,c("aacode","aagpd1213","archcode")]
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


saveRDS(combined_2013,"data/combined_2013_arch.Rds")
