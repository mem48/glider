#Add Archteypes

combined_2013 <- readRDS("data/combined_2013_base.Rds")



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
combined_2013$dwtypenx <- as.character(combined_2013$dwtypenx)

#for(i in 1:nrow(combined_2013)){
#  combined_2013$generaltyp[i] <- general_arch$GeneralType[general_arch$type == combined_2013$type[i] & general_arch$dwage9x == combined_2013$dwage9x[i] ]
#}

for(i in 1:nrow(combined_2013)){
 combined_2013$generaltyp[i] <- general_arch$GeneralType[general_arch$dwtypenx == combined_2013$dwtypenx[i] & general_arch$dwage9x == combined_2013$dwage9x[i] ]
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

#Window Architype
win_arch <- read.csv("data/window_archetypes_13.csv", stringsAsFactors = FALSE)

combined_2013$wintyp <- NA
combined_2013$dblglaz4 <- as.character(combined_2013$dblglaz4)
combined_2013$winage <- as.character(combined_2013$winage)


for(i in 1:nrow(combined_2013)){
  combined_2013$wintyp[i] <- win_arch$WindowType[win_arch$winage == combined_2013$winage[i] & win_arch$dblglaz4 == combined_2013$dblglaz4[i] ]
}

combined_2013$wintyp <- as.factor(combined_2013$wintyp)
summary(combined_2013$wintyp)

#Floor Architype
floor_arch <- read.csv("data/floor_archetypes_13.csv", stringsAsFactors = FALSE)

combined_2013$floortyp <- NA
combined_2013$floorcons <- as.character(combined_2013$floorcons)

for(i in 1:nrow(combined_2013)){
  combined_2013$floortyp[i] <- floor_arch$FloorType[floor_arch$floorcons == combined_2013$floorcons[i] ]
}

combined_2013$floortyp <- as.factor(combined_2013$floortyp)
summary(combined_2013$floortyp)



#Combine togther archetypes
#General-Energy-Wall-Roof-Solar
combined_2013$archcode <- paste0(combined_2013$generaltyp,"-",combined_2013$walltyp,"-",combined_2013$floortyp,"-",combined_2013$rooftyp,"-",combined_2013$solartyp,"-",combined_2013$wintyp,"-",combined_2013$energytyp)
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
