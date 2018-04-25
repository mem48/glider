#Read in EHS and convert to RDS
library(foreign)
#library(dplyr, lib.loc = "M:/R/R-3.3.1/library")
#library(lazyeval, lib.loc = "M:/R/R-3.3.1/library")
library(dplyr)
library(lazyeval)
current_year <- 2013

Int2Factor <- function(x){
  if(!is.null(attr(x, "value.labels"))){
    #Has labels
    vlab <- attr(x, "value.labels")
    
    if(length(unique(x)) > (length(names(vlab)) + 1)){
      #More unique values than lables so might be values in diguise
      message(paste0("Values = ",length(unique(x))," Lables = ",length(names(vlab))," returning number" ))
      x <- as.numeric(x)
    }else{
      #Same of less values than lables so probably lookup
      message(paste0("Values = ",length(unique(x))," Lables = ",length(names(vlab))," returning factor" ))
      if(sum(duplicated(vlab)) > 0){
        cat("Duplicated levels:", vlab, "\n")
      }else if(sum(duplicated(names(vlab))) > 0){
        cat("Duplicated labels:", names(vlab)[duplicated(names(vlab))], "\n")
      }else{
        x <- factor(x, levels = as.numeric(vlab), labels = names(vlab))
        x <- as.character(x)
      }
    }
    
  }else{
    message(paste0("Values = ",length(unique(x))," Lables = None reurning number"))
    # No lables so just return the values  
  }
  return(x)
}


infld <- "D:/Users/earmmor/OneDrive - University of Leeds/Glider - Private/WP2/Data/EHS/EHS-2013-SPSS/UKDA-7802-spss/spss/spss19/"
#infld <- "C:/Users/earmmor/OneDrive/OD/Glider - Private/WP2/Data/EHS/EHS-2013-SPSS/UKDA-7802-spss/spss/spss19/"
###############################################################################
#Physical Table
##############################################################################
physical <- read.spss(paste0(infld,"derived/physical_12and13.sav"),to.data.frame=TRUE, use.value.labels = FALSE)
physical <- lapply(physical, Int2Factor)
physical <- as.data.frame(physical, stringsAsFactors = FALSE)
physical <- physical[,c("aacode","dwtypenx","dwage9x","floorx","floor5x","storeyx","typerstr",
                        "typewstr2","constx","typewfin","typewin","dblglaz4","arnatx","attic",
                        "basement","heat7x","sysage","mainfuel","watersys","boiler",
                        "wallinsy","sap12","cstactcx")]
physical$aacode <- as.character(physical$aacode)
#############################################################################
#General Table
###########################################################################
general <- read.spss(paste0(infld,"derived/general_12and13.sav"),to.data.frame=TRUE, use.value.labels = FALSE)
general <- lapply(general, Int2Factor)
general <- as.data.frame(general, stringsAsFactors = FALSE)
general <- general[,c("aacode","aagpd1213","tenure4x","GorEHCS","imd1010")]
names(general) <- c("aacode","aagpd1213","tenure4x","GorEHCS","imd")
general$aacode <- as.character(general$aacode)
##########################################################################
#elevate Table
####################################################################
elevate <- read.spss(paste0(infld,"physical/elevate.sav"),to.data.frame=TRUE, use.value.labels = FALSE)
elevate <- lapply(elevate, Int2Factor)
elevate <- as.data.frame(elevate, stringsAsFactors = FALSE)
elevate <- elevate[,c( "aacode","Felsolff","Felpvff",
                                "Felsollf","Felpvlf",
                                "Felsolrf","Felpvrf",
                                "Felsolbf","Felpvbf",
                                "felroofp","Fvwpvbf","Fvwpvlf","Fvwpvrf","Fvwpvff")]
elevate$aacode <- as.character(elevate$aacode)
############################################################################
#Services Table
############################################################################
services <- read.spss(paste0(infld,"physical/services.sav"),to.data.frame=TRUE, use.value.labels = FALSE)
services <- lapply(services, Int2Factor)
services <- as.data.frame(services, stringsAsFactors = FALSE)
services <- services[,c("aacode","Finchtyp","Finmhfue","Finmhboi","Finchbag",
                        "Finchoff","Finchthe","Finchtim","Finchove","Finchrom","Finchcon","Finchtrv","Finchtzc","Finchdst",
                        "Finoheat","Finohtyp",
                        "Finwhoac","Finwhxpr","Finwhxty","Finwhxag",
                        "Finwsipr","Finwsiag","Finwdipr","Finwdiag",
                        "Finwsppr","Finwspty","Finwspag",
                        "Finwmppr","Finwmpty","Finwmpag",
                        "Finwhlpr","Finwhlag",
                        "Finwhcyl","Finwhins","Finwhmms",
                        "Finwhcen","Finwhthe","Finlopos","Flitypes","Fliinsul","Finintyp","Flithick")]
services$aacode <- as.character(services$aacode)

##########################################################################
#dimensions table
#######################################################################

dimensions <- read.spss(paste0(infld,"derived/detailed/dimensions_12and13.sav"),to.data.frame=TRUE, use.value.labels = FALSE)
dimensions <- lapply(dimensions, Int2Factor)
dimensions <- as.data.frame(dimensions, stringsAsFactors = FALSE)


dimensions$wall.area.ext <- round(dimensions$efwalar +  dimensions$ebwalar, 2)
dimensions$window.area.ext <- round(dimensions$efwinar +  dimensions$ebwinar, 2)
dimensions$dpc.perim <- round(dimensions$efdpcpe +  dimensions$ebdpcpe, 2)
dimensions$roof.area.plan <- round(dimensions$efrfpar +  dimensions$ebrfpar, 2)
dimensions$roof.area.slope <- round(dimensions$efrfsar +  dimensions$ebrfsar, 2)
#dimensions$floor.area.ground <- round(lowarea, 2)
dimensions <- dimensions[,c("Aacode","FloorArea","FloorPar","IntWalAr","wall.area.ext","window.area.ext","dpc.perim","NFlorm","roof.area.plan","roof.area.slope","cheight0","cheight1","cheight2","AvWallThick","efwalar","ebwalar","efwinar","ebwinar","lowarea")]
names(dimensions) <- c("aacode","FloorAreaTotal","FloorAreaPartitionWalls","InternalWallArea","wall.area.ext","window.area.ext","dpc.perim","NumberofFloors","roof.area.plan","roof.area.slope","cheight0","cheight1","cheight2","AvWallThick","ExternalFrontWallArea","ExternalBackWallArea","ExternalFrontWindowArea ","ExternalBackWindowArea ","AreaOfLowestLevel")

#########################################################################
#Combine
########################################################################
combined <- left_join(physical,general, by = "aacode")
combined <- left_join(combined,services, by = "aacode")
combined <- left_join(combined,elevate, by = "aacode")

remove(general,services,elevate)

#######################################################################
#roof table
########################################################################
roof <- combined[,c("aacode","typerstr","Flitypes","Fliinsul","Finintyp","Flithick","attic",
                    "Felsolff","Felpvff",
                    "Felsollf","Felpvlf",
                    "Felsolrf","Felpvrf",
                    "Felsolbf","Felpvbf",
                    "felroofp","Fvwpvbf","Fvwpvlf","Fvwpvrf","Fvwpvff",
                    "floorx","storeyx","dwtypenx")]

#Remove NAs
rem_na <- function(col){
  for(x in 1:length(col)){
    if(is.na(col[x])){
      col[x]<- "No"
    }
  } 
  return(col)
}

roof$Felpvff <- rem_na(roof$Felpvff)
roof$Felpvbf <- rem_na(roof$Felpvbf)
roof$Felpvlf <- rem_na(roof$Felpvlf)
roof$Felpvrf <- rem_na(roof$Felpvrf)
roof$Felsolff <- rem_na(roof$Felsolff)
roof$Felsolbf <- rem_na(roof$Felsolbf)
roof$Felsollf <- rem_na(roof$Felsollf)
roof$Felsolrf <- rem_na(roof$Felsolrf)
roof$Fvwpvff <- rem_na(roof$Fvwpvff)
roof$Fvwpvbf <- rem_na(roof$Fvwpvbf)
roof$Fvwpvlf <- rem_na(roof$Fvwpvlf)
roof$Fvwpvrf <- rem_na(roof$Fvwpvrf)


#Summarise PV
roof$PV <- NA
for(i in 1:nrow(roof)){
  if((roof$Felpvff[i] == "Yes") || (roof$Felpvbf[i] == "Yes") || (roof$Felpvlf[i] == "Yes") || (roof$Felpvrf[i] == "Yes")){
    roof$PV[i] <- "Yes"
  }else{
    roof$PV[i] <- "No"
    }
}

#Summarise Solar
roof$Solar <- NA
for(j in 1:nrow(roof)){
  if((roof$Felsolff[j] == "Yes") || (roof$Felsolbf[j] == "Yes") || (roof$Felsollf[j] == "Yes") || (roof$Felsolrf[j] == "Yes")){
    roof$Solar[j] <- "Yes"
  }else{
    roof$Solar[j] <- "No"
  }
}

#Summarise Suitability
roof$SolarSuit <- NA
for(k in 1:nrow(roof)){
  if((roof$Fvwpvff[k] == "Yes") || (roof$Fvwpvbf[k] == "Yes") || (roof$Fvwpvlf[k] == "Yes") || (roof$Fvwpvrf[k] == "Yes")){
    roof$SolarSuit [k] <- "Yes"
  }else{
    roof$SolarSuit [k] <- "No"
  }
}

#Summaries Loft Insulation
#Standerside Thickenss and Materials 
roof$LoftIns <-NA
roof$Flithick <- as.character(roof$Flithick)
roof$Flithick <- rem_na(roof$Flithick)

for(b in 1:nrow(roof)){
  if(roof$Flithick[b] == "300mm" | roof$Flithick[b] == ">300mm" | roof$Flithick[b] == "250mm"){
    roof$LoftIns[b] <-"Well Insulated"
  } else if (roof$Flithick[b] == "25mm" | roof$Flithick[b] == "50mm" | roof$Flithick[b] == "75mm" | roof$Flithick[b] == "100mm" | roof$Flithick[b] == "125mm" | roof$Flithick[b] == "150mm" | roof$Flithick[b] == "200mm"){
    roof$LoftIns[b] <-"Poorly Insulated"
  } else {
    roof$LoftIns[b] <-"No Insulation"
  }
}



#Plot
counts <- table(roof$LoftIns)
barplot(counts, main="Loft Insualtion", ylab="Number of Dwellings")

#Calcualte Roof Area
roof$roofarea <- NA
for(e in 1:nrow(roof)){
  if(roof$dwtypenx[e] == "end terrace" | roof$dwtypenx[e] == "mid terrace"|roof$dwtypenx[e] == "semi detached"|roof$dwtypenx[e] == "detached"|roof$dwtypenx[e] == "bungalow"){
    roof$roofarea[e] <- roof$floorx[e] / roof$storeyx[e]
  } else {
    roof$roofarea[e] <- roof$floorx[e]
  }
}

hist(roof$roofarea, c(0,20,40,60,80,100,120,450))
solfacts <- read.csv("../jobs_data/solarrooffactors.csv", row.names = c("end terrace","mid terrace","semi detached","detached","bungalow","converted flat","purpose built flat, low rise","purpose built flat, high rise"), col.names = c("mixed types","pitched",	"mansard",	"flat",	"chalet"), header = F)
names(solfacts) <- c("mixed types","pitched",	"mansard",	"flat",	"chalet")

#Calcualte Useable Area for solar
roof$solfactor <- NA
for(f in 1:nrow(roof)){
  roof$solfactor[f] <- solfacts[roof$dwtypenx[f],roof$typerstr[f]]
}
roof$solarea5 <- NA
roof$solarea <- roof$roofarea * roof$solfactor
for(e in 1:nrow(roof)){
  x <- roof$solarea[e]
  if(x < 10){
    roof$solarea5[e] <- "<10 sqm"
  }else if(x >= 10 & x < 15){
    roof$solarea5[e] <- "10 to 15 sqm"
  }else if(x >= 15 & x < 20){
    roof$solarea5[e] <- "15 to 20 sqm"
  }else if(x >= 20 & x < 25){
    roof$solarea5[e] <- "20 to 25 sqm"
  }else if(x >= 25 & x < 30){
    roof$solarea5[e] <- "25 to 30 sqm"
  }else if(x >= 30 & x <= 35){
    roof$solarea5[e] <- "30 to 35 sqm"
  }else{
    roof$solarea5[e] <- ">35 sqm"
  }
}
roof$solarea5 <- as.factor(roof$solarea5)
#Plot
counts <- table(roof$solarea5)
barplot(counts, main="Roof area for solar pannels", ylab="Number of Dwellings")



roof <- roof[,c("aacode","typerstr","attic","PV","Solar","SolarSuit","LoftIns","solarea","solarea5")]

##########################################################################
#Walls Table
###########################################################################
walls <- combined[,c("aacode","typewstr2","wallinsy")]



#Plot
counts <- table(walls$wallinsy)
barplot(counts, main="External Wall Insualtion", ylab="Number of Dwellings")

############################################################################################
#Shape Table
###########################################################################################
shape <- combined[,c("aacode","dwtypenx","dwage9x","floorx","floor5x","storeyx","basement","Finlopos")]
#shape$type <- as.factor(paste0(shape$Finlopos," ",shape$dwtypenx))
#summary(shape$type)
shape$type <- NA
for (c in 1:nrow(shape)){
  if(shape$Finlopos[c] == "Basement Flat"){
    shape$type[c] <- paste0("Basement ",shape$dwtypenx[c])
  }else if(shape$Finlopos[c] == "Mid Floor Flat"){
    shape$type[c] <- paste0("Mid Floor ",shape$dwtypenx[c])
  }else if(shape$Finlopos[c] == "Top Floor Flat"){
    shape$type[c] <- paste0("Top Floor ",shape$dwtypenx[c])
  }else if(shape$Finlopos[c] == "Ground floor flat"){
    shape$type[c] <- paste0("Ground floor ",shape$dwtypenx[c])
  }else if(shape$Finlopos[c] == "House/Bungalow" & shape$dwtypenx[c] == "bungalow"){
    shape$type[c] <- paste0(shape$dwtypenx[c])
  }else if(shape$Finlopos[c] == "House/Bungalow" & shape$dwtypenx[c] != "bungalow"){
    shape$type[c] <- paste0(shape$dwtypenx[c])
  }
}
shape$type <- as.factor(shape$type)


#Plot
counts <- table(shape$type)
barplot(counts, main="Building Type", ylab="Number of Dwellings",las=2)


#Create an exact age
shape$age <- NA
for(r in 1:nrow(shape)){
  if(shape$dwage9x[r] == "pre 1850"){
    shape$age[r] <- 1849
  }else if (shape$dwage9x[r] == "1850 to 1899"){
    shape$age[r] <- sample(1850:1899, 1)
  }else if (shape$dwage9x[r] == "1900 to 1918"){
    shape$age[r] <- sample(1900:1918, 1)
  }else if (shape$dwage9x[r] == "1919 to 1944"){
    shape$age[r] <- sample(1919:1944, 1)
  }else if (shape$dwage9x[r] == "1945 to 1964"){
    shape$age[r] <- sample(1945:1964, 1)
  }else if (shape$dwage9x[r] == "1965 to 1974"){
    shape$age[r] <- sample(1965:1974, 1)
  }else if (shape$dwage9x[r] == "1975 to 1980"){
    shape$age[r] <- sample(1975:1980, 1)
  }else if (shape$dwage9x[r] == "1981 to 1990"){
    shape$age[r] <- sample(1981:1990, 1)
  }else if (shape$dwage9x[r] == "post 1990"){
    shape$age[r] <- sample(1990:2013, 1)
  }else {
    shape$age[r] <- 0
  }
}

#Remove Unneeded Columns
rems <- !names(shape) %in% c("Finlopos")
shape <- shape[,rems]

############################################################################
#Context Table
###########################################################################
context <- combined[c("aacode","aagpd1213","tenure4x","GorEHCS","imd","arnatx","sap12")]



#########################################################################
#Energy Table
##########################################################################
energy <- combined[c("aacode","Finchtyp","mainfuel","Finmhboi","Finchbag","sysage","watersys",
                     "Finchoff","Finchthe","Finchtim","Finchove","Finchrom","Finchcon","Finchtrv","Finchtzc","Finchdst",
                     "Finwsipr","Finwsiag","Finwdipr","Finwdiag",
                     "Finwsppr",
                     "Finwmppr",
                     "Finwhcyl","Finwhins","Finwhmms"
                     )]

#Remove Nulls from Controls

energy$Finchoff <- rem_na(energy$Finchoff) #On/Off
energy$Finchtim <- rem_na(energy$Finchtim) #central timer
energy$Finchthe <- rem_na(energy$Finchthe) #Boiler Thermostat
energy$Finchove <- rem_na(energy$Finchove) #Manual Overide
energy$Finchrom <- rem_na(energy$Finchrom) #Room Thermostat
energy$Finchcon <- rem_na(energy$Finchcon) #Radiator Control
energy$Finchtrv <- rem_na(energy$Finchtrv) #Thermostatic Radiator Conrol
energy$Finchtzc <- rem_na(energy$Finchtzc) #Time and Temp Zone
energy$Finchdst <- rem_na(energy$Finchdst) #Delayed Start

energy$control <- NA

# Energy Control Levle cumulative number of controls had
#L7 - Delay Start, L6 - Time and temp zones, L5 - Room Thermostat, L4 - Boiler Thermostat, L3 - Manual Overide, L2 - Central Timer, L1 - On/Off, L0 - None, L9 - Other combination

for(l in 1:nrow(energy)){
  if(energy$Finchtim[l] == "Yes" && energy$Finchdst[l] == "Yes" && energy$Finchtzc[l] == "Yes" && energy$Finchrom[l] == "Yes" && energy$Finchthe[l] == "Yes" && energy$Finchove[l] == "Yes" && energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 7
  } else if (energy$Finchtim[l] == "Yes" && energy$Finchtzc[l] == "Yes" && energy$Finchrom[l] == "Yes" && energy$Finchthe[l] == "Yes" && energy$Finchove[l] == "Yes" && energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 6
  } else if (energy$Finchtim[l] == "Yes" && energy$Finchrom[l] == "Yes" && energy$Finchthe[l] == "Yes" && energy$Finchove[l] == "Yes" && energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 5
  } else if (energy$Finchtim[l] == "Yes" && energy$Finchthe[l] == "Yes" && energy$Finchove[l] == "Yes" && energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 4
  } else if (energy$Finchtim[l] == "Yes" && energy$Finchove[l] == "Yes" && energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 3
  } else if (energy$Finchtim[l] == "Yes" && energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 2
  } else if (energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 1    
  } else if (energy$Finchtim[l] == "No" && energy$Finchdst[l] == "No" && energy$Finchtzc[l] == "No" && energy$Finchrom[l] == "No" && energy$Finchthe[l] == "No" && energy$Finchove[l] == "No" && energy$Finchoff[l] == "No"){
    energy$control[l] <- 0
  } else {
    energy$control[l] <- 9
  }
}

counts <- table(energy$control)
barplot(counts, main="Control Level", ylab="Number of Dwellings")

#Radiator Control Level
#L2 - TRVs, L1 - Controls, L0 - None

energy$radcontrol <- NA
for(m in 1:nrow(energy)){
  if(energy$Finchtrv[m] == "Yes"){
    energy$radcontrol[m] <- 2
  } else if (energy$Finchtrv[m] == "No" && energy$Finchcon[m] == "Yes"){
    energy$radcontrol[m] <- 1
  } else {
    energy$radcontrol[m] <- 0
  }
}

#Plot
counts <- table(energy$radcontrol)
barplot(counts, main="Radiator Control Level", ylab="Number of Dwellings")


#Remove Unneeded Columns
rems <- !names(energy) %in% c("Finchtim","Finchoff","Finchthe","Finchove","Finchrom","Finchcon","Finchtrv","Finchtzc","Finchdst")
energy <- energy[,rems]

#Simplify Water Cylinder and Imersion Heater

energy$Finwsipr <- rem_na(energy$Finwsipr) #Single Immersion
energy$Finwdipr <- rem_na(energy$Finwdipr) #Dual Immersion
energy$Finwhcyl <- rem_na(energy$Finwhcyl) #Water Cyclinder

energy$tank <-NA
energy$immersage <- NA
for(n in 1:nrow(energy)){
  if((energy$Finwsipr[n] == "Yes" || energy$Finwdipr[n] == "Yes") && energy$Finwhcyl[n] == "Yes"){
    energy$tank[n] <- "Tank with Immersion Heater"
    if(is.na(energy$Finwsiag[n])){
      energy$immersage[n] <- energy$Finwdiag[n]
    }else{
      energy$immersage[n] <- energy$Finwsiag[n]
    }
  }else if((energy$Finwsipr[n] == "No" && energy$Finwdipr[n] == "No") && energy$Finwhcyl[n] == "Yes"){
    energy$tank[n] <- "Tank without Immersion Heater"
  }else{
    energy$tank[n] <- "No Tank"
  }
}

#Remove Unneeded Columns
rems <- !names(energy) %in% c("Finwsipr","Finwdipr","Finwhcyl","Finwdiag","Finwsiag")
energy <- energy[,rems]

#Plot
counts <- table(energy$tank)
barplot(counts, main="Water Tank Type", ylab="Number of Dwellings")

#Simplify Tank insualtion
energy$tankins <- NA
energy$Finwhmms <- as.character(energy$Finwhmms)
energy$Finwhmms <- rem_na(energy$Finwhmms)
for(c in 1:nrow(energy)){
  if(energy$tank[c] == "No Tank"){
    energy$tankins[c] <-"No Tank"
  } else if (energy$Finwhmms[c] == "150 mm" | energy$Finwhmms[c] == "100 mm" | energy$Finwhmms[c] == "80 mm"){
    energy$tankins[c] <-"Well Insulated"
  } else if (energy$Finwhmms[c] == "12.5mm" | energy$Finwhmms[c] == "25 mm" | energy$Finwhmms[c] == "38 mm" | energy$Finwhmms[c] == "50 mm"){
    energy$tankins[c] <-"Poorly Insulated"
  } else {
    energy$tankins[c] <-"No Insulation"
  }
}

#Plot
counts <- table(energy$tankins)
barplot(counts, main="Water Tank Type", ylab="Number of Dwellings")

#Remove Unneeded Columns
rems <- !names(energy) %in% c("Finwhmms","Finwhins")
energy <- energy[,rems]


#Simplify Instaneous Heater
energy$Finwmppr <- rem_na(energy$Finwmppr) #Mulitpoint
energy$Finwsppr <- rem_na(energy$Finwsppr) #Single Point

energy$instant <- NA
for(o in 1:nrow(energy)){
  if(energy$Finwmppr[o] == "Yes" && energy$Finwsppr[o] == "Yes"){
    energy$instant[o] <- "Yes"
  }else if(energy$Finwmppr[o] == "No" && energy$Finwsppr[o] == "Yes"){
    energy$instant[o] <- "Yes"
  }else if(energy$Finwmppr[o] == "Yes" && energy$Finwsppr[o] == "No"){
    energy$instant[o] <- "Yes"
  }else{
    energy$instant[o] <- "No"
  }
}

#Remove Unneeded Columns
rems <- !names(energy) %in% c("Finwmppr","Finwsppr")
energy <- energy[,rems]

#Plot
counts <- table(energy$instant)
barplot(counts, main="Instaneous Water Heater", ylab="Number of Dwellings")


levels(energy$Finchtyp) <- c(levels(energy$Finchtyp),"No Answer")

#Clean Main ones
for(q in 1:nrow(energy)){
  if(is.na(energy$Finmhboi[q])){
    energy$Finmhboi[q] <- "No boiler"
  }
  if(is.na(energy$Finchtyp[q])){
    energy$Finchtyp[q] <- "No Answer"
  }
  
}

#Clean Fuels
energy$mainfuel <- as.character(energy$mainfuel)
for(g in 1:nrow(energy)){
  if(energy$mainfuel[g] == "bulk LPG" | energy$mainfuel[g] == "bottled gas - propane"){
    energy$mainfuel[g] <- "gas (other)"
  }else if(energy$mainfuel[g] == "house coal" | energy$mainfuel[g] == "smokeless fuel" | energy$mainfuel[g] == "anthracite nuts" | energy$mainfuel[g] == "anthracite grains"){
    energy$mainfuel[g] <- "coal"
  }else if(energy$mainfuel[g] =="electricity (7 hr. on peak)" | energy$mainfuel[g] == "electricity (7 hr. off peak)" | energy$mainfuel[g] ==  "electricity (standard tariff)" | energy$mainfuel[g] ==  "electricity (10 hr. on peak)" | energy$mainfuel[g] == "electricity (10 hr. off peak)" | energy$mainfuel[g] == "electricity (24 hr heating tariff)"){
    energy$mainfuel[g] <- "electricity"
  }
}
energy$mainfuel <- as.factor(energy$mainfuel)

#Plot
counts <- table(energy$mainfuel)
barplot(counts, main="Heater Fuel", ylab="Number of Dwellings")

#Mulit-row per dwelling tables
######################################################################
#Doors Table
######################################################################
#doors <- read.spss(paste0(infld,"physical/doors.sav"),to.data.frame=TRUE)
doors <- read.spss(paste0(infld,"physical/doors.sav"),to.data.frame=TRUE, use.value.labels = FALSE)
doors <- lapply(doors, Int2Factor)
doors <- as.data.frame(doors, stringsAsFactors = FALSE)



doors <- doors[,c("aacode","type","Fexdf1no","Fexdf1ag",
                                  "Fexdf2no","Fexdf2ag")]
doors <- doors[doors$Fexdf1no > 0 | doors$Fexdf2no > 0 ,]
doors$number <- doors$Fexdf1no + doors$Fexdf2no
summary((doors$Fexdf1ag != doors$Fexdf2ag) & (doors$Fexdf1no != 0 | doors$Fexdf2no != 0)) #2/3 have the same age front and back when accounting for only on front or back

#Remove NAs
rem_na_0 <- function(col){
  for(x in 1:length(col)){
    if(is.na(col[x])){
      col[x]<- 0
    }
  } 
  return(col)
}
doors$Fexdf1ag <- rem_na_0(doors$Fexdf1ag)
doors$Fexdf2ag <- rem_na_0(doors$Fexdf2ag)
doors$age <- pmax(doors$Fexdf1ag,doors$Fexdf2ag)

#Remove Unneeded Columns
rems <- !names(doors) %in% c("Fexdf1ag","Fexdf2ag","Fexdf1no","Fexdf2no")
doors <- doors[,rems]
doors <- reshape(doors, direction = "wide", idvar="aacode", timevar="type")

doors$number.Wood <- rem_na_0(doors$number.Wood)
doors$number.UPVC <- rem_na_0(doors$number.UPVC)
doors$number.Metal <- rem_na_0(doors$number.Metal)
doors$age.Wood <- rem_na_0(doors$age.Wood)
doors$age.UPVC <- rem_na_0(doors$age.UPVC)
doors$age.Metal <- rem_na_0(doors$age.Metal)

doors$doornumb <- doors$number.Wood + doors$number.UPVC + doors$number.Metal
doors$doorage <- pmax(doors$age.Wood, doors$age.UPVC, doors$age.Metal)
doors <- doors[,c("aacode","doornumb","doorage")]

################################################################################
#Windows Table
#############################################################################
#windows <- read.spss(paste0(infld,"physical/windows.sav"),to.data.frame=TRUE)
windows <- read.spss(paste0(infld,"physical/windows.sav"),to.data.frame=TRUE, use.value.labels = FALSE)
windows <- lapply(windows, Int2Factor)
windows <- as.data.frame(windows, stringsAsFactors = FALSE)



windows <- windows[,c("aacode","type","Fexwn1no","Fexwn1ag",
                                      "Fexwn2no","Fexwn2ag")]
windows <- windows[windows$Fexwn1no > 0 | windows$Fexwn2no > 0,]

windows$numb <- windows$Fexwn1no + windows$Fexwn2no
summary((windows$Fexwn1ag != windows$Fexwn2ag) & (windows$Fexwn1no != 0 | windows$Fexwn2no != 0)) #2/3 have the same age front and back when accounting for only on front or back

windows$Fexwn1ag <- rem_na_0(windows$Fexwn1ag)
windows$Fexwn2ag <- rem_na_0(windows$Fexwn2ag)
windows$age <- pmax(windows$Fexwn1ag,windows$Fexwn2ag)

#Remove Unneeded Columns
rems <- !names(windows) %in% c("Fexwn1ag","Fexwn2ag","Fexwn1no","Fexwn2no")
windows <- windows[,rems]
windows <- reshape(windows, direction = "wide", idvar="aacode", timevar="type")

windows$`numb.Double-glazed- wood`  <- rem_na_0(windows$`numb.Double-glazed- wood`)  
windows$`age.Double-glazed- wood` <- rem_na_0(windows$`age.Double-glazed- wood`)
windows$`numb.Double-glazed- UPVC` <- rem_na_0(windows$`numb.Double-glazed- UPVC`)
windows$`age.Double-glazed- UPVC` <- rem_na_0(windows$`age.Double-glazed- UPVC`)
windows$`numb.Single-glazed- UPVC` <- rem_na_0(windows$`numb.Single-glazed- UPVC`)
windows$`age.Single-glazed- UPVC` <- rem_na_0(windows$`age.Single-glazed- UPVC`)
windows$`numb.Single-glazed- wood casement` <- rem_na_0(windows$`numb.Single-glazed- wood casement`)
windows$`age.Single-glazed- wood casement`  <- rem_na_0(windows$`age.Single-glazed- wood casement`)
windows$`numb.Double-glazed- metal`  <- rem_na_0(windows$`numb.Double-glazed- metal`)
windows$`age.Double-glazed- metal`  <- rem_na_0(windows$`age.Double-glazed- metal`)
windows$`numb.Single-glazed- wood sash` <- rem_na_0(windows$`numb.Single-glazed- wood sash`)
windows$`age.Single-glazed- wood sash` <- rem_na_0(windows$`age.Single-glazed- wood sash`)
windows$`numb.Single-glazed- metal` <- rem_na_0(windows$`numb.Single-glazed- metal`)
windows$`age.Single-glazed- metal` <- rem_na_0(windows$`age.Single-glazed- metal`)

windows$dblglaze <- windows$`numb.Double-glazed- wood` + windows$`numb.Double-glazed- UPVC` + windows$`numb.Double-glazed- metal`
windows$dblglazeage <- pmax(windows$`age.Double-glazed- wood`,windows$`age.Double-glazed- UPVC`,windows$`age.Double-glazed- metal`)
windows$sngglaze <- windows$`numb.Single-glazed- UPVC` + windows$`numb.Single-glazed- wood casement` + windows$`numb.Single-glazed- wood sash` + windows$`numb.Single-glazed- metal`
windows$sngglazeage <- pmax(windows$`age.Single-glazed- UPVC`,windows$`age.Single-glazed- wood casement`,windows$`age.Single-glazed- wood sash`,windows$`age.Single-glazed- metal`)

#Remove Unneeded Columns
windows <- windows[,c("aacode","dblglaze","dblglazeage","sngglaze","sngglazeage")]


################################################################
# Windows part 2
##################################################################

windows2 <- physical[,c("aacode","dblglaz4")]


###############################################################
# Rooms Table
##############################################################
#rooms <- read.spss(paste0(infld,"interview/rooms.sav"),to.data.frame=TRUE)
rooms <- read.spss(paste0(infld,"interview/rooms.sav"),to.data.frame=TRUE, use.value.labels = FALSE)
rooms <- lapply(rooms, Int2Factor)
rooms <- as.data.frame(rooms, stringsAsFactors = FALSE)

rooms <- rooms[,c("aacode","NRmsEHS","NRms4","NRms5")]
names(rooms) <- c("aacode","NBedrooms","NLivingRooms","NBathrooms")



#####################################################################
#Join back into a master table
###################################################################
combined_2013 <- left_join(context,shape, by = "aacode")
combined_2013 <- left_join(combined_2013,walls, by = "aacode")
combined_2013 <- left_join(combined_2013,roof, by = "aacode")
combined_2013 <- left_join(combined_2013,windows, by = "aacode")
combined_2013 <- left_join(combined_2013,windows2, by = "aacode")
combined_2013 <- left_join(combined_2013,doors, by = "aacode")
combined_2013 <- left_join(combined_2013,energy, by = "aacode")
combined_2013 <- left_join(combined_2013,dimensions, by = "aacode")


combined_2013$LoftIns <- as.factor(combined_2013$LoftIns)

remove(context,doors,roof,shape, walls, windows)

#Gas Available

combined_2013$gasgrid <- NA

for (f in 1:nrow(combined_2013)){
  if(combined_2013$mainfuel[f] == "gas (mains)"){
    combined_2013$gasgrid[f] <- "yes"
  }else if(combined_2013$arnatx[f] == "city centre" | combined_2013$arnatx[f] == "other urban centre"| combined_2013$arnatx[f] == "suburban residential"){
    combined_2013$gasgrid[f] <- "yes"
  }else{
    combined_2013$gasgrid[f] <- "no"
  }
}


#Windows Age

combined_2013$winage <- NA

for(i in 1:nrow(combined_2013)){
  if(combined_2013$dblglazeage[i] >= 40){
    combined_2013$winage[i] <- "40 plus years"
  }else if(combined_2013$dblglazeage[i] >= 30 & combined_2013$dblglazeage[i] < 40){
    combined_2013$winage[i] <- "30 - 40 years"
  }else if(combined_2013$dblglazeage[i] >= 20 & combined_2013$dblglazeage[i] < 30){
    combined_2013$winage[i] <- "20 - 30 years"
  }else if(combined_2013$dblglazeage[i] >= 10 & combined_2013$dblglazeage[i] < 20){
    combined_2013$winage[i] <- "10 - 20 years"
  }else if(combined_2013$dblglazeage[i] >= 0 & combined_2013$dblglazeage[i] < 10){
    combined_2013$winage[i] <- "0 -10 years"
  }
}
combined_2013$winage <- as.factor(combined_2013$winage)

#Floor Types
#https://fet.uwe.ac.uk/conweb/house_ages/elements/section3.htm
combined_2013$floorcons <- NA
for(i in 1:nrow(combined_2013)){
  if(combined_2013$dwage9x[i] == "pre 1850"){
    combined_2013$floorcons[i] <- "Historic"
  }else if(combined_2013$dwage9x[i] == "1850 to 1899" | combined_2013$dwage9x[i] == "1900 to 1918" | combined_2013$dwage9x[i] == "1919 to 1944"){
    combined_2013$floorcons[i] <- "Suspended Timber Floor - Uninsulated"
  }else if(combined_2013$dwage9x[i] == "1945 to 1964" | combined_2013$dwage9x[i] == "1965 to 1974" | combined_2013$dwage9x[i] == "1975 to 1980" | combined_2013$dwage9x[i] == "1981 to 1990"){
    combined_2013$floorcons[i] <- "Concrete Floors - Uninsulated"
  }else if(combined_2013$dwage9x[i] == "post 1990"){
    combined_2013$floorcons[i] <- "Concrete Floors - Insulated"
  }else{
    combined_2013$floorcons[i] <- "Unknown"
  }
}

#Fix missing doors

combined_2013$doornumb[is.na(combined_2013$doornumb)] <- 0


#turn imd into numbers
combined_2013$imd <- as.character(combined_2013$imd)
combined_2013$imd[combined_2013$imd == "most deprived 10% of areas"] <- 1
combined_2013$imd[combined_2013$imd == "2nd"] <- 2
combined_2013$imd[combined_2013$imd == "3rd"] <- 3
combined_2013$imd[combined_2013$imd == "4th"] <- 4
combined_2013$imd[combined_2013$imd == "5th"] <- 5
combined_2013$imd[combined_2013$imd == "6th"] <- 6
combined_2013$imd[combined_2013$imd == "7th"] <- 7
combined_2013$imd[combined_2013$imd == "8th"] <- 8
combined_2013$imd[combined_2013$imd == "9th"] <- 9
combined_2013$imd[combined_2013$imd == "least deprived 10% of areas"] <- 10

combined_2013$imd <- as.integer(combined_2013$imd)


#Save Out

saveRDS(combined_2013,"../jobs_data/combined_2013_base.Rds")




