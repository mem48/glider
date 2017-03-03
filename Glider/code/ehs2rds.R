#Read in EHS and convert to RDS
library(foreign)
library(dplyr, lib.loc = "M:/R/R-3.3.1/library")

infld <- "C:/Users/earmmor/OneDrive/OD/Glider - Private/WP2/Data/EHS/EHS-2013-SPSS/UKDA-7802-spss/spss/spss19/"

#Physical Table
physical <- read.spss(paste0(infld,"derived/physical_12and13.sav"),to.data.frame=TRUE)
physical <- physical[,c("aacode","dwtypenx","dwage9x","floorx","floor5x","storeyx","typerstr",
                        "typewstr2","constx","typewfin","typewin","dblglaz4","arnatx","attic",
                        "basement","heat7x","heatsec","sysage","mainfuel","watersys","boiler",
                        "loftins6","wallinsy","wallcavy","sap12")]

#General Table
general <- read.spss(paste0(infld,"derived/general_12and13.sav"),to.data.frame=TRUE)
general <- general[,c("aacode","tenure4x","vacantx","GorEHCS","rumorph","rucombin","imd1010")]

#elevate Table
elevate <- read.spss(paste0(infld,"physical/elevate.sav"),to.data.frame=TRUE)
elevate <- elevate[,c( "aacode","Felsolff","Felpvff","Felcavff","Felextff",
                                "Felsollf","Felpvlf","Felcavlf","Felextlf",
                                "Felsolrf","Felpvrf","Felcavrf","Felextrf",
                                "Felsolbf","Felpvbf","Felcavbf","Felextbf", 
                                "felroofp","Fvwpvbf","Fvwpvlf","Fvwpvrf","Fvwpvff")]

#Services Table
services <- read.spss(paste0(infld,"physical/services.sav"),to.data.frame=TRUE)
services <- services[,c("aacode","Finchtyp","Findisty","Finmhfue","Finmhboi","Finchbag",
                        "Finchoff","Finchthe","Finchtim","Finchove","Finchrom","Finchcon","Finchtrv","Finchtzc","Finchdst",
                        "Finoheat","Finohphs","Finohtyp",
                        "Finwhoty","Finwhoac","Finwhoag","Finwhxpr","Finwhxty","Finwhxag",
                        "Finwsipr","Finwsiag","Finwdipr","Finwdiag",
                        "Finwsppr","Finwspty","Finwspag",
                        "Finwmppr","Finwmpty","Finwmpag",
                        "Finwhlpr","Finwhlty","Finwhlag",
                        "Finwhcyl","Finwhsiz","Finwhins","Finwhmms",
                        "Finwhcen","Finwhthe","Finlopos","Flitypes","Fliinsul","Finintyp","Flithick")]

combined <- left_join(physical,general, by = "aacode")
combined <- left_join(combined,services, by = "aacode")
combined <- left_join(combined,elevate, by = "aacode")
remove(physical,general,services,elevate)

roof <- combined[,c("aacode","typerstr","Flitypes","Fliinsul","Finintyp","Flithick","attic",
                    "Felsolff","Felpvff",
                    "Felsollf","Felpvlf",
                    "Felsolrf","Felpvrf",
                    "Felsolbf","Felpvbf",
                    "felroofp","Fvwpvbf","Fvwpvlf","Fvwpvrf","Fvwpvff","loftins6")]

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
    roof$PV[i] <- TRUE
  }else{
    roof$PV[i] <- FALSE
    }
}

#Summarise Solar
roof$Solar <- NA
for(j in 1:nrow(roof)){
  if((roof$Felsolff[j] == "Yes") || (roof$Felsolbf[j] == "Yes") || (roof$Felsollf[j] == "Yes") || (roof$Felsolrf[j] == "Yes")){
    roof$Solar[j] <- TRUE
  }else{
    roof$Solar[j] <- FALSE
  }
}

#Summarise Suitability
roof$SolarSuit <- NA
for(k in 1:nrow(roof)){
  if((roof$Fvwpvff[k] == "Yes") || (roof$Fvwpvbf[k] == "Yes") || (roof$Fvwpvlf[k] == "Yes") || (roof$Fvwpvrf[k] == "Yes")){
    roof$SolarSuit [k] <- TRUE
  }else{
    roof$SolarSuit [k] <- FALSE
  }
}

roof <- roof[,c("aacode","typerstr","Flitypes","Fliinsul","Finintyp","Flithick","felroofp","attic","PV","Solar","SolarSuit","loftins6")]


#Walls Table
walls <- combined[,c("aacode","typewstr2","constx","typewfin",
                     "Felcavff","Felextff",
                     "Felcavlf","Felextlf",
                     "Felcavrf","Felextrf",
                     "Felcavbf","Felextbf", "wallinsy","wallcavy")]


#Shape Table
shape <- combined[,c("aacode","dwtypenx","dwage9x","floorx","floor5x","storeyx","basement","Finlopos")]
shape$type <- as.factor(paste0(shape$Finlopos," ",shape$dwtypenx))
summary(shape$type)


#Context Table
context <- combined[c("aacode","tenure4x","vacantx","GorEHCS","rumorph","rucombin","imd1010","arnatx")]


#Energy Table
energy <- combined[c("aacode","Finchtyp","Findisty","mainfuel","Finmhboi","Finchbag","heatsec","sysage","watersys",
                     "Finchoff","Finchthe","Finchtim","Finchove","Finchrom","Finchcon","Finchtrv","Finchtzc","Finchdst",
                     "Finohphs",
                     "Finwhoty","Finwhoag","Finwhxpr","Finwhxty","Finwhxag",
                     "Finwsipr","Finwsiag","Finwdipr","Finwdiag",
                     "Finwsppr",
                     "Finwmppr",
                     "Finwhlpr","Finwhlty","Finwhlag",
                     "Finwhcyl","Finwhsiz","Finwhins","Finwhmms",
                     "Finwhcen","Finwhthe"
                     
                     )]

#Remove Nulls from Controls

energy$Finchoff <- rem_na(energy$Finchoff) #On/Off
energy$Finchthe <- rem_na(energy$Finchthe) #Boiler Thermostat
energy$Finchove <- rem_na(energy$Finchove) #Manual Overide
energy$Finchrom <- rem_na(energy$Finchrom) #Room Thermostat
energy$Finchcon <- rem_na(energy$Finchcon) #Radiator Control
energy$Finchtrv <- rem_na(energy$Finchtrv) #Thermostatic Radiator Conrol
energy$Finchtzc <- rem_na(energy$Finchtzc) #Time and Temp Zone
energy$Finchdst <- rem_na(energy$Finchdst) #Delayed Start

energy$control <- NA

# Energy Control Levle cumulative number of controls had
#L6 - Delay Start, L5 - Time and temp zones, L4 - Room Thermostat, L3 - Boiler Thermostat, L2 - Manual Overide, L1 - On/Off, L0 - None, L9 - Other combination

for(l in 1:nrow(energy)){
  if(energy$Finchdst[l] == "Yes" && energy$Finchtzc[l] == "Yes" && energy$Finchrom[l] == "Yes" && energy$Finchthe[l] == "Yes" && energy$Finchove[l] == "Yes" && energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 6
  } else if (energy$Finchtzc[l] == "Yes" && energy$Finchrom[l] == "Yes" && energy$Finchthe[l] == "Yes" && energy$Finchove[l] == "Yes" && energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 5
  } else if (energy$Finchrom[l] == "Yes" && energy$Finchthe[l] == "Yes" && energy$Finchove[l] == "Yes" && energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 4
  } else if (energy$Finchthe[l] == "Yes" && energy$Finchove[l] == "Yes" && energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 3
  } else if (energy$Finchove[l] == "Yes" && energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 2
  } else if (energy$Finchoff[l] == "Yes"){
    energy$control[l] <- 1
  } else if (energy$Finchdst[l] == "No" && energy$Finchtzc[l] == "No" && energy$Finchrom[l] == "No" && energy$Finchthe[l] == "No" && energy$Finchove[l] == "No" && energy$Finchoff[l] == "No"){
    energy$control[l] <- 0
  } else {
    energy$control[l] <- 9
  }
}

hist(energy$control)

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
hist(energy$radcontrol)

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





test <- energy[,c("Finchtyp","Finmhboi","watersys","instant","tank")]
uni <- unique(test)
#Count the uniqes
uni$count <- NA
for(p in 1:nrow(uni)){
  uni$count[p] <- nrow(test[test$Finchtyp == uni$Finchtyp[p] & test$Finmhboi == uni$Finmhboi[p] & test$watersys == uni$watersys[p] & test$instant == uni$instant[p] & test$tank == uni$tank[p] ,])
}



write.csv(uni,"uni.csv")


#Simplify Water System
#for(p in 1:nrow(energy)){
#  if(energy$Finwhcpr[p] == "Yes" & energy$Finwhopr[p] == "No")
#}
















energy <- energy[c("aacode","Finchtyp","Findisty","mainfuel","Finmhboi","Finchbag","heatsec","sysage","watersys",
                   "Finohphs",
                   "Finwhcpr","Finwhopr","Finwhoty","Finwhoag","Finwhxpr","Finwhxty","Finwhxag",
                   "Finwhlpr","Finwhlty","Finwhlag",
                   "Finwhsiz","Finwhins","Finwhmms",
                   "Finwhcen","Finwhthe","control", "radcontrol","immersage","instant","tank"
                   
)]


#Mulit-row per dwelling tables

#Dormers Table
dormers <- read.spss(paste0(infld,"physical/dormers.sav"),to.data.frame=TRUE)
dormers <- dormers[,c("aacode","type","Fexdb1pr","Fexdb1no","Fexdb1ag", 
                                      "Fexdb2pr","Fexdb2no","Fexdb2ag")]
dormers <- dormers[dormers$Fexdb1pr == 1 | dormers$Fexdb2pr == 1,]

#Doors Table
doors <- read.spss(paste0(infld,"physical/doors.sav"),to.data.frame=TRUE)
doors <- doors[,c("aacode","type","Fexdf1no","Fexdf1ag",
                                  "Fexdf2no","Fexdf2ag")]
doors <- doors[doors$Fexdf1no > 0 | doors$Fexdf2no > 0 ,]



#Windows Table
windows <- read.spss(paste0(infld,"physical/windows.sav"),to.data.frame=TRUE)
windows <- windows[,c("aacode","type","Fexwn1no","Fexwn1ag",
                                      "Fexwn2no","Fexwn2ag")]
windows <- windows[windows$Fexwn1no > 0 | windows$Fexwn2no > 0,]



