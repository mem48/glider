#Read in EHS and convert to RDS
library(foreign)
library(dplyr, lib.loc = "M:/R/R-3.3.1/library")
library(lazyeval, lib.loc = "M:/R/R-3.3.1/library")
current_year <- 2011


infld <- "C:/Users/earmmor/OneDrive/OD/Glider - Private/WP2/Data/EHS/EHS-2011-SPSS/UKDA-7386-spss/spss/spss19/"
###############################################################################
#Physical Table - only partical match
##############################################################################
physical <- read.spss(paste0(infld,"derived/physical_10plus11.sav"),to.data.frame=TRUE)
physical <- physical[,c("aacode","dwtypenx","dwage9x","floorx","floor5x","storeyx","typerstr",
                        "typewstr2","typewin","dblglaz4","arnatx","attic",
                        "basement","heat7x","sysage","mainfuel","watersys","boiler",
                        "wallinsx","wallcavx","sap12")]
#############################################################################
#General Table - match when fixing names
###########################################################################
general <- read.spss(paste0(infld,"derived/general_10plus11.sav"),to.data.frame=TRUE)
general <- general[,c("aacode","aagpd1011","tenure4x","vacantx","gorEHCS","rumorph","Imd1010")]
names(general) <- c("aacode","aagpd1011","tenure4x","vacantx","GorEHCS","rumorph","imd")

##########################################################################
#elevate Table - match
####################################################################
elevate <- read.spss(paste0(infld,"physical/elevate.sav"),to.data.frame=TRUE)
elevate <- elevate[,c( "aacode","Felsolff","Felpvff","Felcavff","Felextff",
                                "Felsollf","Felpvlf","Felcavlf","Felextlf",
                                "Felsolrf","Felpvrf","Felcavrf","Felextrf",
                                "Felsolbf","Felpvbf","Felcavbf","Felextbf", 
                                "felroofp","Fvwpvbf","Fvwpvlf","Fvwpvrf","Fvwpvff")]
############################################################################
#Services Table - match
############################################################################
services <- read.spss(paste0(infld,"physical/services.sav"),to.data.frame=TRUE)
services <- services[,c("aacode","Finchtyp","Finmhfue","Finmhboi","Finchbag",
                        "Finchoff","Finchthe","Finchtim","Finchove","Finchrom","Finchcon","Finchtrv","Finchtzc","Finchdst",
                        "Finoheat","Finohphs","Finohtyp",
                        "Finwheat", "Finwhoty","Finwhoac","Finwhoag","Finwhxpr","Finwhxty","Finwhxag",
                        "Finwsipr","Finwsiag","Finwdipr","Finwdiag",
                        "Finwsppr","Finwspty","Finwspag",
                        "Finwmppr","Finwmpty","Finwmpag",
                        "Finwhlpr","Finwhlty","Finwhlag",
                        "Finwhcyl","Finwhsiz","Finwhins","Finwhmms",
                        "Finwhcen","Finwhthe","Finlopos","Flitypes","Fliinsul","Finintyp","Flithick")]
#########################################################################
#Combine
########################################################################
combined <- left_join(physical,general, by = "aacode")
combined <- left_join(combined,services, by = "aacode")
combined <- left_join(combined,elevate, by = "aacode")
remove(physical,general,services,elevate)

#######################################################################
#roof table
########################################################################
roof <- combined[,c("aacode","typerstr","Flitypes","Fliinsul","Finintyp","Flithick","attic",
                    "Felsolff","Felpvff",
                    "Felsollf","Felpvlf",
                    "Felsolrf","Felpvrf",
                    "Felsolbf","Felpvbf",
                    "felroofp","Fvwpvbf","Fvwpvlf","Fvwpvrf","Fvwpvff")]

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

roof <- roof[,c("aacode","typerstr","Flitypes","Fliinsul","Finintyp","Flithick","felroofp","attic","PV","Solar","SolarSuit")]

##########################################################################
#Walls Table
###########################################################################
walls <- combined[,c("aacode","typewstr2","wallinsx","wallcavx")]

#Plot
counts <- table(walls$wallinsx)
barplot(counts, main="External Wall Insualtion", ylab="Number of Dwellings")

############################################################################################
#Shape Table
###########################################################################################
shape <- combined[,c("aacode","dwtypenx","dwage9x","floorx","floor5x","storeyx","basement","Finlopos")]
shape$type <- as.factor(paste0(shape$Finlopos," ",shape$dwtypenx))
summary(shape$type)

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
rems <- !names(shape) %in% c("Finlopos","dwtypenx","floorx","dwage9x")
shape <- shape[,rems]

############################################################################
#Context Table
###########################################################################
context <- combined[c("aacode","tenure4x","vacantx","GorEHCS","rumorph","imd","arnatx")]



#########################################################################
#Energy Table
##########################################################################
energy <- combined[c("aacode","Finchtyp","mainfuel","Finmhboi","Finchbag","sysage","watersys",
                     "Finchoff","Finchthe","Finchtim","Finchove","Finchrom","Finchcon","Finchtrv","Finchtzc","Finchdst",
                     "Finohphs",
                     "Finwheat", "Finwhoty","Finwhoag",
                     "Finwsipr","Finwsiag","Finwdipr","Finwdiag",
                     "Finwsppr",
                     "Finwmppr",
                     "Finwhlty",
                     "Finwhcyl","Finwhsiz","Finwhins","Finwhmms"
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


#Mulit-row per dwelling tables
######################################################################
#Doors Table
######################################################################
doors <- read.spss(paste0(infld,"physical/doors.sav"),to.data.frame=TRUE)
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
windows <- read.spss(paste0(infld,"physical/windows.sav"),to.data.frame=TRUE)
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

#####################################################################
#Join back into a master table
###################################################################

remove(combined)

combined_2011 <- left_join(context,shape, by = "aacode")
combined_2011 <- left_join(combined_2011,walls, by = "aacode")
combined_2011 <- left_join(combined_2011,roof, by = "aacode")
combined_2011 <- left_join(combined_2011,windows, by = "aacode")
combined_2011 <- left_join(combined_2011,doors, by = "aacode")
combined_2011 <- left_join(combined_2011,energy, by = "aacode")

remove(around,context,doors,elevate,general,physical,roof,services,shape,test,uni,test2, walls, windows)

