library(Hmisc)
#library(Hmisc, lib.loc = "M:/R/R-3.3.1/library")
# Summaries Architypes othe characteristcs

comb <- readRDS("../jobs_data/combined_2013_arch.Rds")
comb$archcode <- as.character(comb$archcode)

arsum_type <- data.frame(id = c("A","B","C","F","G","H","I"), desc = c("Semi Detached/End Terrace",
                                                                       "Detached","Mid terrace",
                                                                       "Bungalow",
                                                                       "converted flat",
                                                                       "Low rise flat",
                                                                       "High rise flat"), stringsAsFactors = F)
arsum_wall <- data.frame(id = c("A","B","C","D"), desc = c("Masonry Cavity","Soild Masonry","Concrete / Other","Mixed"), stringsAsFactors = F)
arsum_window <- data.frame(id = c("A","B","C","D"), desc = c("Entire house","More than half","Less than half","No double glazing"), stringsAsFactors = F)
arsum_energy <- data.frame(id = c("L","A","B","C","H","E","W","D","K","G","F"), desc = c("Other central heating e.g. coal / electric",
                                                                                         "Gas central heating & Water, without Tank",
                                                                                         "Gas central heating & Water, with Immersion Heater",
                                                                                         "Gas central heating & Water, with hot water tank",
                                                                                         "Gas central heating with separate hot water system",
                                                                                         "Oil central heating",
                                                                                         "Wood fuelled central heating",
                                                                                         "Storage Heaters",
                                                                                         "Other heating e.g. Warm Air/ Electric Underfloor",
                                                                                         "Communal Heating System",
                                                                                         "Room Heaters"), stringsAsFactors = F)
arsum_roof <- data.frame(id = c("A","B","C"), desc = c("Pitched no attic","Pitched with attic","Flat"), stringsAsFactors = F)
arsum_solar <- data.frame(id = c("A","B"), desc = c("Yes","No"), stringsAsFactors = F)
arsum_floor <- data.frame(id = c("A","B","C"), desc = c("Concrete Floors","Suspended Timber Floor","Historic"), stringsAsFactors = F)


#estimate the ground floor area
comb$groundfloorarea <- comb$floorx / comb$storeyx
comb$est.extwallarea <- comb$dpc.perim * comb$cheight0 * comb$storeyx

plot(comb$est.extwallarea, comb$wall.area.ext)

summary(comb$groundfloorarea)
abline(a = 1, b = 1, col = "red")

arch <- data.frame(archcode = unique(comb$archcode), 
                   arch_type = "",
                   arch_wall = "",
                   arch_floor = "",
                   arch_roof = "",
                   arch_solar = "",
                   arch_window = "", 
                   ndwel = 0,
                   nsample = 0,
                   age.Q1 = 0,
                   age.Q2 = 0,
                   age.Q3 = 0,
                   sap.Q1 = 0,
                   sap.Q2 = 0,
                   sap.Q3 = 0,
                   windows.Q1 = 0,
                   windows.Q2 = 0,
                   windows.Q3 = 0,
                   gasgrid.yes = 0,
                   tank.wellins = 0,
                   doors.Q1 = 0,
                   doors.Q2 = 0,
                   doors.Q3 = 0,
                   control.Q1 = 0,
                   control.Q2 = 0,
                   control.Q3 = 0,
                   radcontrol.Q1 = 0,
                   radcontrol.Q2 = 0,
                   radcontrol.Q3 = 0,
                   solararea.Q1 = 0,
                   solararea.Q2 = 0,
                   solararea.Q3 = 0,
                   floorarea.Q1 = 0,
                   floorarea.Q2 = 0,
                   floorarea.Q3 = 0,
                   groundfloorarea.Q1 = 0,
                   groundfloorarea.Q2 = 0,
                   groundfloorarea.Q3 = 0,
                   intwallarea.Q1 = 0,
                   intwallarea.Q2 = 0,
                   intwallarea.Q3 = 0,
                   extwallarea.Q1 = 0,
                   extwallarea.Q2 = 0,
                   extwallarea.Q3 = 0,
                   extwindowarea.Q1 = 0,
                   extwindowarea.Q2 = 0,
                   extwindowarea.Q3 = 0,
                   dpcperim.Q1 = 0,
                   dpcperim.Q2 = 0,
                   dpcperim.Q3 = 0,
                   nfloors.Q1 = 0,
                   nfloors.Q2 = 0,
                   nfloors.Q3 = 0,
                   wallins = 0,
                   loftins = 0,
                   roofareaplan.Q1 = 0,
                   roofareaplan.Q2 = 0,
                   roofareaplan.Q3 = 0,
                   roofareaslope.Q1 = 0,
                   roofareaslope.Q2 = 0,
                   roofareaslope.Q3 = 0,
                   ceilingground.Q1 = 0,
                   ceilingground.Q2 = 0,
                   ceilingground.Q3 = 0,
                   ceilingfirst.Q1 = 0,
                   ceilingfirst.Q2 = 0,
                   ceilingfirst.Q3 = 0,
                   imd.Q1 = 0,
                   imd.Q2 = 0,
                   imd.Q3 = 0,
                   stringsAsFactors = F)

arch$archcode <- as.character(arch$archcode)




#Get artchetype text
for(z in 1:nrow(arch)){
  code <- arch$archcode[z]
  #print(arsum_type$desc[arsum_type$id == substr(code,1,1)])
  arch$arch_type[z] <- arsum_type$desc[arsum_type$id == substr(code,1,1)]
  arch$arch_wall[z] <- arsum_wall$desc[arsum_wall$id == substr(code,3,3)]
  arch$arch_floor[z] <- arsum_floor$desc[arsum_floor$id == substr(code,5,5)]
  arch$arch_roof[z] <- arsum_roof$desc[arsum_roof$id == substr(code,7,7)]
  arch$arch_solar[z] <- arsum_solar$desc[arsum_solar$id == substr(code,9,9)]
  arch$arch_window[z] <- arsum_window$desc[arsum_window$id == substr(code,11,11)]
  arch$arch_energy[z] <- arsum_energy$desc[arsum_energy$id == substr(code,13,13)]
}


#Count Dwellings
for (a in 1:nrow(arch)){
  code <- arch$archcode[a]
  arch$ndwel[a] <- sum(comb$aagpd1213[comb$archcode == code])
}

#Count Sample
for (a in 1:nrow(arch)){
  code <- arch$archcode[a]
  arch$nsample[a] <- length(comb$aagpd1213[comb$archcode == code])
}


#Get ages
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  age <- data.frame(age = unique(comb$age[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(age)){
    age$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$age == age$age[c]])
  }
  arch$age.Q1[a] <- wtd.quantile(x = age$age, weights = age$ndwel, probs = 0.25)
  arch$age.Q2[a] <- wtd.quantile(x = age$age, weights = age$ndwel, probs = 0.5)
  arch$age.Q3[a] <- wtd.quantile(x = age$age, weights = age$ndwel, probs = 0.75)
}


#Get SAP
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$sap[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$sap == foo$val[c]])
  }
  arch$sap.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$sap.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$sap.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}

#Get Windows
comb$wintot <- comb$dblglaze + comb$sngglaze

for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$wintot[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$wintot == foo$val[c]])
  }
  arch$windows.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$windows.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$windows.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}

#Get doors
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$doornumb[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$doornumb == foo$val[c]])
  }
  arch$doors.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$doors.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$doors.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}

#Get floor area
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$floorx[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$floorx == foo$val[c]])
  }
  arch$floorarea.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$floorarea.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$floorarea.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}


#Get ground floor area
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$groundfloorarea[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$floorx == foo$val[c]])
  }
  arch$groundfloorarea.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$groundfloorarea.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$groundfloorarea.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}



#Get control
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$control[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$control == foo$val[c]])
  }
  arch$control.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$control.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$control.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}


#Get rad control
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$radcontrol[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$radcontrol == foo$val[c]])
  }
  arch$radcontrol.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$radcontrol.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$radcontrol.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}


#Get floor area
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$floorx[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$floorx == foo$val[c]])
  }
  arch$floorarea.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$floorarea.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$floorarea.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}


#Get solar area
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$solarea[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$solarea == foo$val[c]])
  }
  arch$solararea.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$solararea.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$solararea.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}

#Gas Grid
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = c("yes","no"), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$gasgrid == foo$val[c]])
  }
  arch$gasgrid.yes[a] <- round(foo$ndwel[foo$val == "yes"]/sum(foo$ndwel)*100)
}

#Tank Insulation
comb$tankins <- as.factor(comb$tankins)

for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = c("No Insulation","No Tank","Poorly Insulated","Well Insulated"), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$tankins == foo$val[c]])
  }
  arch$tank.wellins[a] <- round(foo$ndwel[foo$val == "Well Insulated"]/sum(foo$ndwel[foo$val == "Well Insulated"],foo$ndwel[foo$val == "Poorly Insulated"],foo$ndwel[foo$val == "No Insulation"],0.000000000001)*100)
}

#Location

arch$citycentre <- 0
arch$otherurban <- 0
arch$suburban <- 0
arch$ruralres <- 0
arch$villagecentre <- 0
arch$rural <- 0

for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = c("city centre","other urban centre","suburban residential","rural residential","village centre","rural"), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$arnatx == foo$val[c]])
  }
  arch$citycentre[a] <- foo$ndwel[foo$val == "city centre"]
  arch$otherurban[a] <- foo$ndwel[foo$val == "other urban centre"]
  arch$suburban[a] <- foo$ndwel[foo$val == "suburban residential"]
  arch$ruralres[a] <- foo$ndwel[foo$val == "rural residential"]
  arch$villagecentre[a] <- foo$ndwel[foo$val == "village centre"]
  arch$rural[a] <- foo$ndwel[foo$val == "rural"]
}


#Get wall insualtion rates
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = c("cavity uninsulated","cavity with insulation","other","solid uninsulated","solid with insulation"), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$wallinsy == foo$val[c]])
  }
  arch$wallins[a] <- round(sum(foo$ndwel[foo$val %in% c("cavity with insulation","solid with insulation")]) / sum(foo$ndwel) * 100, 2)
}

#Get loft insualtion rates
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = c("Poorly Insulated","Well Insulated","No Insulation"), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$LoftIns == foo$val[c]])
  }
  arch$loftins[a] <- round(sum(foo$ndwel[foo$val == c("Well Insulated")]) / sum(foo$ndwel) * 100, 2)
}


#Get Internal Wall area
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$IntWalAr[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$IntWalAr == foo$val[c]])
  }
  arch$intwallarea.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$intwallarea.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$intwallarea.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}

#Get ExternalWall area
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$wall.area.ext[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$wall.area.ext == foo$val[c]])
  }
  arch$extwallarea.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$extwallarea.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$extwallarea.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}


#Get Internal Wall area of external walls
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$wall.area.ext[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$wall.area.ext == foo$val[c]])
  }
  arch$extwallarea.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$extwallarea.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$extwallarea.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}

#Get exernal Window Area
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$window.area.ext[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$window.area.ext == foo$val[c]])
  }
  arch$extwindowarea.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$extwindowarea.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$extwindowarea.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}


#Get exernal DPC perimiter
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$dpc.perim[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$dpc.perim == foo$val[c]])
  }
  arch$dpcperim.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$dpcperim.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$dpcperim.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}

#Get number of floors
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$NFlorm[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$NFlorm == foo$val[c]])
  }
  arch$nfloors.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$nfloors.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$nfloors.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}


#Get roof plan area
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$roof.area.plan[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$roof.area.plan == foo$val[c]])
  }
  arch$roofareaplan.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$roofareaplan.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$roofareaplan.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}

#Get roof slope area
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$roof.area.slope[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$roof.area.slope == foo$val[c]])
  }
  arch$roofareaslope.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$roofareaslope.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$roofareaslope.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}

#Get ground floor ceiling height
#special version to deal with NAS
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$cheight0[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$cheight0 == foo$val[c]], na.rm = T)
  }
  #Special if to deal with all NAs
  if(nrow(foo) == 1 & is.na(foo[1,1])){
    arch$ceilingground.Q1[a] <- NA
    arch$ceilingground.Q1[a] <- NA
  }else{
    arch$ceilingground.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25, na.rm = T), 2)
    arch$ceilingground.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5, na.rm = T), 2)
    arch$ceilingground.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75, na.rm = T), 2)
  }
}

#Get first floor ceiling height
#special version to deal with NAS
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$cheight1[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$cheight1 == foo$val[c]], na.rm = T)
  }
  #Special if to deal with all NAs
  if(nrow(foo) == 1 & is.na(foo[1,1])){
    arch$ceilingfirst.Q1[a] <- NA
    arch$ceilingfirst.Q3[a] <- NA
  }else{
    arch$ceilingfirst.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25, na.rm = T),2)
    arch$ceilingfirst.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5, na.rm = T),2)
    arch$ceilingfirst.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75, na.rm = T),2)
  }
}

#Get IMD ratings
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$imd[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$imd == foo$val[c]])
  }
  arch$imd.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
  arch$imd.Q2[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.5))
  arch$imd.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}

#Get Tenure
comb$tenure4x <- as.character(comb$tenure4x)

arch$own <- 0
arch$rent.private <- 0
arch$rent.la <- 0
arch$rent.rsl <- 0

for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = c("private rented","local authority","owner occupied","RSL"), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$tenure4x == foo$val[c]])
  }
  arch$own[a] <- foo$ndwel[foo$val == "owner occupied"]
  arch$rent.private[a] <- foo$ndwel[foo$val == "private rented"]
  arch$rent.la[a] <- foo$ndwel[foo$val == "local authority"]
  arch$rent.rsl[a] <- foo$ndwel[foo$val == "RSL"]
}




#Sort Retults
arch <- arch[order(-arch$ndwel),]

#Save Results
write.csv(arch,"data/archetype_summary.csv", row.names = FALSE)

