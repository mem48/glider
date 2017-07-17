library(Hmisc)
#library(Hmisc, lib.loc = "M:/R/R-3.3.1/library")
# Summaries Architypes othe characteristcs

comb <- readRDS("data/combined_2013_arch.Rds")
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
                                                                                         "Wood fueled central heating",
                                                                                         "Storage Heaters",
                                                                                         "Other heating e.g. Warm Air/ Electric Underfloor",
                                                                                         "Communal Heating System",
                                                                                         "Room Heaters"), stringsAsFactors = F)
arsum_roof <- data.frame(id = c("A","B","C"), desc = c("Pitched no attic","Pitched with attic","Flat"), stringsAsFactors = F)
arsum_solar <- data.frame(id = c("A","B"), desc = c("Yes","No"), stringsAsFactors = F)
arsum_floor <- data.frame(id = c("A","B","C"), desc = c("Concrete Floors","Suspended Timber Floor","Historic"), stringsAsFactors = F)



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
                   age.Q3 = 0,
                   sap.Q1 = 0,
                   sap.Q3 = 0,
                   windows.Q1 = 0,
                   windows.Q3 = 0,
                   gasgrid.yes = 0,
                   tank.wellins = 0,
                   doors.Q1 = 0,
                   doors.Q3 = 0,
                   control.Q1 = 0,
                   control.Q3 = 0,
                   radcontrol.Q1 = 0,
                   radcontrol.Q3 = 0,
                   solararea.Q1 = 0,
                   solararea.Q3 = 0,
                   floorarea.Q1 = 0,
                   floorarea.Q3 = 0, 
                   wallins = 0,
                   
                   stringsAsFactors = F)

arch$archcode <- as.character(arch$archcode)




#Get artchetype text
for(z in 1:1000){
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
  arch$floorarea.Q3[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.75))
}


#Get control
for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = unique(comb$control[comb$archcode == code]), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$control == foo$val[c]])
  }
  arch$control.Q1[a] <- round(wtd.quantile(x = foo$val, weights = foo$ndwel, probs = 0.25))
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

for(a in 1:nrow(arch)){
  code <- arch$archcode[a]
  foo <- data.frame(val = c("cavity uninsulated","cavity with insulation","other","solid uninsulated","solid with insulation"), ndwel = 0)
  for(c in 1:nrow(foo)){
    foo$ndwel[c] <- sum(comb$aagpd1213[comb$archcode == code & comb$wallinsy == foo$val[c]])
  }
  arch$wallins[a] <- round(sum(foo$ndwel[foo$val %in% c("cavity with insulation","solid with insulation")]) / sum(foo$ndwel) * 100, 2)
}



write.csv(arch,"data/archetype_summary.csv", row.names = FALSE)


