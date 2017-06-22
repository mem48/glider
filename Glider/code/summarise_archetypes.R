library(Hmisc)

# Summaries Architypes othe characteristcs

comb <- readRDS("data/combined_2013_arch.Rds")
comb$archcode <- as.character(comb$archcode)

arch <- data.frame(archcode = unique(comb$archcode), 
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
                   floorarea.Q3 = 0)

arch$archcode <- as.character(arch$archcode)

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


write.csv(arch,"data/archetype_summary.csv")


