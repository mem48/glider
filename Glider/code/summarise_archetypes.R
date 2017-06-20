# Summaries Architypes othe characteristcs

comb <- readRDS("data/combined_2013_arch.Rds")

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
                   tankins.yes = 0,
                   doors.Q1 = 0,
                   doors.Q3 = 0,
                   control.Q1 = 0,
                   control.Q3 = 0,
                   radcontrol.Q1 = 0,
                   radcontrol.Q3 = 0,
                   solararea.Q1 = 0,
                   solararea.Q3 = 0)


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

plot(comb$sap12)

