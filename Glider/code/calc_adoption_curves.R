library(ggplot2)
library(reshape2)
# Make adoption curves for different tech
arch <- readRDS("data/archetype_summary_retrofitopts.Rds")
comb <- readRDS("data/combined_2013_arch.Rds")
retro.ops <- read.csv("data/retrofit_options.csv")

#Add column for each year
years <- as.character(2013:2050)
retro.ops[years] <- 0
retro.ops$k <- 0
retro.ops$x0 <- 0

#Internal and External Solid Insulation
#No data so assuming a 50:50 split
retro.ops$'2013'[retro.ops$Measure == "Internal Solid Wall Insulation"] <- sum(comb$aagpd1213[comb$wallinsy == "solid with insulation"])/2
retro.ops$'2013'[retro.ops$Measure == "External Solid Wall Insulation"] <- sum(comb$aagpd1213[comb$wallinsy == "solid with insulation"])/2

retro.ops$'2050'[retro.ops$Measure == "Internal Solid Wall Insulation"] <- sum(comb$aagpd1213[comb$wallinsy %in% c("solid with insulation", "solid uninsulated","other") ])/2
retro.ops$'2050'[retro.ops$Measure == "External Solid Wall Insulation"] <- sum(comb$aagpd1213[comb$wallinsy %in% c("solid with insulation", "solid uninsulated","other") ])/2

#Cavity wall insulation
retro.ops$'2013'[retro.ops$Measure == "Cavity Wall Insulation"] <- sum(comb$aagpd1213[comb$wallinsy == "cavity with insulation"])
retro.ops$'2050'[retro.ops$Measure == "Cavity Wall Insulation"] <- sum(comb$aagpd1213[comb$wallinsy %in% c("cavity with insulation", "cavity uninsulated") ])


#Party Caity Wall Insualtion



#Draft Proofing



#Loft Insulation
retro.ops$'2013'[retro.ops$Measure == "Loft Insulation"] <- sum(comb$aagpd1213[comb$LoftIns == "Well Insulated" & comb$rooftyp == "A" & comb$generaltyp %in% c("A","B","C","F")])
retro.ops$'2050'[retro.ops$Measure == "Loft Insulation"] <- sum(comb$aagpd1213[comb$LoftIns %in% c("Poorly Insulated","Well Insulated","No Insulation") & comb$rooftyp == "A" & comb$generaltyp %in% c("A","B","C","F")])

#Room In Roof Insualtion
retro.ops$'2013'[retro.ops$Measure == "Room in roof insulation"] <- sum(comb$aagpd1213[comb$LoftIns == "Well Insulated" & comb$rooftyp == "B" & comb$generaltyp %in% c("A","B","C","F")])
retro.ops$'2050'[retro.ops$Measure == "Room in roof insulation"] <- sum(comb$aagpd1213[comb$LoftIns %in% c("Poorly Insulated","Well Insulated","No Insulation") & comb$rooftyp == "B" & comb$generaltyp %in% c("A","B","C","F")])

#Flat Roof Insualtion
retro.ops$'2013'[retro.ops$Measure == "Flat roof insulation"] <- sum(comb$aagpd1213[comb$LoftIns == "Well Insulated" & comb$rooftyp == "C" & comb$generaltyp %in% c("A","B","C","F")])
retro.ops$'2050'[retro.ops$Measure == "Flat roof insulation"] <- sum(comb$aagpd1213[comb$LoftIns %in% c("Poorly Insulated","Well Insulated","No Insulation") & comb$rooftyp == "C" & comb$generaltyp %in% c("A","B","C","F")])

#Floor Insualtion Solid
retro.ops$'2050'[retro.ops$Measure == "Floor Insulation Solid"] <- sum(comb$aagpd1213[comb$floortyp == "A" & comb$generaltyp %in% c("A","B","C","F")])
retro.ops$'2013'[retro.ops$Measure == "Floor Insulation Solid"] <- sum(comb$aagpd1213[comb$floortyp == "A" & comb$generaltyp %in% c("A","B","C","F")])/100 #No data assume 1% uptake

#Floor Insualtion Suspended
retro.ops$'2050'[retro.ops$Measure == "Floor Insulation Suspended"] <- sum(comb$aagpd1213[comb$floortyp == "B" & comb$generaltyp %in% c("A","B","C","F")])
retro.ops$'2013'[retro.ops$Measure == "Floor Insulation Suspended"] <- sum(comb$aagpd1213[comb$floortyp == "B" & comb$generaltyp %in% c("A","B","C","F")])/100 #not data so assume 1% uptake

#new or replacement windows
#base on fixed lifetime
retro.ops$'2013'[retro.ops$Measure == "new or replacement windows"] <- sum(comb$aagpd1213[comb$floortyp == "A" & comb$winage == "0 -10 years"])
retro.ops$'2050'[retro.ops$Measure == "new or replacement windows"] <- sum(comb$aagpd1213)

#New or replacement external doors


#solar photovoltaics
retro.ops$'2013'[retro.ops$Measure == "solar photovoltaics"] <- sum(comb$aagpd1213[comb$solartyp == "A" & comb$PV == "Yes" & comb$generaltyp %in% c("A","B","C","F")])
retro.ops$'2050'[retro.ops$Measure == "solar photovoltaics"] <- sum(comb$aagpd1213[comb$solartyp == "A" & comb$PV %in% c("Yes","No") & comb$generaltyp %in% c("A","B","C","F")])

#solar water heating
retro.ops$'2013'[retro.ops$Measure == "solar water heating"] <- sum(comb$aagpd1213[comb$solartyp == "A" & 
                                                                                     comb$Solar == "Yes" & 
                                                                                     comb$energytyp %in% c("B","C","E") & 
                                                                                     comb$generaltyp %in% c("A","B","C","F")])
retro.ops$'2050'[retro.ops$Measure == "solar water heating"] <- sum(comb$aagpd1213[comb$solartyp == "A" & 
                                                                                     comb$Solar %in% c("Yes","No") & 
                                                                                     comb$energytyp %in% c("B","C","E") & 
                                                                                     comb$generaltyp %in% c("A","B","C","F")])


#heating controls
retro.ops$'2013'[retro.ops$Measure == "heating controls"] <- sum(comb$aagpd1213[comb$control == 7])
retro.ops$'2050'[retro.ops$Measure == "heating controls"] <- sum(comb$aagpd1213) # All dwellings need some from of heating controls




#Future heating mix
#based on Green Building Council WRAP 2050
#Secnario 3: to deliver 80% carbon reductions
#gas boiler 46%
#resistive heating 6%
#ASHP 30%
#GSHP 10%
#Geotherm 1%
#District 7%

#air source heat pump
retro.ops$'2013'[retro.ops$Measure == "air source heat pump"] <- 1000 # no data so assume 1000
retro.ops$'2050'[retro.ops$Measure == "air source heat pump"] <- sum(comb$aagpd1213) * 0.3


#ground source heat pump
retro.ops$'2013'[retro.ops$Measure == "ground source heat pump"] <- 1000 # no data so assume 1000
retro.ops$'2050'[retro.ops$Measure == "ground source heat pump"] <- sum(comb$aagpd1213) * 0.1

#biomass boiler


#district heating connection
retro.ops$'2013'[retro.ops$Measure == "district heating connection"] <- sum(comb$aagpd1213[comb$energytyp == "G"]) # communal heating not quite the same a distric heating
retro.ops$'2050'[retro.ops$Measure == "district heating connection"] <- sum(comb$aagpd1213) * 0.07

#Boiler replacements
retro.ops$'2013'[retro.ops$Measure == "Boiler replacements"] <- sum(comb$aagpd1213[comb$energytyp %in% c("A","B","C","H")]) 
retro.ops$'2050'[retro.ops$Measure == "Boiler replacements"] <- sum(comb$aagpd1213) * 0.46

#Electric storage heater replacement
retro.ops$'2013'[retro.ops$Measure == "Electric storage heater replacement"] <- sum(comb$aagpd1213[comb$energytyp == "D"]) #storage heater not quite the same as resistive
retro.ops$'2050'[retro.ops$Measure == "Electric storage heater replacement"] <- sum(comb$aagpd1213) * 0.07




#energy efficient lighting


#energy efficient appliances


#radiator panels


#New boiler / heating


#Hot water cylinder insulation
retro.ops$'2013'[retro.ops$Measure == "Hot water cylinder insulation"] <- sum(comb$aagpd1213[comb$tankins == "Well Insulated" ])
retro.ops$'2050'[retro.ops$Measure == "Hot water cylinder insulation"] <- sum(comb$aagpd1213[comb$tankins %in% c("Poorly Insulated","No Insulation","Well Insulated") ])


#Primary pipework insulation


#Underfloor heating


#mechanical ventilation with heat recovery





#Calualate Uptake

#Calcualte existing uptake of measures
cal.uptake <- function(L,k,x,x0){
  res <- L / (1 + (exp(1) ** (-k * (x - x0) )))
  return(res)
}

#Inverse for decling uptake
cal.downtake <- function(L,k,x,x0){
  res <- L / (1 + (exp(1) ** (k * (x - x0) )))
  return(res)
}

get.uptake <- function(measure){
  #http://www.clear-lines.com/blog/post/S-shaped-market-adoption-curve.aspx
  rownumb <- which(retro.ops$Measure == measure)
  L <- retro.ops[rownumb,"2050"]
  y2013 <- retro.ops[rownumb,"2013"]
  k <- (log(L/y2013 - 1) - log(L/(0.99*L) - 1))/(2050 - 2013)
  x0 <- log(L/y2013 - 1)/k + 2013
  message(measure," ",round(x0,2)," ",round(k,2))
  
  for(i in 2014:2049){
    sub <- round(cal.uptake(L,k,x = i,x0),0)
    if(i == 2014){
      res <- sub
    }else{
      res <- c(res,sub)
    }
  }
  #Bind 2050, k , x0 on to the end
  res <- c(res,L,k,x0)
  return(res)
}

linear.uptake <- function(measure){
  rownumb <- which(retro.ops$Measure == measure)
  L <- retro.ops[rownumb,"2050"]
  y2013 <- retro.ops[rownumb,"2013"]
  diff <- (L - y2013)/(2050 - 2013)
  res <- list()
  for(i in 2014:2049){
    if(i == 2014){
      res[[i]] <- round(y2013 + diff,0)
    }else{
      sub <- round(res[[i-1]] + diff,0)
      if(sub > L){
        res[[i]] <- L
      }else{
        res[[i]] <- sub
      }
      
    }
  }
  res <- unlist(res)
  res <- c(res,L,NA,NA)
  return(res)
}

neg.scurve <- function(measure){
  #http://www.clear-lines.com/blog/post/S-shaped-market-adoption-curve.aspx
  rownumb <- which(retro.ops$Measure == measure)
  L <- retro.ops[rownumb,"2013"]
  y2013 <- retro.ops[rownumb,"2050"]
  k <- (log(L/y2013 - 1) - log(L/(0.99*L) - 1))/(2050 - 2013)
  x0 <- log(L/y2013 - 1)/k + 2050
  message(measure," ",round(x0,2)," ",round(k,2))
  
  for(i in 2014:2049){
    sub <- round(cal.downtake(L,k,x = i,x0),0)
    if(i == 2014){
      res <- sub
    }else{
      res <- c(res,sub)
    }
  }
  #Bind 2050, k , x0 on to the end
  res <- c(res,y2013,k,x0)
  return(res)
}

for(i in 1:nrow(retro.ops)){
  measure <- retro.ops$Measure[i]
  method <- retro.ops$Model[i]
  if(method == "scurve"){
    retro.ops[retro.ops$Measure == measure, c(years[2:38],"k","x0") ] <- get.uptake(measure)
  }else if(method == "linear"){
    retro.ops[retro.ops$Measure == measure, c(years[2:38],"k","x0") ] <- linear.uptake(measure)
  }else if(method == "negcurve"){
    retro.ops[retro.ops$Measure == measure, c(years[2:38],"k","x0") ] <- neg.scurve(measure)
  }
  
}


foo <- retro.ops[retro.ops$'2014' != 0,c("Measure",years)]

df <- melt(foo)#the function melt reshapes it from wide to long
df$Measure <- NULL
df$rowid <- foo$Measure  #add a rowid identifying variable
head(df)
ggplot(df, aes(variable, value, group=factor(rowid))) + geom_line(aes(color=factor(rowid)), size = 2)
