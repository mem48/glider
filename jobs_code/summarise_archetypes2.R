library(Hmisc)
library(dplyr)
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


#fill in missing cieling heights
comb$cheight0[is.na(comb$cheight0)] = median(comb$cheight0, na.rm = T )
comb$cheight1[is.na(comb$cheight1)] = median(comb$cheight0, na.rm = T )

# etimate number of floors
# assume flats are single storey
comb$nfloors = ifelse(comb$dwtypenx %in% c("converted flat","purpose built flat, low rise","purpose built flat, high rise"),
                      1,  comb$storeyx)

#estimate the ground floor area
# account for flats
comb$groundfloorarea <- comb$floorx / comb$nfloors
# estmaite the external wall area
# account for flats and 
# comb$extwallarea <- ifelse(comb$dwtypenx %in% c("converted flat","purpose built flat, low rise","purpose built flat, high rise"),
#                            comb$wall.area.ext / comb$storeyx,
#                            comb$dpc.perim * comb$cheight0 * comb$storeyx)

comb$extwallarea <- ifelse(comb$dwtypenx %in% c("converted flat","purpose built flat, low rise","purpose built flat, high rise"),
                               sqrt(comb$floorx) * 2 * comb$cheight0,
                               comb$dpc.perim * comb$cheight0 * comb$storeyx)

plot(comb$extwallarea, comb$wall.area.ext)

summary(comb$groundfloorarea)
abline(a = 1, b = 1, col = "red")

# estinmate the number of external walls
external.walls <- function(x){
  if(x %in% c("detached","bungalow")){
    return(4)
  }else if(x %in% c("semi detached","end terrace")){
    return(3)
  }else{
    return(2)
  }
}

comb$externalwalls = sapply(comb$type,external.walls)

# estimate external wall areas
comb$extwallarea.front = comb$extwallarea / comb$externalwalls
comb$extwallarea.exceptfront = comb$extwallarea - comb$extwallarea.front

# estimate window area
comb$window.area.ext.est = ifelse(comb$dwtypenx %in% c("converted flat","purpose built flat, low rise","purpose built flat, high rise"),
                                  comb$window.area.ext.ext / comb$storeyx,
                                  comb$window.area.ext)


# estimate internal wall areas
# assumking a 300mm wall thickness 
#comb$intwallarea.extwalls = comb$wall.area.ext - ((comb$cheight0 * comb$storeyx) * 2 * comb$externalwalls * 0.3 )
comb$intwallarea.extwalls = ifelse(comb$dwtypenx %in% c("converted flat","purpose built flat, low rise","purpose built flat, high rise"),
                                   comb$extwallarea - ((comb$cheight0 * 1) * 2 * comb$externalwalls * 0.3 ),
                                   comb$extwallarea - ((comb$cheight0 * comb$storeyx) * 2 * comb$externalwalls * 0.3 ))



plot(comb$wall.area.ext, comb$intwallarea.extwalls)
abline(a = 1, b = 1, col = "red")
comb$intwallarea.frontwall = comb$intwallarea.extwalls / comb$externalwalls
#comb$intwallarea.extpartywalls = comb$wall.area.ext - ((comb$cheight0 * comb$storeyx) * 2 * 4 * 0.3 )
comb$intwallarea.extpartywalls = ifelse(comb$dwtypenx %in% c("converted flat","purpose built flat, low rise","purpose built flat, high rise"),
                                   comb$extwallarea - ((comb$cheight0 * 1) * 2 * 4 * 0.3 ),
                                   comb$extwallarea - ((comb$cheight0 * comb$storeyx) * 2 * 4 * 0.3 ))

# get total number of windows
comb$wintot <- comb$dblglaze + comb$sngglaze

arch = comb %>%
        group_by(archcode) %>%
          summarise(
                    # Headline Numbers
                    ndwel =              sum(aagpd1213,na.rm = T),
                    nsample =            length(aagpd1213),
                    # Quartile Breakdowns
                    age.Q1 =             wtd.quantile(age,             aagpd1213, probs = 0.25),
                    age.Q2 =             wtd.quantile(age,             aagpd1213, probs = 0.50),
                    age.Q3 =             wtd.quantile(age,             aagpd1213, probs = 0.75),
                    sap.Q1 =             wtd.quantile(sap12,             aagpd1213, probs = 0.25),
                    sap.Q2 =             wtd.quantile(sap12,             aagpd1213, probs = 0.50),
                    sap.Q3 =             wtd.quantile(sap12,             aagpd1213, probs = 0.75),
                    windows.Q1 =         wtd.quantile(wintot,          aagpd1213, probs = 0.25),
                    windows.Q2 =         wtd.quantile(wintot,          aagpd1213, probs = 0.50),
                    windows.Q3 =         wtd.quantile(wintot,          aagpd1213, probs = 0.75),
                    doors.Q1 =           wtd.quantile(doornumb,        aagpd1213, probs = 0.25),
                    doors.Q2 =           wtd.quantile(doornumb,        aagpd1213, probs = 0.50),
                    doors.Q3 =           wtd.quantile(doornumb,        aagpd1213, probs = 0.75),
                    floorarea.Q1 =       wtd.quantile(floorx,          aagpd1213, probs = 0.25),
                    floorarea.Q2 =       wtd.quantile(floorx,          aagpd1213, probs = 0.50),
                    floorarea.Q3 =       wtd.quantile(floorx,          aagpd1213, probs = 0.75),
                    groundfloorarea.Q1 = wtd.quantile(groundfloorarea, aagpd1213, probs = 0.25),
                    groundfloorarea.Q2 = wtd.quantile(groundfloorarea, aagpd1213, probs = 0.50),
                    groundfloorarea.Q3 = wtd.quantile(groundfloorarea, aagpd1213, probs = 0.75),
                    control.Q1 =         wtd.quantile(control,         aagpd1213, probs = 0.25),
                    control.Q2 =         wtd.quantile(control,         aagpd1213, probs = 0.50),
                    control.Q3 =         wtd.quantile(control,         aagpd1213, probs = 0.75),
                    radcontrol.Q1 =      wtd.quantile(radcontrol,      aagpd1213, probs = 0.25),
                    radcontrol.Q2 =      wtd.quantile(radcontrol,      aagpd1213, probs = 0.50),
                    radcontrol.Q3 =      wtd.quantile(radcontrol,      aagpd1213, probs = 0.75),
                    solararea.Q1 =       wtd.quantile(solarea,       aagpd1213, probs = 0.25),
                    solararea.Q2 =       wtd.quantile(solarea,       aagpd1213, probs = 0.50),
                    solararea.Q3 =       wtd.quantile(solarea,       aagpd1213, probs = 0.75),
                    # Wall Areas
                    intwallarea.Q1 =     wtd.quantile(IntWalAr,        aagpd1213, probs = 0.25),
                    intwallarea.Q2 =     wtd.quantile(IntWalAr,        aagpd1213, probs = 0.50),
                    intwallarea.Q3 =     wtd.quantile(IntWalAr,        aagpd1213, probs = 0.75),
                    intwallarea.extwalls.Q1 = wtd.quantile(intwallarea.extwalls,   aagpd1213, probs = 0.25),
                    intwallarea.extwalls.Q2 = wtd.quantile(intwallarea.extwalls,   aagpd1213, probs = 0.50),
                    intwallarea.extwalls.Q3 = wtd.quantile(intwallarea.extwalls,   aagpd1213, probs = 0.75),
                    intwallarea.extpartywalls.Q1 = wtd.quantile(intwallarea.extpartywalls,   aagpd1213, probs = 0.25),
                    intwallarea.extpartywalls.Q2 = wtd.quantile(intwallarea.extpartywalls,   aagpd1213, probs = 0.50),
                    intwallarea.extpartywalls.Q3 = wtd.quantile(intwallarea.extpartywalls,   aagpd1213, probs = 0.75),
                    intwallarea.frontwall.Q1 = wtd.quantile(intwallarea.frontwall,   aagpd1213, probs = 0.25),
                    intwallarea.frontwall.Q2 = wtd.quantile(intwallarea.frontwall,   aagpd1213, probs = 0.50),
                    intwallarea.frontwall.Q3 = wtd.quantile(intwallarea.frontwall,   aagpd1213, probs = 0.75),
                    extwallarea.Q1 =     wtd.quantile(extwallarea,   aagpd1213, probs = 0.25),
                    extwallarea.Q2 =     wtd.quantile(extwallarea,   aagpd1213, probs = 0.50),
                    extwallarea.Q3 =     wtd.quantile(extwallarea,   aagpd1213, probs = 0.75),
                    extwallarea.front.Q1 =       wtd.quantile(extwallarea.front,         aagpd1213, probs = 0.25),
                    extwallarea.front.Q2 =       wtd.quantile(extwallarea.front,         aagpd1213, probs = 0.50),
                    extwallarea.front.Q3 =       wtd.quantile(extwallarea.front,         aagpd1213, probs = 0.75),
                    extwallarea.exceptfront.Q1 = wtd.quantile(extwallarea.exceptfront,   aagpd1213, probs = 0.25),
                    extwallarea.exceptfront.Q2 = wtd.quantile(extwallarea.exceptfront,   aagpd1213, probs = 0.50),
                    extwallarea.exceptfront.Q3 = wtd.quantile(extwallarea.exceptfront,   aagpd1213, probs = 0.75),
                    #Other Dimensions
                    extwindowarea.Q1 =   wtd.quantile(window.area.ext.est, aagpd1213, probs = 0.25),
                    extwindowarea.Q2 =   wtd.quantile(window.area.ext.est, aagpd1213, probs = 0.50),
                    extwindowarea.Q3 =   wtd.quantile(window.area.ext.est, aagpd1213, probs = 0.75),
                    dpcperim.Q1 =        wtd.quantile(dpc.perim,       aagpd1213, probs = 0.25),
                    dpcperim.Q2 =        wtd.quantile(dpc.perim,       aagpd1213, probs = 0.50),
                    dpcperim.Q3 =        wtd.quantile(dpc.perim,       aagpd1213, probs = 0.75),
                    nfloors.Q1 =         wtd.quantile(nfloors,          aagpd1213, probs = 0.25),
                    nfloors.Q2 =         wtd.quantile(nfloors,          aagpd1213, probs = 0.50),
                    nfloors.Q3 =         wtd.quantile(nfloors,          aagpd1213, probs = 0.75),
                    roofareaplan.Q1 =    wtd.quantile(roof.area.plan,  aagpd1213, probs = 0.25),
                    roofareaplan.Q2 =    wtd.quantile(roof.area.plan,  aagpd1213, probs = 0.50),
                    roofareaplan.Q3 =    wtd.quantile(roof.area.plan,  aagpd1213, probs = 0.75),
                    roofareaslope.Q1 =   wtd.quantile(roof.area.slope, aagpd1213, probs = 0.25),
                    roofareaslope.Q2 =   wtd.quantile(roof.area.slope, aagpd1213, probs = 0.50),
                    roofareaslope.Q3 =   wtd.quantile(roof.area.slope, aagpd1213, probs = 0.75),
                    ceilingground.Q1 =   wtd.quantile(cheight0,        aagpd1213, probs = 0.25),
                    ceilingground.Q2 =   wtd.quantile(cheight0,        aagpd1213, probs = 0.50),
                    ceilingground.Q3 =   wtd.quantile(cheight0,        aagpd1213, probs = 0.75),
                    ceilingfirst.Q1 =    wtd.quantile(cheight1,        aagpd1213, probs = 0.25, na.rm = T),
                    ceilingfirst.Q2 =    wtd.quantile(cheight1,        aagpd1213, probs = 0.50, na.rm = T),
                    ceilingfirst.Q3 =    wtd.quantile(cheight1,        aagpd1213, probs = 0.75, na.rm = T),
                    imd.Q1 =             wtd.quantile(imd,             aagpd1213, probs = 0.25),
                    imd.Q2 =             wtd.quantile(imd,             aagpd1213, probs = 0.50),
                    imd.Q3 =             wtd.quantile(imd,             aagpd1213, probs = 0.75),
                    # Rates and Stats
                    gasgrid.yes =        round(sum(aagpd1213[gasgrid == "yes"])/sum(aagpd1213)*100,1),
                    tank.wellins =       round(sum(aagpd1213[tankins == "Well Insulated"])/sum(aagpd1213[tankins != "No Tank"])*100,1),
                    wallins =            round(sum(aagpd1213[wallinsy %in% c("cavity with insulation","solid with insulation")])/sum(aagpd1213)*100,1),
                    loftins =            round(sum(aagpd1213[LoftIns  %in% c("Well Insulated")])/sum(aagpd1213)*100,1),
                    # Locations
                    citycentre =         sum(aagpd1213[arnatx == "city centre"], na.rm = T),
                    otherurban =         sum(aagpd1213[arnatx == "other urban centre"], na.rm = T),
                    suburban =           sum(aagpd1213[arnatx == "suburban residential"], na.rm = T),
                    ruralres =           sum(aagpd1213[arnatx == "rural residential"], na.rm = T),
                    villagecentre =      sum(aagpd1213[arnatx == "village centre"], na.rm = T),
                    rural =              sum(aagpd1213[arnatx == "rural"], na.rm = T),
                    # Tenure
                    own =                sum(aagpd1213[tenure4x == "owner occupied"], na.rm = T),
                    rent.private =       sum(aagpd1213[tenure4x == "private rented"], na.rm = T),
                    rent.la =            sum(aagpd1213[tenure4x == "local authority"], na.rm = T),
                    rent.rsl =           sum(aagpd1213[tenure4x == "RSL"], na.rm = T)
                    )



arch$tank.wellins[is.na(arch$tank.wellins)] = 0 

#Sort Retults
arch <- arch[order(-arch$ndwel),]

#Save Results
write.csv(arch,"../jobs_data/archetype_summary.csv", row.names = FALSE)

