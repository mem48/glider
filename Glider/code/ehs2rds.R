#Read in EHS and convert to RDS
library(foreign)
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
services <- services[,c("aacode","Finchloc","Finchtyp","Findisty","Finmhfue","Finmhboi","Finchbag",
                        "Finchoff","Finchthe","Finchtim","Finchove","Finchrom","Finchcon","Finchtrv","Finchtzc","Finchdst",
                        "Finoheat","Finohphs","Finohtyp",
                        "Finwheat","Finwhcpr","Finwhopr","Finwhoty","Finwhoac","Finwhoag","Finwhxpr","Finwhxty","Finwhxag",
                        "Finwsipr","Finwsiag","Finwdipr","Finwdiag",
                        "Finwsppr","Finwspty","Finwspag",
                        "Finwmppr","Finwmpty","Finwmpag",
                        "Finwhlpr","Finwhlty","Finwhlag",
                        "Finwhcyl","Finwhsiz","Finwhins","Finwhmms",
                        "Finwhcen","Finwhthe","Finlopos","Flitypes","Fliinsul","Finintyp","Flithick")]






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




