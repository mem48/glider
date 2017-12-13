devtools::install_github("virgesmith/humanleague")

library(humanleague)
library(dplyr)
?humanleague


ageByGender = array(c(1,2,5,3,4,3,4,5,1,2), dim=c(5,2))
ageByEthnicity = array(c(4,6,5,6,4,5), dim=c(3,2))
seed = array(rep(1,30), dim=c(5,2,3))
res = qisi(seed, list(c(1,2), c(3,2)), list(ageByGender, ageByEthnicity))

result <- res$result
expectation <- res$expectation
conv <- res$conv
pop <- res$pop
chiSq <- res$chiSq
pValue <- res$pValue



arch <- readRDS("../jobs_data/archetype_summary_retrofitopts.Rds")
comb <- readRDS("../jobs_data/combined_2013_arch.Rds")
retro.ops <- read.csv("../jobs_data/retrofit_options_library.csv")

# Lets make a mulidimensional array

comb.sub <- comb[comb$archcode %in% arch$archcode[1:144],] #Only take the top 80% or archetypes
sum(comb.sub$aagpd1213) / sum(comb$aagpd1213)

comb.sub <- comb.sub[,c("archcode","wallinsy","winage","arnatx","LoftIns","aagpd1213")]

# Group by the variaibles
comb.group <- comb.sub %>% group_by(archcode,wallinsy,winage,arnatx,LoftIns) %>% summarise(sum(aagpd1213))
sum(comb.group$`sum(aagpd1213)`) == sum(comb.sub$aagpd1213)

comb.group <- as.data.frame(comb.group)
names(comb.group) <- c("archcode","wallinsy","winage","arnatx","LoftIns","aagpd1213")


#Convert to Numbers
comb.group$archcode <- as.factor(comb.group$archcode)
archcodes <- levels(comb.group$archcode)
archcodes
comb.group$archcode <- as.numeric(comb.group$archcode)

comb.group$wallinsy <- as.factor(comb.group$wallinsy)
wallinsy <- levels(comb.group$wallinsy)
wallinsy
comb.group$wallinsy <- as.numeric(comb.group$wallinsy)

comb.group$winage <- as.factor(comb.group$winage)
winage <- levels(comb.group$winage)
winage
comb.group$winage <- as.numeric(comb.group$winage)

comb.group$arnatx <- as.factor(comb.group$arnatx)
arnatx <- levels(comb.group$arnatx)
arnatx
comb.group$arnatx <- as.numeric(comb.group$arnatx)

comb.group$LoftIns <- as.factor(comb.group$LoftIns)
LoftIns <- levels(comb.group$LoftIns)
LoftIns
comb.group$LoftIns <- as.numeric(comb.group$LoftIns)


#Make the Array
# https://stackoverflow.com/questions/22654409/r-convert-data-frame-to-multi-dimensional-matrix
mda <- array(data = NA, 
             dim = c(max(comb.group$archcode), 
                     max(comb.group$wallinsy), 
                     max(comb.group$winage), 
                     max(comb.group$arnatx), 
                     max(comb.group$LoftIns)),
              dimnames = list(archcodes,wallinsy,winage,arnatx,LoftIns)       
                     )


for (i in 1:nrow(comb.group)){
  mda[comb.group[i,"archcode"], comb.group[i,"wallinsy"], comb.group[i,"winage"], comb.group[i,"arnatx"], comb.group[i,"LoftIns"]] <- comb.group[i, "aagpd1213"]
}

sum(mda, na.rm = T) / sum(comb.sub$aagpd1213) == 1

foo <- mda[1,,,,]
foo






