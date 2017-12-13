library(dplyr)


#arch <- read.csv("../jobs_data/archetype_summary.csv", stringsAsFactors = F)
comb <- readRDS("../jobs_data/combined_2013_arch.Rds")
retro.ops <- read.csv("../jobs_data/retrofit_options_library.csv", stringsAsFactors = F)



comb.sub <- comb[comb$archcode %in% arch$archcode[1:144],] #Only take the top 80% or archetypes
sum(comb.sub$aagpd1213) / sum(comb$aagpd1213)

comb.sub <- comb.sub[,c("archcode","wallinsy","winage","arnatx","LoftIns","aagpd1213")]

# Group by the variaibles
comb.group <- comb.sub %>% group_by(archcode,wallinsy,winage,arnatx,LoftIns) %>% summarise(sum(aagpd1213))
sum(comb.group$`sum(aagpd1213)`) == sum(comb.sub$aagpd1213)

comb.group <- as.data.frame(comb.group)
names(comb.group) <- c("archcode","wallinsy","winage","arnatx","LoftIns","aagpd1213")


# For each subgroup work out which retrofit options are suitable

# convert options to lists
retro.ops$wall <- strsplit(gsub(" ","",retro.ops$wall), ",")
retro.ops$floor <- strsplit(gsub(" ","",retro.ops$floor), ",")
retro.ops$roof <- strsplit(gsub(" ","",retro.ops$roof), ",")
retro.ops$solar <- strsplit(gsub(" ","",retro.ops$solar), ",")
retro.ops$window <- strsplit(gsub(" ","",retro.ops$window), ",")
retro.ops$energy <- strsplit(gsub(" ","",retro.ops$energy), ",")
retro.ops$type <- strsplit(gsub(" ","",retro.ops$type), ",")

retro.ops$winage <- strsplit(retro.ops$winage, ",")
retro.ops$arnatx <- strsplit(retro.ops$arnatx, ",")
retro.ops$LoftIns <- strsplit(retro.ops$LoftIns, ",")
retro.ops$wallinsy <- strsplit(retro.ops$wallinsy, ",")


#Get Retrofit Options
get.options <- function(a){
  #Get Codes
  code <- comb.sub$archcode[a]
  wall.id <- substr(code,3,3)
  floor.id <- substr(code,5,5)
  roof.id <- substr(code,7,7)
  solar.id <- substr(code,9,9)
  window.id <- substr(code,11,11)
  energy.id <- substr(code,13,13)
  type.id <- substr(code,1,1)
  winage.id <- comb.sub$winage[a]
  arnatx.id <- comb.sub$arnatx[a]
  loftins.id <- comb.sub$loftins[a]
  wallinsy.id <- comb.sub$wallins[a]
  
  
  
  
  retro.ops.sub <- retro.ops[(unlist(lapply(retro.ops$wall, function(x) {wall.id %in% x})) | retro.ops$wall == "All") & 
                     (unlist(lapply(retro.ops$floor, function(x) {floor.id %in% x})) | retro.ops$floor == "All") & 
                     (unlist(lapply(retro.ops$roof, function(x) {roof.id %in% x})) | retro.ops$roof == "All") & 
                     (unlist(lapply(retro.ops$solar, function(x) {solar.id %in% x})) | retro.ops$solar == "All") & 
                     (unlist(lapply(retro.ops$window, function(x) {window.id %in% x})) | retro.ops$window == "All") & 
                     (unlist(lapply(retro.ops$energy, function(x) {energy.id %in% x})) | retro.ops$energy == "All") & 
                     (unlist(lapply(retro.ops$type, function(x) {type.id %in% x})) | retro.ops$type == "All")
                   ,]
  
  return(retro.ops.sub$id)
}
