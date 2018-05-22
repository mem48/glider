# proposes retrofit

#Get list of Archetyes

arch <- read.csv("../jobs_data/archetype_summary.csv", stringsAsFactors = F)
ropt <- read.csv("../jobs_data/retrofit_options.csv", stringsAsFactors = F)

# convert options to lists
ropt$wall <- strsplit(gsub(" ","",ropt$wall), ",")
ropt$floor <- strsplit(gsub(" ","",ropt$floor), ",")
ropt$roof <- strsplit(gsub(" ","",ropt$roof), ",")
ropt$solar <- strsplit(gsub(" ","",ropt$solar), ",")
ropt$window <- strsplit(gsub(" ","",ropt$window), ",")
ropt$energy <- strsplit(gsub(" ","",ropt$energy), ",")
ropt$type <- strsplit(gsub(" ","",ropt$type), ",")


#Get Retrofit Options
get.options <- function(a){
  #Get Codes
  code <- arch$archcode[a]
  wall.id <- substr(code,3,3)
  floor.id <- substr(code,5,5)
  roof.id <- substr(code,7,7)
  solar.id <- substr(code,9,9)
  window.id <- substr(code,11,11)
  energy.id <- substr(code,13,13)
  type.id <- substr(code,1,1)
  
  ropt.sub <- ropt[(unlist(lapply(ropt$wall, function(x) {wall.id %in% x})) | ropt$wall == "All") & 
                     (unlist(lapply(ropt$floor, function(x) {floor.id %in% x})) | ropt$floor == "All") & 
                     (unlist(lapply(ropt$roof, function(x) {roof.id %in% x})) | ropt$roof == "All") & 
                     (unlist(lapply(ropt$solar, function(x) {solar.id %in% x})) | ropt$solar == "All") & 
                     (unlist(lapply(ropt$window, function(x) {window.id %in% x})) | ropt$window == "All") & 
                     (unlist(lapply(ropt$energy, function(x) {energy.id %in% x})) | ropt$energy == "All") & 
                     (unlist(lapply(ropt$type, function(x) {type.id %in% x})) | ropt$type == "All")
                     ,]
  
  return(ropt.sub$id)
}

arch$ropt <- lapply(1:nrow(arch), get.options)
summary(lengths(arch$ropt))

saveRDS(arch,"data/archetype_summary_retrofitopts.Rds")
