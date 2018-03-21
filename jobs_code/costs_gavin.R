# Readin in gavins new data

retrofit.options <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP2 Economic risks/Retrofit_specs_unit_costs_machine_readable_v2.xlsx", sheet = "by_retrofit_option")
names(retrofit.options) <- c("cost £","ID","sub-project","variations","options","compatible standard","materials","hired plant","labour","wall",	"floor",	"roof",	"solar",	"window",	"energy",	"type")
retrofit.options <- retrofit.options[3:nrow(retrofit.options),]
retrofit.options$`cost £` <- NULL
retrofit.options <- retrofit.options[!is.na(retrofit.options$ID),]

materials <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP2 Economic risks/Retrofit_specs_unit_costs_machine_readable_v2.xlsx", sheet = "materials_list")
names(materials) <- c("ID",	"sub-project",	"variations",	"options",	"compatible standard",	"materials",	"cost",	"unit","variaible",	"notes",	"sources")
materials <- materials[3:nrow(materials),]
materials <- materials[materials$ID != "NONE",]
materials <- as.data.frame(materials)


arch <- readRDS("../jobs_data/archetype_summary_retrofitopts.Rds")

#Propose Retrofit Options

# convert options to lists
retrofit.options$wall <- strsplit(gsub(" ","",retrofit.options$wall), ",")
retrofit.options$floor <- strsplit(gsub(" ","",retrofit.options$floor), ",")
retrofit.options$roof <- strsplit(gsub(" ","",retrofit.options$roof), ",")
retrofit.options$solar <- strsplit(gsub(" ","",retrofit.options$solar), ",")
retrofit.options$window <- strsplit(gsub(" ","",retrofit.options$window), ",")
retrofit.options$energy <- strsplit(gsub(" ","",retrofit.options$energy), ",")
retrofit.options$type <- strsplit(gsub(" ","",retrofit.options$type), ",")

retrofit.options.x <- retrofit.options[retrofit.options$`compatible standard` %in% c("X","ALL"),]
retrofit.options.y <- retrofit.options[retrofit.options$`compatible standard` %in% c("Y","ALL"),]
retrofit.options.z <- retrofit.options[retrofit.options$`compatible standard` %in% c("Z","ALL"),]

#Get Retrofit Options
get.options <- function(a, data){
  #Get Codes
  code <- arch$archcode[a]
  wall.id <- substr(code,3,3)
  floor.id <- substr(code,5,5)
  roof.id <- substr(code,7,7)
  solar.id <- substr(code,9,9)
  window.id <- substr(code,11,11)
  energy.id <- substr(code,13,13)
  type.id <- substr(code,1,1)
  
  retrofit.options.sub <- data[(unlist(lapply(data$wall, function(x) {wall.id %in% x})) | data$wall == "All") & 
                     (unlist(lapply(data$floor, function(x) {floor.id %in% x})) | data$floor == "All") & 
                     (unlist(lapply(data$roof, function(x) {roof.id %in% x})) | data$roof == "All") & 
                     (unlist(lapply(data$solar, function(x) {solar.id %in% x})) | data$solar == "All") & 
                     (unlist(lapply(data$window, function(x) {window.id %in% x})) | data$window == "All") & 
                     (unlist(lapply(data$energy, function(x) {energy.id %in% x})) | data$energy == "All") & 
                     (unlist(lapply(data$type, function(x) {type.id %in% x})) | data$type == "All")
                   ,]
  
  return(retrofit.options.sub$ID)
}


get.options(1, retrofit.options.x)
get.options(1, retrofit.options.y)
get.options(1, retrofit.options.z)

arch$retrofit.options.x <- lapply(1:nrow(arch), get.options, data = retrofit.options.x)
arch$retrofit.options.y <- lapply(1:nrow(arch), get.options, data = retrofit.options.y)
arch$retrofit.options.z <- lapply(1:nrow(arch), get.options, data = retrofit.options.z)

summary(lengths(arch$retrofit.options.x))
summary(lengths(arch$retrofit.options.y))
summary(lengths(arch$retrofit.options.z))

saveRDS(arch,"../jobs_data/archetype_summary_retrofitopts_gavin.Rds")

#Cost Retrofit Options
cost.options <- function(x){
  options <- unlist(arch$retrofit.options.x[x])
  materials.sub <- materials[materials$ID %in% options, ]
  materials.sub <- materials.sub[,c("ID","compatible standard",	"materials",	"cost",	"unit","variable")]
}

foo <- unique(materials$variaible)
foo[order(foo)]
