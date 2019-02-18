library(dplyr)

# Readin in gavins new data
file.path = "D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP2 Economic risks/Retrofit_specs_unit_costs_machine_readable_v6.xlsx"

retrofit.options <- readxl::read_xlsx(file.path, sheet = "by_retrofit_option")
names(retrofit.options) <- c("ID","sub-project","variations","options","compatible_standard","materials","hired plant","labour","wall",	"floor",	"roof",	"solar",	"window",	"energy",	"type")
retrofit.options <- retrofit.options[3:nrow(retrofit.options),]
retrofit.options <- retrofit.options[!is.na(retrofit.options$ID),]

materials <- readxl::read_xlsx(file.path, sheet = "materials_list")
names(materials) <- c("ID",	"sub-project",	"variations",	"options",	"compatible_standard",	"materials",	"cost",	"unit","variable",	"notes",	"sources")
materials <- materials[materials$ID != "NONE",]
materials <- materials[3:nrow(materials),]
materials <- as.data.frame(materials)

materials$cost = as.numeric(materials$cost)


arch <- read.csv("../jobs_data/archetype_summary.csv", stringsAsFactors = F)

#Propose Retrofit Options

# convert options to lists
retrofit.options$wall <- strsplit(gsub(" ","",retrofit.options$wall), ",")
retrofit.options$floor <- strsplit(gsub(" ","",retrofit.options$floor), ",")
retrofit.options$roof <- strsplit(gsub(" ","",retrofit.options$roof), ",")
retrofit.options$solar <- strsplit(gsub(" ","",retrofit.options$solar), ",")
retrofit.options$window <- strsplit(gsub(" ","",retrofit.options$window), ",")
retrofit.options$energy <- strsplit(gsub(" ","",retrofit.options$energy), ",")
retrofit.options$type <- strsplit(gsub(" ","",retrofit.options$type), ",")

retrofit.options.x <- retrofit.options[grepl("X",retrofit.options$`compatible_standard`),]
retrofit.options.y <- retrofit.options[grepl("Y",retrofit.options$`compatible_standard`),]
retrofit.options.z <- retrofit.options[grepl("Z",retrofit.options$`compatible_standard`),]

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
cost.options <- function(x, type){
  arch.sub = arch[x,]
  
  if(type == "x"){
    options <- unlist(arch.sub$retrofit.options.x)
  }else if(type == "y"){
    options <- unlist(arch.sub$retrofit.options.y)
  }else if(type == "z"){
    options <- unlist(arch.sub$retrofit.options.z)
  }else{
    message("No Type")
    stop()
  }
  
  arch.sub = arch.sub[,c("groundfloorarea.Q2","intwallarea.extwalls.Q2",
                         "extwallarea.Q2","intwallarea.frontwall.Q2",
                         "intwallarea.frontwall.Q2","extwallarea.exceptfront.Q2",
                         "roofareaplan.Q2", "roofareaslope.Q2", 
                         "dpcperim.Q2", "windows.Q2",
                         "doors.Q2","intwallarea.extpartywalls.Q2"
                           )]
  
  names(arch.sub) = c("ground floor area","internal wall area (external walls)",
                      "external wall area","internal wall area (front external wall only)",
                      "internal wall area (front external wall only)","external wall area (all exception front elevation)",
                      "plan roof area","slope roof area",
                      "DPC Perimeter","windows",
                      "doors","internal wall area (external and party walls)")
  
  arch.sub = as.data.frame(t(arch.sub))
  arch.sub$variaible = rownames(arch.sub)
  names(arch.sub) = c("value","variable")
  arch.sub = rbind(arch.sub,data.frame(value = 1, variable = "per dwelling"))
  rownames(arch.sub) = 1:nrow(arch.sub)
  
  materials.sub <- materials[materials$ID %in% options, ]
  materials.sub <- materials.sub[,c("ID","compatible_standard",	"materials",	"cost",	"unit","variable")]
  
  materials.sub = left_join(materials.sub, arch.sub, by = "variable")
  materials.sub$cost_total = materials.sub$cost * materials.sub$value
  
  total.cost = sum(materials.sub$cost_total, na.rm = T)
  materials.sub$cost_total[materials.sub$variable == "percent1"] = total.cost/100
  materials.sub$cost_total[materials.sub$variable == "percent4"] = total.cost/100 * 4
  materials.sub$cost_total[materials.sub$variable == "percent12"] = total.cost/100 * 12
  materials.sub = materials.sub[,c("ID","compatible_standard","cost_total")]
  return(materials.sub)
}

costs.list.x = lapply(1:nrow(arch), cost.options, type = "x")
costs.list.y = lapply(1:nrow(arch), cost.options, type = "y")
costs.list.z = lapply(1:nrow(arch), cost.options, type = "z")



# Check for duplicates
duplicate.options = function(costs.sub){
  costs.sub$sel = grepl("1",unlist(costs.sub$`compatible_standard`, recursive=FALSE))
  costs.sub$type = substr(costs.sub$ID,1,2)
  #costs.sub$min = suppressWarnings(sapply(costs.sub$type,function(x){min(costs.sub$cost[costs.sub$type == x & costs.sub$sel], na.rm = T)}))
  #costs.sub$average = suppressWarnings(sapply(costs.sub$type,function(x){mean(costs.sub$cost[costs.sub$type == x & costs.sub$sel], na.rm = T)}))
  #costs.sub = costs.sub[!costs.sub$sel | (costs.sub$sel &  costs.sub$cost == costs.sub$min), ]
  costs.sub.a = costs.sub[!costs.sub$sel,]
  costs.sub.b = costs.sub[costs.sub$sel,]
  costs.sub.c = costs.sub.b %>%
                  group_by(type) %>%
                  summarise(ID = paste(unique(ID), collapse = " "),
                            #mincost = min(cost),
                            #maxcost = max(cost),
                            cost = mean(cost),
                            compatible_standard = unique(compatible_standard))
  costs.sub.a = costs.sub.a[,c("ID","cost","compatible_standard","type")]
  costs.sub.c = costs.sub.c[,c("ID","cost","compatible_standard","type")]
  costs.sub.fin = rbind(costs.sub.a,costs.sub.c)
  
  return(costs.sub.fin)
}


clean.duplicateds = function(i, type){
  
  if(type == "x"){
    costs.sub = costs.list.x[[i]]
  }else if(type == "y"){
    costs.sub = costs.list.y[[i]]
  }else if(type == "z"){
    costs.sub = costs.list.z[[i]]
  }else{
    message("No Type")
    stop()
  }
  
  options.sub = retrofit.options[retrofit.options$ID %in% costs.sub$ID,]
  options.sub = options.sub[,c("ID","compatible_standard")]
  costs.sub = costs.sub %>%
                group_by(ID) %>%
                  summarise(cost = sum(cost_total))
  
  costs.sub = left_join(costs.sub, options.sub, by = c("ID"))
  costs.sub = duplicate.options(costs.sub = costs.sub)
  return(costs.sub)
  
}

costs.list.x.clean = lapply(1:length(costs.list.x), clean.duplicateds, type = "x")
costs.list.y.clean = lapply(1:length(costs.list.y), clean.duplicateds, type = "y")
costs.list.z.clean = lapply(1:length(costs.list.z), clean.duplicateds, type = "z")

# Join inot the dataframe
arch$costs.x = costs.list.x.clean
arch$costs.y = costs.list.y.clean
arch$costs.z = costs.list.z.clean

arch$costs.x.total = round(sapply(arch$costs.x,function(x){sum(x$cost)}),0)
arch$costs.y.total = round(sapply(arch$costs.y,function(x){sum(x$cost)}),0)
arch$costs.z.total = round(sapply(arch$costs.z,function(x){sum(x$cost)}),0)

saveRDS(arch,"../jobs_data/archetype_withcosts.Rds")

summary(arch$costs.x.total[arch$ndwel > 20000]) #gets 80% of the stock for only 13% of the archetypes
summary(arch$costs.y.total[arch$ndwel > 20000])
summary(arch$costs.z.total[arch$ndwel > 20000])

# find some intresting archetypes

arch_interesting = arch[arch$costs.x.total %in% c(min(arch$costs.x.total[arch$ndwel > 20000]),quantile(arch$costs.x.total[arch$ndwel > 20000], probs = 0.25),median(arch$costs.x.total[arch$ndwel > 20000]),mean(arch$costs.x.total[arch$ndwel > 20000]),quantile(arch$costs.x.total[arch$ndwel > 20000], probs = 0.75),max(arch$costs.x.total[arch$ndwel > 20000])) |
                        arch$costs.y.total %in% c(min(arch$costs.y.total[arch$ndwel > 20000]),quantile(arch$costs.y.total[arch$ndwel > 20000], probs = 0.25),median(arch$costs.y.total[arch$ndwel > 20000]),mean(arch$costs.y.total[arch$ndwel > 20000]),quantile(arch$costs.y.total[arch$ndwel > 20000], probs = 0.75),max(arch$costs.y.total[arch$ndwel > 20000])) |
                        arch$costs.z.total %in% c(min(arch$costs.z.total[arch$ndwel > 20000]),quantile(arch$costs.z.total[arch$ndwel > 20000], probs = 0.25),median(arch$costs.z.total[arch$ndwel > 20000]),mean(arch$costs.z.total[arch$ndwel > 20000]),quantile(arch$costs.z.total[arch$ndwel > 20000], probs = 0.75),max(arch$costs.z.total[arch$ndwel > 20000]))
                          ,]

arch_interesting = arch_interesting[arch_interesting$ndwel > 20000,]

arch_interesting$retrofit.options.x = lapply(arch_interesting$costs.x, function(x){paste(x$ID, collapse = " ")})
arch_interesting$retrofit.options.y = lapply(arch_interesting$costs.y, function(x){paste(x$ID, collapse = " ")})
arch_interesting$retrofit.options.z = lapply(arch_interesting$costs.z, function(x){paste(x$ID, collapse = " ")})

arch_interesting$costs.x = NULL
arch_interesting$costs.y = NULL
arch_interesting$costs.z = NULL

arch_interesting = t(arch_interesting)


write.csv(arch_interesting,"../jobs_data/archetype_examples.csv")

