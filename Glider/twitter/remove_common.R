#Remove common twitter accounts
#read in list of connections
connect.all <- readRDS(paste0("twitter/data/Connections-livedump-2017-08-04.Rds"))

#Get list of accounts to keep and remove
celeb <- read.csv("twitter/data/Top-1000-Celebrity-Twitter-Accounts.csv")
celeb <- celeb[!duplicated(celeb$twitter),]

topfol <- read.csv("twitter/data/commonaccounts.csv", stringsAsFactors = F)
core <- read.csv("twitter/accounts_cores.csv", stringsAsFactors = F)

keep <- core$Name
keep <- c(keep, topfol$screenName[topfol$type == "KEEP"])
keep <- unique(keep)

remove <- topfol$screenName[topfol$type != "KEEP"]
remove <- c(remove,celeb$twitter)
remove <- unique(remove)
remove <- remove[!(remove %in% keep)]

nrow(connect.all)
connect.all <- connect.all[!(connect.all$screenName %in% remove),]
nrow(connect.all)

#take out forign language
connect.all <- connect.all[(connect.all$lang %in% c("en","en-gb","en-GB")) | is.na(connect.all$lang),]
nrow(connect.all)


unique(connect.all$lang)

#take out accounts not in the UK
#locs <- data.frame(id = unique(connect.all$location), count = 0)
#locs$id <- as.character(locs$id)
#connect.all$location <- as.character(connect.all$location)

#getlocs <- function(a){
#  n <- length(connect.all$location[connect.all$location == locs$id[a] ])
#  return(n)
#}


#library(pbapply)
#locs$count <- pblapply(1:nrow(locs), getlocs2)
#locs$count <- as.integer(locs$count)

#save out locations list
#write.csv(locs,"twitter/data/locations.csv")

locs <- read.csv("twitter/data/locations.csv")
locs <- locs[,c("id","count","keep")]

locs.rem <- locs[!is.na(locs$keep),]
locs.rem <- locs.rem[locs.rem$keep == "n",]

nrow(connect.all)
connect.all <- connect.all[!(connect.all$location %in% locs.rem$id),]
nrow(connect.all)


saveRDS(connect.all, paste0("twitter/data/Connections-cleaned-",Sys.Date(),".Rds"))


