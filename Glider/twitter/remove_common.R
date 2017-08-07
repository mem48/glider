#Remove common twitter accounts



#read in list of connections

connect.all <- readRDS(paste0("E:/OneDrive - University of Leeds/Glider - Private/WP3/twitterdata/data/Connections-livedump-2017-08-04.Rds"))

#large <- connect.all[connect.all$followersCount > 100000,]
#large <- large[!duplicated(large$screenName),]

#write.csv(large,"twitter/data/commonaccounts.csv")


#read in to 1000
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

#take out forign accounts
nrow(connect.all)
connect.all <- connect.all[(connect.all$lang %in% c("en","en-gb","en-GB")) | is.na(connect.all$lang),]
nrow(connect.all)
unique(connect.all$lang)
locs <- data.frame(id = unique(connect.all$location), count = 0)

getlocs <- function(loc){
  n <- length(connect.all$location[connect.all$location == loc])
}

locs$count <- lapply(1:nrow(locs), )
