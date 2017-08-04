#Remove common twitter accounts

#read in list of connections

connect.all <- readRDS(paste0("twitter/data/Connections-livedump-2017-08-04.Rds"))

large <- connect.all[connect.all$followersCount > 100000,]
large <- large[!duplicated(large$screenName),]

write.csv(large,"twitter/data/commonaccounts.csv")
