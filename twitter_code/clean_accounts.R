#Clean up tweets list
acc.all <- readRDS("twitter/data/broadAccounts.Rds")

nrow(acc.all)
#Remove protected accounts as can't used them in the analysis
acc.clean <- acc.all[acc.all$protected == F,]
nrow(acc.all)

#Remove inactive accounts
min.tweets <- 10
min.followers <- 10
min.friends <- 10

acc.clean <- acc.clean[acc.clean$statusesCount >=  min.tweets,]
nrow(acc.clean)

acc.clean <- acc.clean[acc.clean$followersCount >=  min.followers,]
nrow(acc.clean)

acc.clean <- acc.clean[acc.clean$friendsCount >=  min.friends,]
nrow(acc.clean)

#Remove forign langauge accounts
acc.clean <- acc.clean[acc.clean$lang %in% c("en","en-gb","en-GB"),]
nrow(acc.clean)

#remove accounts in locations outside the uk
locs <- read.csv("twitter/data/locations.csv")
locs <- locs[locs$keep == "n",]
acc.clean <- acc.clean[!acc.clean$location %in% locs$id,]
nrow(acc.clean)

#Search Specifically for USA
locs.usa <- regexpr('USA', acc.clean$location)
acc.clean <- acc.clean[locs.usa == -1,]
nrow(acc.clean)

#remove accounts that use emoji in ther name
emoji <- regexpr('<U+', acc.clean$name)
acc.clean <- acc.clean[emoji == -1,]
nrow(acc.clean)

regexpr('<U+', "<U+1D04><U+1D3C><U+1D3A>s<U+1D1B>")
#remove accounts know to be celebrities etc
celeb <- read.csv("twitter/data/commonaccounts.csv")
celeb <- celeb[celeb$type != "KEEP",]
acc.clean <- acc.clean[!acc.clean$screenName %in% celeb$screenName,]
nrow(acc.clean)

saveRDS(acc.clean,"twitter/data/broadAccounts-clean.Rds")


