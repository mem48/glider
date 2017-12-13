# Second round of data gathering
#Load in the core functions
source("secrets.R")
source("functions.R")

# Get top candiate for roaund 3 accounts

accounts <- read.csv("../twitter_data/parRerun/round3_accounts.csv", stringsAsFactors = F)
accounts <- accounts$x

# We want to do in batches so that we can save our progress
# make batchse of 50

results <- get.SNAdata(accounts, temp.fld = "F:/Twitter/ThirdRound", batch.start = 24, trim = TRUE)

