# Second round of data gathering
#Load in the core functions
source("twitter/secrets.R")
source("twitter/functions.R")

# Get output of the first round

conn.simple <- readRDS("twitter/data/parReRun/FirstRoundConnections.Rds")

#sort by importance
conn.simple <- conn.simple[order(-conn.simple$weight),]

#make  a list of the accounts to check
accounts <- unique(conn.simple$to)

#remove the ones we have already done
accounts <- accounts[!accounts %in% unique(conn.simple$from)]

# remove junk accoutns that we daon;t want to do
common <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP3 Institutional incumbents/TwitterAccountsReview_withDescAnal_malcolm.xlsx", sheet =  "TwitterAccountsReview_withDescA")
common <- common[,c("name","Account Category","Account Type")]
common <- common[!is.na(common$`Account Category`),]
common <- common$name[common$`Account Category` == "Junk"]

length(accounts)
accounts <- accounts[!accounts %in% common]
length(accounts)

# We want to do in batches so that we can save our progress
# make batchse of 100

results <- get.SNAdata(accounts, temp.fld = "F:/Twitter/SecondRoundParReRun", batch.start = 176, trim = TRUE)



saveRDS(results, "twitter/data/parReRun/SecondRoundRawData.Rds")

#batchet 15, 16 may be mangled
