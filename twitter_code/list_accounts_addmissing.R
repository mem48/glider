# Create List of Accounts
library(igraph)
library(dplyr)

# Read In Accounts 

accounts_old  <- readxl::read_xlsx("../twitter_data/accounts_fulldata.xlsx", sheet =  "accounts_fulldata")
accounts_old <- accounts_old[,c("name","Account Category","Account Type")]
accounts <- readRDS("../twitter_data/all/accounts.Rds")

accounts <- accounts[!accounts$screenName %in% accounts_old$name,]

g <- readRDS("../twitter_data/parRerun/All_Data.Rds")

# Calcualte Values quick values
V(g)$degree.total <- degree(g, mode = "total")
V(g)$degree.in <- degree(g, mode = "in")
V(g)$degree.out <- degree(g, mode = "out")
V(g)$strength.total <- strength(g, mode = "total")
V(g)$strength.in <- strength(g, mode = "in")
V(g)$strength.out <- strength(g, mode = "out")


# Clacualte Slow values
V(g)$closeness <- closeness(g)
V(g)$eigenvector <-  eigen_centrality(g, directed = T, scale = F, weights = E(g)$weight)$vector
V(g)$PageRank <- page_rank(g, directed = TRUE, damping = 0.85, weights = E(g)$weight)$vector
V(g)$between <- betweenness(g)

#Produce Summary Table
verts <- igraph::as_data_frame(g, what="vertices")

# Join Togther
accounts$screenName[duplicated(accounts$screenName)]

accounts <- accounts[!duplicated(accounts$screenName),]
verts2 <- left_join(verts, accounts, by = c("name" = "screenName"))


#Read in Calssification data
common <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP3 Institutional incumbents/TwitterAccountsReview_withDescAnal_malcolm.xlsx", sheet =  "TwitterAccountsReview_withDescA")


verts2 <- left_join(verts2, common, by = c("name" = "name"))
verts2 <- verts2[, names(verts2)[!names(verts2) %in% c("url","id","listedCount","followRequestSent","profileImageUrl")] ]

names(verts2)

write.csv(verts2,"../twitter_data/accounts_fulldata.csv", row.names = F)


