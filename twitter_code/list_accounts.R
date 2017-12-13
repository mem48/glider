# Create List of Accounts
library(igraph)
library(dplyr)

# Read In Accounts 

accounts <- readRDS("../twitter_data/all/accounts.Rds")
g <- readRDS("../twitter_data/parRerun/All_Weighted_Tweets.Rds")


gorder(g)
g.trim <- delete.vertices(g, which(degree(g) < 200)) # Trim down to a manageable amount
gorder(g.trim)


# Calcualte Values quick values
V(g.trim)$degree.total <- degree(g.trim, mode = "total")
V(g.trim)$degree.in <- degree(g.trim, mode = "in")
V(g.trim)$degree.out <- degree(g.trim, mode = "out")
V(g.trim)$strength.total <- strength(g.trim, mode = "total")
V(g.trim)$strength.in <- strength(g.trim, mode = "in")
V(g.trim)$strength.out <- strength(g.trim, mode = "out")


# Clacualte Slow values
V(g.trim)$closeness <- closeness(g.trim)
V(g.trim)$eigenvector <-  eigen_centrality(g.trim, directed = T, scale = F, weights = E(g.trim)$weight)$vector
V(g.trim)$PageRank <- page_rank(g.trim, directed = TRUE, damping = 0.85, weights = E(g.trim)$weight)$vector
V(g.trim)$between <- betweenness(g.trim)

#Produce Summary Table
verts <- igraph::as_data_frame(g.trim, what="vertices")

# Join Togther
accounts$screenName[duplicated(accounts$screenName)]

accounts <- accounts[!duplicated(accounts$screenName),]
verts2 <- left_join(verts, accounts, by = c("name" = "screenName"))


#Read in Calssification data
common <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP3 Institutional incumbents/TwitterAccountsReview_withDescAnal_malcolm.xlsx", sheet =  "TwitterAccountsReview_withDescA")
common <- common[,c("name","Account Category","Account Type")]

verts2 <- left_join(verts2, common, by = c("name" = "name"))
verts2 <- verts2[, names(verts2)[!names(verts2) %in% c("url","id","listedCount","followRequestSent","profileImageUrl")] ]

names(verts2)

write.csv(verts2,"../twitter_data/accounts_fulldata.csv", row.names = F)


