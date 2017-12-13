# Find ROund 3 Accounts

library(igraph)

g <- readRDS("../twitter_data/parRerun/SecondRound_300batches_Graph.Rds")
gorder(g)

#Remove the Junk Accounts
common <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP3 Institutional incumbents/TwitterAccountsReview_withDescAnal_malcolm.xlsx", sheet =  "TwitterAccountsReview_withDescA")
common <- common[,c("name","Account Category","Account Type")]
common <- common[!is.na(common$`Account Category`),]
remove <- common$name[common$`Account Category` %in% c("Junk","Media","Government")]
length(remove)

gorder(g)
g.trim <- delete.vertices(g, which(V(g)$name %in% remove)) # Discard the junk
g.trim <- delete.vertices(g.trim, which(degree(g.trim) < 10)) # Trim down to a manageable amount
gorder(g.trim)


# Calcualte Values quick values
V(g.trim)$degree.total <- degree(g.trim, mode = "total")
V(g.trim)$degree.in <- degree(g.trim, mode = "in")
V(g.trim)$degree.out <- degree(g.trim, mode = "out")
V(g.trim)$strength.total <- strength(g.trim, mode = "total")
V(g.trim)$strength.in <- strength(g.trim, mode = "in")
V(g.trim)$strength.out <- strength(g.trim, mode = "out")

verts <- igraph::as_data_frame(g.trim, what="vertices")
write.csv(verts,"../twitter_data/parRerun/verts_to_check.csv")

#Get the accounts that we have not done
# I.e. no out links
nrow(verts)
verts <- verts[verts$degree.out == 0,]
nrow(verts)

#Trim off the low values
verts <- verts[verts$degree.in >= 100,]
verts <- verts[verts$strength.in >= 500,]
verts <- verts[order(-verts$degree.in),]

round3 <- verts$name
write.csv(round3,"../twitter_data/parRerun/round3_accounts.csv", row.names = FALSE)
