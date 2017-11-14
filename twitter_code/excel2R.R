# join excel speadsheet to twiit account descriptions

acc1 <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP3 Institutional incumbents/TwitterAccountsReview.xlsx", sheet =  "TwitterAccountsReview")
acc2 <- readRDS("twitter/TwitterAccountsReview-Detailed.Rds")

#remove unwanted columns
acc2 <- acc2[,c("description","statusesCount","followersCount","favoritesCount","friendsCount","name","created","protected","verified","screenName","location","lang","id")]


library(dplyr)

acc3 <- left_join(acc1,acc2, by = c("name" = "screenName"))
write.csv(acc3,"twitter/TwitterAccountsReview_withDesc.csv")
