# Do text search for retrofit terms

acc1 <- readxl::read_xlsx("D:/Users/earmmor/OneDrive - University of Leeds/GLIDER/03 Work Packages/WP3 Institutional incumbents/TwitterAccountsReview_withDesc_v2.xlsx", sheet =  "TwitterAccountsReview_withDesc")

library(tidytext)
library(dplyr)

text_df <- data_frame(account = 1:nrow(acc1), text = acc1$description)
words_df <- unnest_tokens(text_df, word, text)
nrow(words_df)
words_df <- anti_join(words_df, stop_words) # Reccmoe common wors like the. and, I
nrow(words_df)
text_count <- count(words_df, word, sort = T)
write.csv(text_count,"twitter/data/word_count.txt")

#Manual + automatest list of retrofit key words

keywords <- c("architecture","architects", "architect", "architectural",
              "building", "builder","built", "buildings",
              "construction","climate change","climate",
              "design",
              "energy efficiency","efficiency","eco-renovation", "eco-retrofit","eco", "electrician", "energy", "environmental",
              "environment","engineering", 
              "green", "glazing",
              "housing", "homes","home", "house",
              "insulation",
              "low carbon",
              "plumber","plumbing","property", "planning", "passivhaus",
              "windows",
              "retrofit","rennovation","refurbishment", "renewables", "renewable", "roofing",
              "smart","sustainable","solar","sustainability")


matches <- grep(pattern = paste(keywords,collapse="|"), x = acc1$description, ignore.case = TRUE, value = FALSE)

for(i in matches){
  if(is.na(acc1$`Account Category`[i])){
    acc1$`Account Category`[i] <- "Keep"
  }
}

write.csv(acc1,"twitter/data/TwitterAccountsReview_withDescAnal.csv")
