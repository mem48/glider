library(tidytext)
library(dplyr)

## Uses the values from contruct_network

names(tweets)
head(tweets)




text_df <- data_frame(id = tweets$id , account = tweets$screenName, text = tweets$text)
words_df <- unnest_tokens(text_df, word, text)
nrow(words_df)
words_df <- anti_join(words_df, stop_words) # Remove common wors like the. and, I
nrow(words_df)
text_count <- count(words_df, word, sort = T)
#write.csv(text_count,"twitter/data/word_count.txt")

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


matches <- grep(pattern = paste(keywords,collapse="|"), x = tweets$text, ignore.case = TRUE, value = FALSE)
matches <- 1:nrow(tweets) %in% matches

tweets$containsKeyWord = matches

tweets.sum <- tweets[,c("screenName","containsKeyWord")]
tweets.sum <- tweets.sum[tweets.sum$containsKeyWord,]
acc.sum <- as.data.frame(table(tweets.sum))
acc.sum <- acc.sum[,c("screenName","Freq")]

accounts <- left_join(accounts,acc.sum, by = c("screenName" = "screenName"))
accounts$Freq[is.na(accounts$Freq)] <- 0

names(tweets)


summary()

for(i in matches){
  if(is.na(acc1$`Account Category`[i])){
    acc1$`Account Category`[i] <- "Keep"
  }
}

write.csv(acc1,"twitter/data/TwitterAccountsReview_withDescAnal.csv")