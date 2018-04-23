# Now Analise the text of the tweets
# http://www.rdatamining.com/docs/twitter-analysis-with-r

library(tm)
library(dplyr)
library(parallel)
library(tidyr)
library(igraph)


accounts <- readRDS("../twitter_data/all/accounts.Rds")
friends <- readRDS("../twitter_data/all/friends")
likes <- readRDS("../twitter_data/all/likes")
tweets <- readRDS("../twitter_data/all/tweets.Rds")

tweets <- tweets[,c("text","favorited","favoriteCount", "replyToSN","created","truncated","replyToSID","id",
                    "replyToUID","screenName","retweetCount","isRetweet","retweeted")]
likes <- likes[,c("text","favorited","favoriteCount", "replyToSN","created","truncated","replyToSID","id",
                  "replyToUID","screenName","retweetCount","isRetweet","retweeted","favOf")]

# Thinks we are looking for 
# 1) use of the @tag
# 2) retweeting
# 3) hashtags
# 4) keywords

keywords.retrofit <-      c("eco-renovation", "eco-retrofit",
                            "greendeal","greenbuilding",
                            "heatpump",
                            "insulation",
                            "passivhaus","passivehouse",
                            "retrofit","renovation","refurbishment")
keywords.eco <-           c("carbon","climate change","climatechange" ,"climate","co2","cleanenergy",
                            "eco","energy", "environmental","environment","emissions","energyefficiency","efficiency",
                            "green",
                            "heat",
                            "lowcarbon","lowenergy",
                            "renewables", "renewable",
                            "smart","sustainable","solar","sustainability",
                            "pv","photovoltaic",
                            "zerocarbon")
keywords.construction <- c("architecture","architects", "architect", "architectural",
                           "building", "builder","built", "buildings","buildingcontrol","builders",
                           "construction",
                           "design",
                           "electrician","electricity","engineering","engineers",
                           "joiner",
                           "glazing",
                           "infrastructure",
                           "plumber","plumbing","plumbers","plaster","plasterer",
                           "windows",
                           "roofing","roof","repair","rmi")
keywords.housing <-      c("housing", "homes","home", "house","heating","homeowner","housingcrisis",
                           "carbonmonoxide",
                           "property", "planning",
                           "landlord",
                           "renting",
                           "fuelpoverty",
                           "ukhousing")

#check for dupliates
#shoudl all be false
summary(duplicated(c(keywords.construction,keywords.eco,keywords.housing,keywords.retrofit)))


#Function to ananlysie a tweet
scan.tweet <- function(x){
  #remove URLS and certain punctuation and convert to lowercase
  x <- gsub("http[^[:space:]]*", "", x)
  x <- gsub(paste0("[^", paste(c("#", "@",letters,LETTERS," ",c(0:9)), collapse=""), "]+"), "", x)
  
  #split into words
  y <- stringr::str_split(x," ")[[1]]
  
  result <- list()
  # Search for @
  ats <- grep(x = y, pattern = "@", value = T)
  if(length(ats) == 0){ats <- NA}
  # search for #
  hashtags <- grep(x = y, pattern = "#", value = T)
  if(length(hashtags) == 0){hashtags <- NA}
  #now remove hashtags
  
  y = gsub("#","",y)
  y <- chartr("A-Z", "a-z",y)
  #search for keywords
  result <- data.frame(retrofit = sum(y %in% keywords.retrofit), 
                            eco = sum(y %in% keywords.eco), 
                            construction = sum(y %in% keywords.construction), 
                            housing = sum(y %in% keywords.housing),
                            ats = NA,
                            hashtags = NA)
  
  #result$ats <- NA
  #result$hashtags <- NA
  class(result$ats) <- "list"
  class(result$hashtags) <- "list"
  
  result$ats[1] <- list(ats)
  result$hashtags[1] <- list(hashtags)
  
  return(result)
}


#tweets.summary <- lapply(tweets$text, scan.tweet)
##########################################################
#Parallel
start <- Sys.time()
fun <- function(cl){
  parLapply(cl, tweets$text ,scan.tweet)
}
cl <- makeCluster(6) #make clusert and set number of cores
clusterExport(cl=cl, varlist=c("keywords.retrofit", "keywords.eco","keywords.construction","keywords.housing"))
#clusterExport(cl=cl, c('download.region') )
clusterEvalQ(cl, {library(stringr)})
tweets.summary <- fun(cl)
stopCluster(cl)
end <- Sys.time()
message(paste0("Finished scanning ",nrow(tweets)," tweets in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
rm(cl,fun, start, end)
##########################################################
tweets.summary <- bind_rows(tweets.summary)

saveRDS(tweets.summary,"../twitter_data/all/tweets_keywordsearch.Rds")

tweets.summary <- readRDS("../twitter_data/all/tweets_keywordsearch.Rds")
tweets.summary$all.keywords <- tweets.summary$retrofit + tweets.summary$eco + tweets.summary$construction + tweets.summary$housing

tweets <- cbind.data.frame(tweets, tweets.summary)

summary(tweets.summary$all.keywords)


#Look at if the keywords are good matches for the manual classification
# the seem to be

accounts.keywords <- tweets[,c("screenName","all.keywords","retrofit","eco","construction","housing")]
accounts.keywords <- accounts.keywords %>%
                      group_by(screenName) %>%
                      summarise(all.keywords = sum(all.keywords),
                                retrofit = sum(retrofit),
                                eco = sum(eco),
                                construction = sum(construction),
                                housing = sum(housing),
                                tweetscount = length(screenName))

saveRDS(accounts.keywords,"../twitter_data/all/accounts_keywords.Rds")

# Load in the categories
common <- readxl::read_xlsx("../twitter_data/accounts_fulldata_trim.xlsx", sheet =  "accounts_fulldata")
common <- common[,c("name","Account Category")]
common <- common[!is.na(common$`Account Category`),]

accounts.keywords <- left_join(accounts.keywords, common, by = c("screenName" = "name"))
names(accounts.keywords) <- c("screenName","all.keywords","retrofit","eco","construction","housing","tweetscount","AccountCategory")

accounts.keywords$all.keywords.rate = accounts.keywords$all.keywords / accounts.keywords$tweetscount
accounts.keywords$retrofit.rate = accounts.keywords$retrofit / accounts.keywords$tweetscount
accounts.keywords$eco.rate = accounts.keywords$eco / accounts.keywords$tweetscount
accounts.keywords$construction.rate = accounts.keywords$construction / accounts.keywords$tweetscount
accounts.keywords$housing.rate = accounts.keywords$housing / accounts.keywords$tweetscount


accounts.keywords.summary <- accounts.keywords %>%
                              group_by(AccountCategory) %>%
                              summarise(all.keywords = median(all.keywords),
                                        retrofit = median(retrofit.rate),
                                        eco = median(eco.rate),
                                        construction = median(construction.rate),
                                        housing = median(housing.rate),
                                        keywordrate = median(all.keywords.rate))
#Top Hashtags
top.hashtags <- as.data.frame(table(unlist(tweets.summary$hashtags)))

#Get a list of accounts to include
accounts.tokeep  <- accounts.keywords$screenName[accounts.keywords$all.keywords.rate > 0.1]


tweets.tokeep <- tweets[tweets$screenName %in% accounts.tokeep,] 
likes <- likes[likes$screenName %in% accounts.tokeep,]
likes <- likes[,c("favOf","screenName")]
likes$type <- "likes"
names(likes) <- c("from","to","type")

friends <- friends[friends$friendof %in% accounts.tokeep,]
friends <- friends[,c("friendof","screenName")]
friends$type <- "friends"
names(friends) <- c("from","to","type")

retweets <- tweets.tokeep[tweets$isRetweet,]
retweets$retweetOf <- sapply(retweets$ats, function(x){return(sub("@","",x[[1]]))})
retweets$retweetOf <- as.character(retweets$retweetOf)
retweets <- retweets[,c("screenName","retweetOf")]
retweets$type <- "retweets"
names(retweets) <- c("from","to","type")

#Construct the mentions dataframe
mentions <- tweets.tokeep[lengths(tweets.tokeep$ats) > 1,]
mentions <- mentions[,c("screenName","ats","isRetweet")]

mentions.list <- list()

get.mentions <- function(x){
  #message(x)
  line <- mentions[x,]
  accs <- line$ats[[1]]
  acclen <- length(accs)
  if(line$isRetweet[1]){
    #Retweet so ignore the first name
    if(acclen == 1){
      # Retweet only return nothing
      result <- NA
    }else{
      # Return all but the first
      result <- line[rep(1,acclen - 1),]
      result$ats <- accs[seq(from = 2, to = acclen )]
      result$isRetweet <- NULL
      names(result) <- c("from","to")
      result$to = gsub("@","",result$to)
    }
    
  }else{
    #not retweet so get all the names
    result <- line[rep(1,acclen),]
    result$ats <- accs[seq(from = 1, to = acclen )]
    result$isRetweet <- NULL
    names(result) <- c("from","to")
    result$to = gsub("@","",result$to)
  }
  return(result)
}

mentions.list <- lapply(1:nrow(mentions), get.mentions)
mentions.list <- bind_rows(mentions.list)
mentions.list$type <- "mentions"
names(mentions.list) <- c("from","to","type")

conn.all <- bind_rows(friends, likes, retweets, mentions.list)

conn.simple <- conn.all %>%
                group_by(from,to,type) %>%
                summarise(n = n())

conn.simple <- spread(conn.simple, key = type, value = n )

conn.simple$friends[is.na(conn.simple$friends)] <- 0
conn.simple$likes[is.na(conn.simple$likes)] <- 0
conn.simple$mentions[is.na(conn.simple$mentions)] <- 0
conn.simple$retweets[is.na(conn.simple$retweets)] <- 0

conn.simple$weight <- conn.simple$friends + conn.simple$likes + conn.simple$mentions + conn.simple$retweets

summary(is.na(conn.simple$from))
summary(is.na(conn.simple$to))

conn.simple <- conn.simple[!is.na(conn.simple$from) & !is.na(conn.simple$to),]
#conn.simple$friends <- as.character(conn.simple$friends)
conn.simple <- as.data.frame(conn.simple)

saveRDS(conn.simple,"../twitter_data/all/conn_simple_types_textanal.Rds")
rm(conn.all,tweets, mentions, friends, likes, retweets, tweets.tokeep, tweets.summary, top.hashtags, mentions.list)

