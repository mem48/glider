#New Analysys Mithod post 9th April Workshop

library(dplyr)
library(stringr)
library(parallel)
library(tidyr)

# main stages

# Extract a wider range of keywords and test the thresholds ofr inclusion
# make clusters based on friends only
# use likes, retweets, metnions to investigate the strucutre of communications

#############################
# step 1 keywords of intrest

old.clustersummary <- readRDS("../twitter_data/cluster_summaries.Rds")
old.keywords <- list()
for(i in 1:length(old.clustersummary)){
  old.keywords[[i]] <- old.clustersummary[[i]][[2]]
}
old.keywords <- unlist(old.keywords)
old.keywords <- data.frame(keyName=names(old.keywords), value=old.keywords, row.names=NULL)

old.keywords <- old.keywords %>%
                group_by(keyName) %>%
                summarise( count = sum(value))

#export to CSV
write.csv(old.keywords,"../twitter_data/keywordsummary.csv", row.names = F)
rm(old.keywords,i,old.clustersummary)

keywords <- read.csv("../twitter_data/keywordsummary_tagged.csv", stringsAsFactors = F)
keywords <- keywords[!keywords$group == "",]

keywords.simple <- keywords %>% group_by(group) %>% summarise(val = list(keyName))
rm(keywords)

tweets <- readRDS("../twitter_data/all/tweets.Rds")

tweets <- tweets[,c("text")]



#Function to ananlysie a tweet
count.keywords <- function(i,y){
  keywords.count = sum(y %in% keywords.simple$val[i][[1]])
  return(keywords.count)
}


#Function to ananlysie a tweet
scan.tweet <- function(x, keywords.number){
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
  #keywords.number <- nrow(keywords.simple)
  
  counts <- matrix(data = sapply(1:keywords.number, count.keywords, y = y ), nrow = 1, ncol = keywords.number)
  
  result <- list(ats,hashtags,counts)
  
  return(result)
}

#tweets.summary <- lapply(tweets[1:1000], scan.tweet, keywords.number = nrow(keywords.simple))

##########################################################
#Parallel
start <- Sys.time()
fun <- function(cl){
  parLapply(cl, tweets ,scan.tweet, keywords.number = nrow(keywords.simple))
}
cl <- makeCluster(7) #make clusert and set number of cores
clusterExport(cl=cl, varlist=c("keywords.simple"))
clusterExport(cl=cl, c('count.keywords') )
clusterEvalQ(cl, {library(stringr)})
tweets.summary <- fun(cl)
stopCluster(cl)
end <- Sys.time()
message(paste0("Finished scanning ",nrow(tweets)," tweets in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
rm(cl,fun, start, end)
##########################################################
#rm(tweets)
#tweets.summary <- bind_rows(tweets.summary)

#create data frame
ats.list <- sapply(tweets.summary,function(x) x[1])
hashtags.list <- sapply(tweets.summary,function(x) x[2])
counts.list <- sapply(tweets.summary,function(x) x[3])
counts.list <- do.call("rbind",counts.list)
counts.total <- rowSums(counts.list)
counts.list <- as.data.frame(counts.list)
names(counts.list) <- keywords.simple$group

tweets.summary.df <- data.frame(id = 1:length(tweets.summary), ats = NA, hashtags = NA)
tweets.summary.df$ats <- ats.list
tweets.summary.df$hashtags <- hashtags.list

tweets.summary.df <- bind_cols(tweets.summary.df, counts.list)
tweets.summary.df$keywords.all <- counts.total
tweets.summary.df$id <- NULL


saveRDS(tweets.summary.df,"../twitter_data/all/tweets_keywordsearch_advanced.Rds")
#tweets.summary.old <- readRDS("../twitter_data/all/tweets_keywordsearch.Rds")

tweets.summary.df$screenName <- tweets$screenName
names(tweets.summary.df) <- sub(" ","",names(tweets.summary.df))
names(tweets.summary.df) <- sub(" ","",names(tweets.summary.df))
accounts.summary <- tweets.summary.df %>%
                    group_by(screenName) %>%
                    summarise(affordable = sum(affordable),
                              architect = sum(architect),
                              bed = sum(bed),
                              bim = sum(bim),             
                              build = sum(build),
                              carbonandclimate = sum(carbonandclimate),
                              CIBSE = sum(CIBSE),
                              CIH = sum(CIH),
                              construction = sum(construction),
                              design   = sum(design),        
                              doorsandwindows = sum(doorsandwindows),
                              eco = sum(eco),
                              economy = sum(economy),
                              efficiency = sum(efficiency),
                              electric = sum(electric),
                              energy   = sum(energy),        
                               engineering  = sum(engineering),
                               environment = sum(environment),
                               flood = sum(flood),
                               floor = sum(floor),
                               fuel = sum(fuel),
                               gas = sum(gas),             
                              green = sum(green),
                              heating = sum(heating),
                              homeless = sum(homeless),
                              house = sum(house),
                              infrastructure = sum(infrastructure),
                              installer  = sum(installer),      
                              insulation = sum(insulation),
                              JRF = sum(JRF),
                              land = sum(land),
                              landlord = sum(landlord),
                              natfed = sum(natfed),
                              neighbourhood   = sum(neighbourhood), 
                               passivehouse = sum(passivehouse),
                               plumbing = sum(plumbing),
                               poverty = sum(poverty),
                               property = sum(property),
                               renewables = sum(renewables),
                               rent= sum(rent),       
                               residential = sum(residential),
                               retrofit = sum(retrofit),
                               RIBA = sum(RIBA),
                               RICS = sum(RICS),
                               roof = sum(roof),
                               skills  = sum(skills),         
                              smart = sum(smart),
                              surveying = sum(surveying),
                              sustainability = sum(sustainability),
                              taxandbenefits = sum(taxandbenefits),
                              timber = sum(timber),
                              trades  = sum(trades),         
                               keywords.all = sum(keywords.all),
                             tweetscount = length(screenName))


#change counts to rates
for(i in 2:54){
  accounts.summary[,i] <- accounts.summary[,i] / accounts.summary$tweetscount
}


saveRDS(accounts.summary,"../twitter_data/all/accounts_keywords_advanced.Rds")

########

# make cumulative plot of kewywords
accounts.summary <- accounts.summary[order(accounts.summary$keywords.all),]
accounts.summary$keywords.count <- accounts.summary$keywords.all * accounts.summary$tweetscount
keyword.cumsum <- cumsum(accounts.summary$keywords.count)
plot(keyword.cumsum)



accounts.tokeep  <- accounts.summary$screenName[accounts.summary$keywords.all > 0.1]

friends <- readRDS("../twitter_data/all/friends")
likes <- readRDS("../twitter_data/all/likes")
tweets <- readRDS("../twitter_data/all/tweets.Rds")

tweets <- tweets[,c("text","favorited","favoriteCount", "replyToSN","created","truncated","replyToSID","id",
                    "replyToUID","screenName","retweetCount","isRetweet","retweeted")]
likes <- likes[,c("text","favorited","favoriteCount", "replyToSN","created","truncated","replyToSID","id",
                  "replyToUID","screenName","retweetCount","isRetweet","retweeted","favOf")]

tweets$ats <- tweets.summary.df$ats
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

saveRDS(conn.simple,"../twitter_data/all/conn_simple_advanced.Rds")
