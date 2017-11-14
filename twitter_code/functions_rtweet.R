library(parallel)

#Function to check and wait if api limit has/will be exceeded
rate.wait <- function(user, type = c("Friends","Followers","Tweets","Favorites")){
  
  type.fixed <- paste0(type, paste(rep(" ",9 - nchar(type)), collapse = '')) # Put Spaces on the end of the type for the log
  #Calculate the number of requests
  if(type == "Friends"){
    nreq <- ceiling(user$friendsCount / 5000)
    resource <- "/friends/ids"
  }else if(type == "Followers"){
    nreq <- ceiling(user$followersCount / 5000)
    resource <- "/followers/ids"
  }else if(type == "Tweets"){
    nreq <- 1 #Can only make one timeline request
    resource <- "/statuses/user_timeline"
  }else if(type == "Favorites"){
    nreq <- ceiling(user$favoritesCount / 200)
    resource <- "/favorites/list"
  }else{
    message(paste0("Unknown Type of wait request, ",type,", in function rate.wait" ))
    stop()
  }
  
  #Check for number of requests
  limit <- getCurRateLimitInfo()
  limit <- limit[limit$resource == resource,]
  lim <- as.numeric(limit$limit[1])
  rem <- as.numeric(limit$remaining[1])
  
  # Calcualte how long to wait if required
  if(nreq <= rem){
    #We have enough requests
  }else if((nreq > rem) & (nreq <= lim)){
    #We don't have enough now so we must wait
    wait <- limit$reset[1] - Sys.time()
    wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
    if(wait < 0){wait <- 1} #Remove -ve waiting times
    message(paste0(Sys.time()," ",type.fixed,": Waiting for ",round(wait/60,1)," minutes"))
    #message(paste0(Sys.time()," ",type,": Waiting for ",wait," sec"))
    Sys.sleep(wait)
    
  }else if(nreq > lim){
    #We will never have enough
    message(paste0(Sys.time()," ",type,": Request of ",nreq," exceeds the maximum of ",lim,". Making an attemp"))
    #Waiting will increase total number of requests we can make so wait
    message("Waiting will increase the number of results returned")
    wait <- limit$reset[1] - Sys.time()
    wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
    if(wait < 0){wait <- 1} #Remove -ve waiting times
    message(paste0(Sys.time()," ",type.fixed,": Waiting for ",round(wait/60,1)," minutes"))
    Sys.sleep(wait)
  }else{
    #Some error has occured
    warning(paste0(Sys.time()," ",type,":Unknown number of requests has occured: nreq: ",nreq," reamining: ",rem," limit: ",lim))
    stop()
  }
  
  
}


###########################################################################################################

get.data <- function(x, type = c("Friends","Followers","Tweets","Favorites")){
  
  start.time <- Sys.time()
  type.fixed <- paste0(type, paste(rep(" ",9 - nchar(type)), collapse = '')) # Put Spaces on the end of the type for the log
  # Check if given a id or a user objects
  if(class(x)[1] == "user"){
    user <- x
    id <- x$screenName
  }else if(class(x)[1] == "character"){
    id <- x
    user <- getUser(id)
  }else{
    message(paste0(Sys.time()," ",type.fixed,": Unknown type of input ",class(x)[1], " in function get.friends"))
    stop()
  }
  
  #Check if we can do this accounts
  if(type == "Friends"){
    proceed <- (user$friendsCount == 0)
  }else if(type == "Followers"){
    proceed <- (user$followersCount == 0)
  }else if(type == "Tweets"){
    proceed <- (user$statusesCount == 0)
  }else if(type == "Favorites"){
    proceed <- (user$favoritesCount == 0)
  }else{
    message(paste0(Sys.time()," Unknown type: ",type,", in function get.data"))
    stop()
  }
  
  
  
  #Check if account is protected as can't get data on prtected accounts
  if(user$protected | proceed){
    message(paste0(Sys.time()," ",type.fixed,": User ",id," has a protected account or no ",type ,"s to collect so skipping"))
  }else{
    #Wait Check
    if(type != "Favorites"){
      rate.wait(user = user, type = type)
    }
    
    #Go and get the data
    if(type == "Friends"){
      friends <- try(user$getFriends())
      if(class(favs) == "try-error"){
        friends <- NULL
        message(paste0(Sys.time()," Friends: Unable to find account ",user$screenName," moving to next"))
      }else{
        data.df <- do.call("rbind", lapply(friends, as.data.frame))
        data.df$friendof <- id
        data.df$followerof <- NA
      }
      data.total <- user$friendsCount
    }else if(type == "Followers"){
      follower <- user$getFollowers() # who followers this user
      if(class(favs) == "try-error"){
        follower <- NULL
        message(paste0(Sys.time()," Followers: Unable to find account ",user$screenName," moving to next"))
      }else{
        data.df <- do.call("rbind", lapply(follower, as.data.frame))
        data.df$friendof <- NA
        data.df$followerof <- id
      }
      data.total <- user$followersCount
    }else if(type == "Tweets"){
      tweets <- userTimeline(id, n = 3200, includeRts=TRUE, excludeReplies=FALSE)
      data.df <- twListToDF(tweets)
      data.df <- data.df[!duplicated(data.df$id),]
      tweets <- userTimeline(id, n = 3200, includeRts=TRUE, excludeReplies=FALSE)
      if(class(favs) == "try-error"){
        tweets <- NULL
        message(paste0(Sys.time()," Tweets: Unable to find account ",user$screenName," moving to next"))
      }else{
        tweets <- userTimeline(id, n = 3200, includeRts=TRUE, excludeReplies=FALSE)
        data.df <- twListToDF(tweets)
        data.df <- data.df[!duplicated(data.df$id),]
      }
      data.total <- user$statusesCount
    }else if(type == "Favorites"){
      #Favorites Collection is different as have to iterate the requests in batches of 200
      data.df <- get.data.favorites(user)
      data.total <- user$favoritesCount
    }else{
      message(paste0(Sys.time()," Unknown type: ",type,", in function get.data"))
      stop()
    }
    
    
    
    #Return Results
    end.time <- Sys.time()
    message(paste0(Sys.time()," ",type.fixed,": ",round(nrow(data.df)/data.total*100,1),"% at ",round(nrow(data.df)/as.numeric(difftime(end.time,start.time,units = "secs")),0) ,"/sec for ",id))
    return(data.df)

  }
}





############################################################################################################

get.data.favorites <- function(user){
  #Get all the favorites from an account, only to be called from within get.data
  start.time <- Sys.time()
 
  #Check for number of requests
  # Favorites can onyl be requested in batches of 200 so wiating process is different
  nreq <- ceiling(user$favoritesCount / 200)
  
  favs.list <- list()
  nreq.todo <- min(c(nreq,33)) #Twitter never seems to return more than 3300 favorites, so capping removes an unnecessary API call
  #Get the data
  for(a in seq(from = 1, to = nreq.todo)){
    #Check remaining allowance
    limit <- getCurRateLimitInfo()
    limit <- limit[limit$resource == "/favorites/list",]
    rem <- as.numeric(limit$remaining[1])
    #message(paste0(Sys.time()," Rem =  ",rem," Reset = ",limit$reset[1]," limit = ",as.numeric(limit$limit[1])))
    if(rem <= 0){ #Changed to zero when only making 100 requests per loop
      wait <- as.integer(as.numeric(limit$reset[1] - Sys.time(), units = "secs")) + 1 #Cal wait time and add 1 seconds
      if(wait < 0){wait <- 1} #Remove -ve waiting times
      message(paste0(Sys.time()," Favorites: Waiting for ",round(wait/60,1)," minutes due to : Used up allowance in a loop"))
      Sys.sleep(wait)
      rm(limit,rem,wait)
    }else{
      rm(limit,rem)
    }
    
    #Get Data
    #if(a == 1){
    #  favs <- user$getFavorites(n = 100) #Get the first 100 favorites # can get 200 but that used 2 requests
    #}else{
    #  favs <- user$getFavorites(n = 100, max_id = minid) #Get the next 100 favorites
    #}
    
    if(a == 1){
      minid <- NULL #Se Min ID for the first time
    }
    
    favs <- try(user$getFavorites(n = 100, max_id = minid), silent = T)
    if(class(favs) == "try-error"){
      favs <- NULL
      message(paste0(Sys.time()," Favorites: Unable to find account ",user$screenName," during loop number ",a," moving to next loop"))
    }else{
      if(length(favs) > 1 | (a == 1 & length(favs) == 1) ){ #Twitter will always return the last tweet, so 1 means run out of things to get, except if this is the first loop and their is only 1 favorite
        favs <- twListToDF(favs)
        favs.list[[a]] <- favs #add to list
        minid <- min(as.double(favs$id)) - 1
      }else{
        #message(paste0(Sys.time()," Favorites: Request ",a," returned no results, this means we have reached the limit of how far back twitter can go"))
        break
      }
      
    }
    #message(paste0(Sys.time()," Number of Favs = ",length(favs)," a = ",a))
    rm(favs)
  }
  
  #Remove any empty lists
  favs.list <- favs.list[lapply(favs.list,length)>0]
  favs.df <- do.call("rbind",favs.list)
  
  if(!is.null(favs.df)){
    #Check for duplicates
    favs.df <- favs.df[!duplicated(favs.df$id),]
    
    #Add Who is
    favs.df$favOf <- user$screenName
  }
  
  #Return Results
  end.time <- Sys.time()
  #message(paste0(Sys.time()," Favorites: ",round(nrow(favs.df)/user$favoritesCount*100,1),"%  at",round(nrow(favs.df)/as.numeric(difftime(end.time,start.time,units = "secs")),1) ," /sec for ",user$screenName))
  return(favs.df)

}

#######################################################################################################
get.users <- function(ids, output = c("data.frame","list")){
  #Get user data for a list of IDs
  #Get user object
  start.time <- Sys.time()
  #Make a list
  accounts.list <- list()
  #Remove any duplicates
  ids <- unique(ids)
  #Count the number of requests
  nreq <- length(ids)
  #Check limits - difficult as can request more uses than can request limit checks
  #check rate limit
  limit <- getCurRateLimitInfo()
  limit <- limit[limit$resource == "/users/show/:id",]
  lim <- as.numeric(limit$limit[1])
  rem <- as.numeric(limit$remaining[1])
  
  #Edge case where we start with no allowance
  if(rem == 0){
    wait <- as.integer(as.numeric(limit$reset[1] - Sys.time(), units = "secs")) + 5 #Cal wait time and add 5 seconds
    if(wait < 0){wait <- 1} #Remove -ve waiting times
    message(paste0(Sys.time()," Users: Waiting for ",round(wait/60,1)," minutes due to starting with no allowance"))
    Sys.sleep(wait)
    
    limit <- getCurRateLimitInfo()
    limit <- limit[limit$resource == "/users/show/:id",]
    lim <- as.numeric(limit$limit[1])
    rem <- as.numeric(limit$remaining[1])
  }
  
  #Make a list of how many requests can be made per loop
  loops <- list()
  if(nreq >= rem){
    loops[[1]] <- rem
  }else{
    loops[[1]] <- nreq
  }
  loops.extra <- ceiling((nreq - rem)/lim)
  if(loops.extra >= 1){
    left <- nreq - rem
    for(a in 1:loops.extra){
      if(left >= lim){
        loops[[a + 1]] <- lim
        left <- left - lim
      }else{
        loops[[a + 1]] <- left
      }
    }
  }
  loops <- unlist(loops)
  
  #Now do that many number of loops
  for(b in 1:length(loops)){
    for(c in 1:loops[b]){
      #Get the id for the main list
      if(b == 1){
        idno <- c
      }else{
        idno <- c + sum(loops[1:(b-1)])
      }
      
      user <- try(getUser(ids[idno]), silent = T)
      if(class(user) == "try-error"){
        user <- NULL
        message(paste0(Sys.time()," Users: Unable to find account ",ids[idno]," moving to next account"))
      }else{
        if(output == "data.frame"){
          user <- user$toDataFrame()
          accounts.list[[idno]] <- user
        }else if(output == "list"){
          accounts.list[[idno]] <- user
        }else{
          message("Unknown Output Type")
          stop()
        }
        
      }
      
    }
    #After doing a loop will need to wait
    #unless its the last loop
    if(b != length(loops)){
      limit <- getCurRateLimitInfo()
      limit <- limit[limit$resource == "/users/show/:id",]
      wait <- as.integer(as.numeric(limit$reset[1] - Sys.time(), units = "secs")) + 5 #Cal wait time and add 5 seconds
      if(wait < 0){wait <- 1} #Remove -ve waiting times
      message(paste0(Sys.time()," Users: Waiting for ",round(wait/60,1)," minutes"))
      Sys.sleep(wait)
    }
  }
  
  if(output == "data.frame"){
    accounts.df <- do.call("rbind",accounts.list)
    accounts.df <- accounts.df[!duplicated(accounts.df$id),] #Check for duplicates
    end.time <- Sys.time()
    message(paste0(Sys.time(), " ",nrow(accounts.df),"/",length(ids), " users  @ ",round(nrow(accounts.df)/as.numeric(difftime(end.time,start.time,units = "secs")),1) ," users/second"))
    return(accounts.df)
  }else if(output == "list"){
    #Remove any empty lists
    accounts.list <- accounts.list[lapply(accounts.list,length)>0]
    end.time <- Sys.time()
    message(paste0(Sys.time(), " ",length(accounts.list),"/",length(ids), " users  @ ",round(length(accounts.list)/as.numeric(difftime(end.time,start.time,units = "secs")),1) ," users/second"))
    return(accounts.list)
  }

}


########################################################################################################################

get.SNAdata <- function(ids,temp.fld){
  #Grand fucntion which gathers mulitple forms of data using the waiting time of one request to make other requests
  
  #Get user object
  start.time <- Sys.time()
  #Remove any duplicates
  ids <- unique(ids)
  #Count the number of requests
  nreq <- length(ids)
  
  # We are limited mosyl by the number of friends requests we can make
  # Make a list of how many requests can be made per loop
  loops <- list()
  lim = 50
  
  if(nreq >= lim){ 
    loops[[1]] <- lim
  }else{
    loops[[1]] <- nreq
  }
  loops.extra <- ceiling((nreq - lim)/lim)
  if(loops.extra >= 1){
    left <- nreq - lim
    for(a in 1:loops.extra){
      if(left >= lim){
        loops[[a + 1]] <- lim
        left <- left - lim
      }else{
        loops[[a + 1]] <- left
      }
    }
  }
  loops <- unlist(loops)
  message(paste0(Sys.time()," To get ",nreq," accounts will require ",length(loops)," batches of up to ",lim))
  
  #Make Main Lists
  accounts.list <- list()
  friends.list <- list()
  favorites.list <- list()
  tweets.list <- list()
  
  
  #Loop over the batches
  for(b in seq(from = 1, to = length(loops))){
    # Make a vector of the ids to do in this batch
    if(b == 1){
      idnos <- 1:loops[1]
    }else{
      idnos <- (sum(loops[1:(b-1)]) + 1) : (sum(loops[1:(b-1)]) + loops[b])
    }
    
    # Series of lapply fucntions for each data type
    message(paste0(Sys.time()," Doing batch ",b,": Getting account details"))
    batch.accounts <- get.users(ids[idnos], output = "list")
    
    #Convert to Data frame
    batch.accounts.list <- list()
    for(e in seq(1,length(batch.accounts))){
      sub <- batch.accounts[[e]]
      sub <- sub$toDataFrame()
      batch.accounts.list[[e]] <- sub
    }
    
    batch.accounts.df <- do.call("rbind",batch.accounts.list)
    accounts.list[[b]] <- batch.accounts.df

    #Make a cluster to run each data request sepately
    tasks <- list(
      job1 = function() lapply(batch.accounts, get.data, type = "Friends"),
      job2 = function() lapply(batch.accounts, get.data, type = "Favorites"),
      job3 = function() lapply(batch.accounts, get.data, type = "Tweets")
    )
    
    #create directory
    if(!dir.exists(paste0("twitterlog"))){
      dir.create(paste0("twitterlog"))
    }
    
    message(paste0(Sys.time()," Doing batch ",b,": Starting Cluster"))
    cl <- makeCluster( length(tasks), outfile = paste0("twitterlog/parlog-",b,"-",Sys.Date(),".txt") )
    clusterExport(cl=cl, varlist=c("batch.accounts"), envir=environment())
    clusterEvalQ(cl, {library(twitteR); source("twitter/functions.R"); source("twitter/secrets.R")})
    out <- clusterApply( 
      cl,
      tasks,
      function(f) f()
    )
    stopCluster(cl)
    
    message(paste0(Sys.time()," Doing batch ",b,": Cleaning Results"))
    #Deconstruct the list of dataframes
    
    out1 <- out[[1]] #friend
    out2 <- out[[2]] #favorite
    out3 <- out[[3]] #tweets
    
    #Remove any null results from the list
    out1 <- out1[lapply(out1,length)>0] #friend
    out2 <- out2[lapply(out2,length)>0] #favorite
    out3 <- out3[lapply(out3,length)>0] #tweets
    
    friends.list[[b]] <- do.call("rbind",out1) 
    favorites.list[[b]] <- do.call("rbind",out2) 
    tweets.list[[b]] <- do.call("rbind",out3) 
    rm(out,out1,out2,out3)
    
    if(!is.null(temp.fld)){
      saveRDS(friends.list,paste0(temp.fld,"/FriendsList-",Sys.Date(),".Rds"))
      saveRDS(favorites.list,paste0(temp.fld,"/FavoritesList-",Sys.Date(),".Rds"))
      saveRDS(tweets.list,paste0(temp.fld,"/TweetsList-",Sys.Date(),".Rds"))
      saveRDS(accounts.list,paste0(temp.fld,"/AccountsList-",Sys.Date(),".Rds"))
    }
  
  } # End of Outer Loop
  accounts.list
  #Combine the master lists
  accounts <- do.call("rbind",accounts.list)
  friends <- do.call("rbind",friends.list)
  favorites <- do.call("rbind",favorites.list)
  tweets <- do.call("rbind",tweets.list)
  
  #Put data frames into a big list to return to user
  result <- list(accounts = accounts, friends = friends, favorites = favorites, tweets = tweets)
  end.time <- Sys.time()
  message(paste0(Sys.time(), " data gathered for ",length(ids), " users "))
  return(result)
}
