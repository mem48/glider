library(parallel)


get.friends <- function(id){
  start.time <- Sys.time()
  #Get a list of friends
  #Get user object
  user <- getUser(id)
  
  #Check if account is protected as can't get data on prtected accounts
  if(user$protected){
    message(paste0("User ",id," has a protected account, so skipping"))
  }else{
    #CHeck for users with no friends
    if(user$friendsCount == 0){
      message(paste0("User ",id," has no friends, so skipping"))
    }else{
      #Check for number of requests
      nreq <- ceiling(user$friendsCount / 5000)
      limit <- getCurRateLimitInfo()
      limit <- limit[limit$resource == "/friends/ids",]
      lim <- as.numeric(limit$limit[1])
      rem <- as.numeric(limit$remaining[1])
      
      #Do we have enough requests
      if(nreq <= rem){
        #We have enough requests
      }else if((nreq > rem) & (nreq <= lim)){
        #We don't have enough now so we must wait
        wait <- limit$reset[1] - Sys.time()
        wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
        if(wait < 0){wait <- 1} #Remove -ve waiting times
        message(paste0("Waiting for ",wait," seconds at ",Sys.time()))
        Sys.sleep(wait)
        
      }else if(nreq > lim){
        #We will never have enough
        message(paste0("Request of ",nreq," exceeds the maximum of ",lim,". Making an attemp at ",Sys.time()))
        #Waiting will increase total number of requests we can make so wait
        message("Waiting will increase the number of results returned")
        wait <- limit$reset[1] - Sys.time()
        wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
        if(wait < 0){wait <- 1} #Remove -ve waiting times
        message(paste0("Waiting for ",wait," seconds at ",Sys.time()))
        Sys.sleep(wait)
      }else{
        #Some error has occured
        warning(paste0("Unknown number of requests has occured: nreq: ",nreq," reamining: ",rem," limit: ",lim," at ",Sys.time()))
        stop()
      }
      
      #Go and get the data
      friends <- user$getFriends() # who this user follows
      friends.df <- do.call("rbind", lapply(friends, as.data.frame))
      friends.df$friendof <- id
      friends.df$followerof <- NA
      
      #Return Results
      end.time <- Sys.time()
      #message(paste0("Got ",nrow(friends.df)," out of ",user$friendsCount, " friends for ",id," at a rate of ",round(nrow(friends.df)/as.numeric(difftime(end.time,start.time,units = "secs")),2)," users/second ",Sys.time()))
      message(paste0(Sys.time()," ",nrow(friends.df),"/",user$friendsCount, " friends @ ",round(nrow(friends.df)/as.numeric(difftime(end.time,start.time,units = "secs")),1)," users/second for ",id))
      return(friends.df)
    }
  }
}

#########################################################################################################
# same as get.friends without the waiting checks
get.friends.nowait <- function(id){
  start.time <- Sys.time()
  #Get a list of friends
  #Get user object
  user <- getUser(id)
  
  #Check if account is protected as can't get data on prtected accounts
  if(user$protected){
    message(paste0("User ",id," has a protected account, so skipping"))
  }else{
    #CHeck for users with no friends
    if(user$friendsCount == 0){
      message(paste0("User ",id," has no friends, so skipping"))
    }else{
      
      #Go and get the data
      friends <- user$getFriends() # who this user follows
      friends.df <- do.call("rbind", lapply(friends, as.data.frame))
      friends.df$friendof <- id
      friends.df$followerof <- NA
      
      #Return Results
      end.time <- Sys.time()
      message(paste0("Got ",nrow(friends.df)," out of ",user$friendsCount, " friends for ",id," at a rate of ",round(nrow(friends.df)/as.numeric(difftime(end.time,start.time,units = "secs")),2)," users/second ",Sys.time()))
      return(friends.df)
    }
  }
}



##########################################################################################################

get.followers <- function(id){
  #Get a list of friends
  #Get user object
  start.time <- Sys.time()
  user <- getUser(id)
  if(user$protected){
    message(paste0("User ",id," as a protected account, so skipping"))
  }else{
    #CHeck for users with no followers
    if(user$followersCount == 0){
      message(paste0("User ",id," has no followers, so skipping"))
    }else{
      #Check for number of requests
      nreq <- ceiling(user$followersCount / 5000)
      limit <- getCurRateLimitInfo()
      limit <- limit[limit$resource == "/followers/ids",]
      lim <- as.numeric(limit$limit[1])
      rem <- as.numeric(limit$remaining[1])
      
      #Do we have enough requests
      if(nreq <= rem){
        #We have enough requests
      }else if((nreq > rem) & (nreq <= lim)){
        #We don't have enough now so we must wait
        wait <- limit$reset[1] - Sys.time()
        wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
        if(wait < 0){wait <- 1} #Remove -ve waiting times
        message(paste0("Waiting for ",wait," seconds at ",Sys.time()))
        Sys.sleep(wait)
        
      }else if(nreq > lim){
        #We will never have enough
        message(paste0("Request of ",nreq," exceeds the maximum of ",lim,". Making an attemp at ",Sys.time()))
        #Waiting will increase total number of requests we can make so wait
        message("Waiting will increase the number of results returned")
        wait <- limit$reset[1] - Sys.time()
        wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
        if(wait < 0){wait <- 1} #Remove -ve waiting times
        message(paste0("Waiting for ",wait," seconds at ",Sys.time()))
        Sys.sleep(wait)
      }else{
        #Some error has occured
        warning(paste0("Unknown number of requests has occured: nreq: ",nreq," reamining: ",rem," limit: ",lim," at ",Sys.time()))
        stop()
      }
      
      #Go and get the data
      follower <- user$getFollowers() # who this user follows
      follower.df <- do.call("rbind", lapply(follower, as.data.frame))
      follower.df$friendof <- NA
      follower.df$followerof <- id
      
      #Return Results
      end.time <- Sys.time()
      message(paste0("Got ",nrow(follower.df)," out of ",user$followersCount, " followers for ",id," at a rate of ",round(nrow(follower.df)/as.numeric(difftime(end.time,start.time,units = "secs")),2) ," users/second at ",Sys.time()))
      return(follower.df)
    }
  }
}

#######################################################################################################
get.favorites <- function(id){
  #Get all the favorites from an account
  #Get user object
  start.time <- Sys.time()
  user <- getUser(id)
  if(user$protected){
    message(paste0("User ",id," as a protected account, so skipping"))
  }else{
    #Check for users with no favorites
    if(user$favoritesCount == 0){
      message(paste0("User ",id," has no followers, so skipping"))
    }else{
      #Check for number of requests
      nreq <- ceiling(user$favoritesCount / 200)
      #message(paste0("Doing user ",id,"'s ",user$favoritesCount," favorites in ",nreq," requests. Aprrox finish time is ",Sys.time() + (60 * nreq)))
      #Check remaining allowance
      limit <- getCurRateLimitInfo()
      limit <- limit[limit$resource == "/favorites/list",]
      rem <- as.numeric(limit$remaining[1])
      if(rem <= 1){ #Changed from 0 to 1 as can use 2 requests to get data
        wait <- as.integer(as.numeric(limit$reset[1] - Sys.time(), units = "secs")) + 5 #Cal wait time and add 5 seconds
        if(wait < 0){wait <- 1} #Remove -ve waiting times
        message(paste0("Waiting for ",round(wait/60,1)," minutes at ",Sys.time()," due to: remaining allowance <= 1"))
        Sys.sleep(wait)
        rm(limit,rem,wait)
      }else{
        #message(paste0("Rem =  ",rem))
        rm(limit,rem)
      }
      
      #Get the data
      if(nreq == 1){
        #Simple case of only one request
        favs <- user$getFavorites(n = 200)
        favs.df <- twListToDF(favs)
      }else if(nreq > 1){
        #batch case of multiple requests
        #Go and get the first batch of data
        favs <- user$getFavorites(n = 200)
        favs <- twListToDF(favs)
        favs.list <- list()
        favs.list[[1]] <- favs
        minid <- min(as.double(favs$id)) - 1 #Get the fist status in the batch and then remove 1
        rm(favs)
        
        for(a in 2:nreq){
          #Check remaining allowance
          limit <- getCurRateLimitInfo()
          limit <- limit[limit$resource == "/favorites/list",]
          rem <- as.numeric(limit$remaining[1])
          if(rem <= 1){
            wait <- as.integer(as.numeric(limit$reset[1] - Sys.time(), units = "secs")) + 5 #Cal wait time and add 5 seconds
            if(wait < 0){wait <- 1} #Remove -ve waiting times
            message(paste0("Waiting for ",round(wait/60,1)," minutes at ",Sys.time()," due to : Used up allowance in a loop"))
            Sys.sleep(wait)
            rm(limit,rem,wait)
          }else{
            #message(paste0("Rem =  ",rem))
            rm(limit,rem)
          }
          favs <- user$getFavorites(n = 200, max_id = minid) #Get the next 200 favorites
          if(length(favs) > 0){
            favs <- twListToDF(favs)
            favs.list[[a]] <- favs #add to list
            minid <- min(as.double(favs$id)) - 1
          }else{
            message(paste0("Request ",a," returned no results, this means we have reached the limit of how far back twitter can go"))
            break
          }
          rm(favs)
        }
        favs.df <- do.call("rbind",favs.list)
      }else{
        warning(paste0("Gosh! Unknown number of requests: ",nreq))
        stop()
      }
      
      #Check for duplicates
      favs.df <- favs.df[!duplicated(favs.df$id),]
      
      #Add Who is
      favs.df$favOf <- id
      
      #Return Results
      end.time <- Sys.time()
      #message(paste0("Got ",nrow(favs.df)," out of ",user$favoritesCount, " favorities for ",id," at a rate of ",round(nrow(favs.df)/as.numeric(difftime(end.time,start.time,units = "secs")),2) ," favorites/second at ",Sys.time()))
      message(paste0(Sys.time()," ",nrow(favs.df),"/",user$favoritesCount, " favorities @ ",round(nrow(favs.df)/as.numeric(difftime(end.time,start.time,units = "secs")),1)," favorites/second for ",id))
      return(favs.df)
    }
  }
}

#######################################################################################################
get.favorites.nowait <- function(id){
  #Get all the favorites from an account
  #Get user object
  start.time <- Sys.time()
  user <- getUser(id)
  if(user$protected){
    message(paste0("User ",id," as a protected account, so skipping"))
  }else{
    #Check for users with no favorites
    if(user$statusesCount == 0){
      message(paste0("User ",id," has no followers, so skipping"))
    }else{
      #Check for number of requests
      nreq <- ceiling(user$favoritesCount / 200)
      #message(paste0("Doing user ",id,"'s ",user$favoritesCount," favorites in ",nreq," requests. Aprrox finish time is ",Sys.time() + (60 * nreq)))
      
      #Get the data
      if(nreq == 1){
        #Simple case of only one request
        favs <- user$getFavorites(n = 200)
        favs.df <- twListToDF(favs)
      }else if(nreq > 1){
        #batch case of multiple requests
        #Go and get the first batch of data
        favs <- user$getFavorites(n = 200)
        favs <- twListToDF(favs)
        favs.list <- list()
        favs.list[[1]] <- favs
        minid <- min(as.double(favs$id)) - 1 #Get the fist status in the batch and then remove 1
        rm(favs)
        
        for(a in 2:nreq){
          #Check remaining allowance
          limit <- getCurRateLimitInfo()
          limit <- limit[limit$resource == "/favorites/list",]
          rem <- as.numeric(limit$remaining[1])
          if(rem <= 1){
            wait <- as.integer(as.numeric(limit$reset[1] - Sys.time(), units = "secs")) + 5 #Cal wait time and add 5 seconds
            if(wait < 0){wait <- 1} #Remove -ve waiting times
            message(paste0("Waiting for ",round(wait/60,1)," minutes at ",Sys.time()))
            Sys.sleep(wait)
            rm(limit,rem,wait)
          }else{
            #message(paste0("Rem =  ",rem))
            rm(limit,rem)
          }
          favs <- user$getFavorites(n = 200, max_id = minid) #Get the next 200 favorites
          if(length(favs) > 0){
            favs <- twListToDF(favs)
            favs.list[[a]] <- favs #add to list
            minid <- min(as.double(favs$id)) - 1
          }else{
            message(paste0("Request ",a," returned no results, this means we have reached the limit of how far back twitter can go"))
            break
          }
          rm(favs)
        }
        favs.df <- do.call("rbind",favs.list)
      }else{
        warning(paste0("Gosh! Unknown number of requests: ",nreq))
        stop()
      }
      
      #Check for duplicates
      favs.df <- favs.df[!duplicated(favs.df$id),]
      
      #Add Who is
      favs.df$favOf <- id
      
      #Return Results
      end.time <- Sys.time()
      message(paste0("Got ",nrow(favs.df)," out of ",user$favoritesCount, " favorities for ",id," at a rate of ",round(nrow(favs.df)/as.numeric(difftime(end.time,start.time,units = "secs")),2) ," favorites/second at ",Sys.time()))
      return(favs.df)
    }
  }
}


###################################################################################################

get.tweets <- function(id){
  #Get all the favorites from an account
  #Get user object
  start.time <- Sys.time()
  user <- getUser(id)
  if(user$protected){
    message(paste0("User ",id," as a protected account, so skipping"))
  }else{
    #Check for users with no favorites
    if(user$statusesCount == 0){
      message(paste0("User ",id," has no tweets, so skipping"))
    }else{
      #Check for number of requests
      nreq <- 1 #Can only make one timelin request
      limit <- getCurRateLimitInfo()
      limit <- limit[limit$resource == "/statuses/user_timeline",]
      lim <- as.numeric(limit$limit[1]) / 32 #making a full request of 3200 (twitteR limit) use 32 twitter api requests
      rem <- as.numeric(limit$remaining[1]) / 32
      
      #Do we have enough requests
      if(nreq <= rem){
        #We have enough requests
      }else if((nreq > rem) & (nreq <= lim)){
        #We don't have enough now so we must wait
        wait <- limit$reset[1] - Sys.time()
        wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
        if(wait < 0){wait <- 1} #Remove -ve waiting times
        message(paste0("Waiting for ",round(wait/60,1)," minutes at ",Sys.time()))
        Sys.sleep(wait)
        
      }else if(nreq > lim){
        #We will never have enough
        message(paste0("Request of ",nreq," exceeds the maximum of ",lim,". Making an attemp at ",Sys.time()))
        #Waiting will increase total number of requests we can make so wait
        message("Waiting will increase the number of results returned")
        wait <- limit$reset[1] - Sys.time()
        wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
        if(wait < 0){wait <- 1} #Remove -ve waiting times
        message(paste0("Waiting for ",round(wait/60,1)," minutes at ",Sys.time()))
        Sys.sleep(wait)
      }else{
        #Some error has occured
        warning(paste0("Unknown number of requests has occured: nreq: ",nreq," reamining: ",rem," limit: ",lim," at ",Sys.time()))
        stop()
      }
      
      #Simple case
      tweets <- userTimeline(id, n = 3200, includeRts=TRUE, excludeReplies=FALSE)
      tweets.df <- twListToDF(tweets)
      
      #Check for duplicates
      tweets.df <- tweets.df[!duplicated(tweets.df$id),]
      
      #Return Results
      end.time <- Sys.time()
      #message(paste0("Got ",nrow(tweets.df)," out of ",user$statusesCount, " tweets for ",id," at a rate of ",round(nrow(tweets.df)/as.numeric(difftime(end.time,start.time,units = "secs")),2) ," tweets/second at ",Sys.time()))
      message(paste0(Sys.time()," ",nrow(tweets.df),"/", user$statusesCount, " tweets @ ",round(nrow(tweets.df)/as.numeric(difftime(end.time,start.time,units = "secs")),1) ," tweets/second for ",id))
      return(tweets.df)
    }
  }
}

###################################################################################################
#Same as get.tweets but with the waiting checks removed
get.tweets.nowait <- function(id){
  #Get all the favorites from an account
  #Get user object
  start.time <- Sys.time()
  user <- getUser(id)
  if(user$protected){
    message(paste0("User ",id," as a protected account, so skipping"))
  }else{
    #Check for users with no favorites
    if(user$favoritesCount == 0){
      message(paste0("User ",id," has no followers, so skipping"))
    }else{
      
      #Simple case
      tweets <- userTimeline(id, n = 3200, includeRts=TRUE, excludeReplies=FALSE)
      tweets.df <- twListToDF(tweets)
      
      #Check for duplicates
      tweets.df <- tweets.df[!duplicated(tweets.df$id),]
      
      #Return Results
      end.time <- Sys.time()
      message(paste0(Sys.time()," ",nrow(tweets.df),"/", user$statusesCount, " tweets @ ",round(nrow(tweets.df)/as.numeric(difftime(end.time,start.time,units = "secs")),1) ," tweets/second for ",id))
      return(tweets.df)
    }
  }
}


#######################################################################################################
get.users <- function(ids){
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
    message(paste0("Waiting for ",round(wait/60,1)," minutes at ",Sys.time()))
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
  message(paste0("To get ",nreq," accounts will require ",length(loops)," batches of ",lim,". Approx finish time is ",Sys.time() + (60 * 15 * length(loops))))
  #Now do that many number of loops
  for(b in 1:length(loops)){
    for(c in 1:loops[b]){
      #Get the id for the main list
      if(b == 1){
        idno <- c
      }else{
        idno <- c + sum(loops[1:(b-1)])
      }
      #message(paste0("ID = ",idno," c = ",c," b = ",b," loops = ",loops))
      #user <- getUser(ids[idno])
      user <- try(getUser(ids[idno]), silent = T)
      if(class(user) == "try-error"){
        user <- NULL
        message(paste0("Unable to find account ",ids[idno]," moving to next account at ",Sys.time()))
      }else{
        user <- user$toDataFrame()
        accounts.list[[idno]] <- user
      }
      
    }
    #After doing a loop will need to wait
    #unless its the last loop
    if(b != length(loops)){
      limit <- getCurRateLimitInfo()
      limit <- limit[limit$resource == "/users/show/:id",]
      wait <- as.integer(as.numeric(limit$reset[1] - Sys.time(), units = "secs")) + 5 #Cal wait time and add 5 seconds
      if(wait < 0){wait <- 1} #Remove -ve waiting times
      message(paste0("Waiting for ",round(wait/60,1)," minutes at ",Sys.time()))
      Sys.sleep(wait)
    }
  }
  accounts.df <- do.call("rbind",accounts.list)
  
  #Check for duplicates
  accounts.df <- accounts.df[!duplicated(accounts.df$id),]
  
  #Return Results
  end.time <- Sys.time()
  #message(paste0("Got ",nrow(accounts.df)," out of ",length(ids), " users. At a rate of ",round(nrow(accounts.df)/as.numeric(difftime(end.time,start.time,units = "secs")),2) ," users/second at ",Sys.time()))
  message(paste0(Sys.time(), " ",nrow(accounts.df),"/",length(ids), " users  @ ",round(nrow(accounts.df)/as.numeric(difftime(end.time,start.time,units = "secs")),1) ," users/second"))
  return(accounts.df)
  
}

########################################################################################################################
# variation that can be used in the get.SNAdata function
get.user.nowait <- function(id){
  
  accounts.list <- list()
  # Get user
  user <- try(getUser(id), silent = T)
    if(class(user) == "try-error"){
        user <- NULL
        message(paste0("Unable to find account ",id," moving to next account at ",Sys.time()))
    }else{
        user <- user$toDataFrame()
        accounts.list[[1]] <- user
    }
   
  accounts.df <- do.call("rbind",accounts.list)
  
  #Check for duplicates
  accounts.df <- accounts.df[!duplicated(accounts.df$id),]
  
  #Return Results
  end.time <- Sys.time()
  #message(paste0("Got ",nrow(accounts.df)," out of ",length(ids), " users. At a rate of ",round(nrow(accounts.df)/as.numeric(difftime(end.time,start.time,units = "secs")),2) ," users/second at ",Sys.time()))
  return(accounts.df)
  
}




########################################################################################################################

get.SNAdata <- function(ids){
  #Grand fucntion which gathers mulitple forms of data using the waiting time of one request to make other requests
  
  #Get user object
  start.time <- Sys.time()
  #Remove any duplicates
  ids <- unique(ids)
  #Count the number of requests
  nreq <- length(ids)
  #Check limits - difficult as can request more uses than can request limit checks
  #check rate limit
  limit <- getCurRateLimitInfo()
  limit <- limit[limit$resource %in% c("/users/show/:id","/statuses/user_timeline","/favorites/list","/friends/ids"),]
  limit$limit <- as.integer(limit$limit)
  limit$remaining <- as.integer(limit$remaining)
  limit$limit[limit$resource == "/statuses/user_timeline"] <- limit$limit[limit$resource == "/statuses/user_timeline"] / 32 #making a full request of 3200 (twitteR limit) use 32 twitter api requests
  limit$remaining[limit$resource == "/statuses/user_timeline"] <- limit$remaining[limit$resource == "/statuses/user_timeline"] / 32
  
  #Edge case where we start with no allowance
  if(any(limit$remaining == 0)){
    wait <- as.integer(as.numeric(max(limit$reset[limit$remaining == 0]) - Sys.time(), units = "secs")) + 2 #Cal wait time and add 2 seconds
    if(wait < 0){wait <- 1} #Remove -ve waiting times
    message(paste0("Waiting for ",round(wait/60,1)," minutes at ",Sys.time()))
    Sys.sleep(wait)
    
    limit <- getCurRateLimitInfo()
    limit <- limit[limit$resource %in% c("/users/show/:id","/statuses/user_timeline","/favorites/list","/friends/ids"),]
    limit$limit <- as.integer(limit$limit)
    limit$remaining <- as.integer(limit$remaining)
  }
  
  # We are limited mosyl by the number of friends requests we can make
  # Make a list of how many requests can be made per loop
  loops <- list()
  lim <- min(limit$limit) #Lowest Limit
  rem <- limit$remaining[limit$limit == lim][1] #Check number of requests agains the type of request with the lowest limit
  
  #lim = 100
  #rem = 100
  
  
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
  message(paste0("To get ",nreq," accounts will require ",length(loops)," batches of up to ",lim))
  
  #Make Main Lists
  accounts.list <- list()
  friends.list <- list()
  favorites.list <- list()
  tweets.list <- list()
  
  
  #Loop over the batches
  for(b in 1:length(loops)){
    message(paste0("Doing batch ", b," at ",Sys.time()))
    # Make a vector of the ids to do in this batch
    if(b == 1){
      idnos <- 1:loops[1]
    }else{
      idnos <- (sum(loops[1:(b-1)]) + 1) : (sum(loops[1:(b-1)]) + loops[b])
    }
    
    # Series of lapply fucntions for each data type
    message(paste0("Getting account details at ",Sys.time()))
    batch.accounts <- lapply(ids[idnos], get.user.nowait)
    batch.accounts.df <- do.call("rbind",batch.accounts)
    accounts.list[[b]] <- batch.accounts.df
    rm(batch.accounts, batch.accounts.df)
    
    #Make a cluster to run each data request sepately
    tasks <- list(
      job1 = function() lapply(ids[idnos], get.friends),
      job2 = function() lapply(ids[idnos], get.favorites),
      job3 = function() lapply(ids[idnos], get.tweets)
    )
    #stp <- Sys.time()
    #message(length(ids))
    
    #Remove old logs
    if(file.exists("parlog.txt")){
      file.remove("parlog.txt")
    }
    
    cl <- makeCluster( length(tasks), outfile = "parlog.txt" )
    clusterExport(cl=cl, varlist=c("ids", "idnos"), envir=environment())
    clusterEvalQ(cl, {library(twitteR); source("twitter/functions.R"); source("twitter/secrets.R")})
    out <- clusterApply( 
      cl,
      tasks,
      function(f) f()
    )
    stopCluster(cl)
    #ftp <- Sys.time()
    #difftime(ftp,stp)
    
    
    
    
    
    # Organise accounts to do based on number of requests
    #batch.accounts.df$reqFriend <- ceiling(batch.accounts.df$friendsCount / 5000) # Get max of 5000 friends per request
    #batch.accounts.df$reqFav <- ceiling(batch.accounts.df$favoritesCount / 200) # Get max of 200 favorites per request
    
    #sum(batch.accounts.df$reqFriend)
    #sum(batch.accounts.df$reqFav)
    #sts <- Sys.time()
    
    #message(paste0("Getting friends at ",Sys.time()))
    #batch.friends <- lapply(ids[idnos], get.friends)
    #message(paste0("Getting favorites at ",Sys.time()))
    #batch.favorites <- lapply(ids[idnos], get.favorites)
    #message(paste0("Getting tweets at ",Sys.time()))
    #batch.tweets <- lapply(ids[idnos], get.tweets)
    
    #fts <- Sys.time()
    #difftime(fts,sts)
    
    
    message(paste0("Cleaning up results at ",Sys.time()))
    #Deconstruct the list of dataframes
    friends.list[[b]] <- do.call("rbind",out[[1]]) #friend
    favorites.list[[b]] <- do.call("rbind",out[[2]]) #favorite
    tweets.list[[b]] <- do.call("rbind",out[[3]]) #tweets
    rm(out)
    
    
    
    
    
    #batch.friends.df <- do.call("rbind",batch.friends)
    #batch.favorites.df <- do.call("rbind",batch.favorites)
    #batch.tweets.df <- do.call("rbind",batch.tweets)
    #rm(batch.accounts,batch.friends,batch.favorites,batch.tweets)
    
    
    #Add data frames to the master lists
    #accounts.list[[b]] <- batch.accounts.df
    #friends.list[[b]] <- batch.friends.df
    #favorites.list[[b]] <- batch.favorites.df
    #tweets.list[[b]] <- batch.tweets.df
    #rm(batch.accounts.df,batch.friends.df,batch.favorites.df,batch.tweets.df)
    
    #After doing a loop will need to wait
    #unless its the last loop
    
    #if(b != length(loops)){
    #  limit <- getCurRateLimitInfo()
    #  limit <- limit[limit$resource %in% c("/users/show/:id","/statuses/user_timeline","/favorites/list","/friends/ids"),]
    #  limit$limit <- as.integer(limit$limit)
    #  limit$remaining <- as.integer(limit$remaining)
    #  wait <- as.integer(as.numeric(limit$reset[limit$remaining == min(limit$remaining)][1] - Sys.time(), units = "secs")) + 2 #Cal wait time and add 5 seconds
    #  if(wait < 0){wait <- 1} #Remove -ve waiting times
    #  message(paste0("Waiting for ",round(wait/60,1)," minutes at ",Sys.time()," due to : End of batch wait is required"))
    #  Sys.sleep(wait)
    #}
  } # End of Outer Loop
  
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
