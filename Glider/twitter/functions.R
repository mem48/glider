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
      message(paste0("Got ",nrow(friends.df)," out of ",user$friendsCount, " friends for ",id," at a rate of ",round(nrow(friends.df)/as.numeric(difftime(end.time,start.time,units = "secs")),2)," users/second ",Sys.time()))
      return(friends.df)
    }
  }
}

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
