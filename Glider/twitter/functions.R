get.friends <- function(id){
  #Get a list of friends
  #Get user object
  user <- getUser(id)
  
  #Check for number of requests
  nreq <- ceiling(user$friendsCount / 5000)
  limit <- getCurRateLimitInfo()
  limit <- limit[limit$resource == "/friends/ids",]
  
  #Do we have enough requests
  if(nreq <= limit$remaining[1]){
    #We have enough requests
  }else if((nreq > limit$remaining[1]) & (nreq <= limit$limit[1])){
    #We don't have enough now so we must wait
    wait <- limit$reset[1] - Sys.time()
    wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
    if(wait < 0){wait <- 1} #Remove -ve waiting times
    message(paste0("Waiting for ",wait," seconds at ",Sys.time()))
    Sys.sleep(wait)
    
  }else if(nreq > limit$limit[1]){
    #We will never have enough
    message(paste0("Request of ",nreq," exceeds the maximum of ",limit$limit[1],". Making an attemp at ",Sys.time()))
    #Waiting will increase total number of requests we can make so wait
    message("Waiting will increase the number of results returned")
    wait <- limit$reset[1] - Sys.time()
    wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
    if(wait < 0){wait <- 1} #Remove -ve waiting times
    message(paste0("Waiting for ",wait," seconds at ",Sys.time()))
    Sys.sleep(wait)
  }else{
    #Some error has occured
    warning(paste0("Unknown number of requests has occured: nreq: ",nreq," reamining: ",limit$remaining[1]," limit: ",limit$limit[1]," at ",Sys.time()))
    stop()
  }
  
  #Go and get the data
  friends <- user$getFriends() # who this user follows
  friends.df <- do.call("rbind", lapply(friends, as.data.frame))
  friends.df$friendof <- id
  friends.df$followerof <- NA
  
  #Return Results
  message(paste0("Got ",nrow(friends.df)," out of ",user$friendsCount, " friends for ",id," at ",Sys.time()))
  return(friends.df)
}

get.followers <- function(id){
  #Get a list of friends
  #Get user object
  user <- getUser(id)
  
  #Check for number of requests
  nreq <- ceiling(user$followersCount / 5000)
  limit <- getCurRateLimitInfo()
  limit <- limit[limit$resource == "/followers/ids",]
  
  #Do we have enough requests
  if(nreq <= limit$remaining[1]){
    #We have enough requests
  }else if((nreq > limit$remaining[1]) & (nreq <= limit$limit[1])){
    #We don't have enough now so we must wait
    wait <- limit$reset[1] - Sys.time()
    wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
    if(wait < 0){wait <- 1} #Remove -ve waiting times
    message(paste0("Waiting for ",wait," seconds at ",Sys.time()))
    Sys.sleep(wait)
    
  }else if(nreq > limit$limit[1]){
    #We will never have enough
    message(paste0("Request of ",nreq," exceeds the maximum of ",limit$limit[1],". Making an attemp at ",Sys.time()))
    #Waiting will increase total number of requests we can make so wait
    message("Waiting will increase the number of results returned")
    wait <- limit$reset[1] - Sys.time()
    wait <- as.integer(as.numeric(wait, units = "secs")) + 5 #Convert to number and add five seconds for safety
    if(wait < 0){wait <- 1} #Remove -ve waiting times
    message(paste0("Waiting for ",wait," seconds at ",Sys.time()))
    Sys.sleep(wait)
  }else{
    #Some error has occured
    warning(paste0("Unknown number of requests has occured: nreq: ",nreq," reamining: ",limit$remaining[1]," limit: ",limit$limit[1]," at ",Sys.time()))
    stop()
  }
  
  #Go and get the data
  follower <- user$getFollowers() # who this user follows
  follower.df <- do.call("rbind", lapply(follower, as.data.frame))
  follower.df$friendof <- NA
  follower.df$followerof <- id
  
  #Return Results
  message(paste0("Got ",nrow(follower.df)," out of ",user$followersCount, " followers for ",id," at ",Sys.time()))
  return(follower.df)
}
