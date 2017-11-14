# Mare differetn starting cores

cores <- read.csv("twitter/accounts_cores.csv", stringsAsFactors = F)
c1 <- cores$Twitter[cores$Core_Supply_chain == 1]
c2 <- cores$Twitter[cores$Core_Green == 1]
c3 <- cores$Twitter[cores$Core_govt == 1]
c4 <- cores$Twitter[cores$Core_training == 1]
c5 <- cores$Twitter[cores$Core_standards == 1]
c6 <- cores$Twitter[cores$Core_social_housing == 1]

#Which accounts are not in any core
cores$Twitter[!cores$Twitter %in% unique(c(c1,c2,c3,c4,c5,c6))]



# Get firs round connections
conn.simple <- readRDS("twitter/data/parReRun/FirstRoundConnections.Rds")

conn.c1 <- conn.simple[conn.simple$from %in% c1,]
conn.c2 <- conn.simple[conn.simple$from %in% c2,]
conn.c3 <- conn.simple[conn.simple$from %in% c3,]
conn.c4 <- conn.simple[conn.simple$from %in% c4,]
conn.c5 <- conn.simple[conn.simple$from %in% c5,]
conn.c6 <- conn.simple[conn.simple$from %in% c6,]

#get the size of the networks
length(unique(conn.c1$to))
length(unique(conn.c2$to))
length(unique(conn.c3$to))
length(unique(conn.c4$to))
length(unique(conn.c5$to))
length(unique(conn.c6$to))




#summarise the new connections
new.acc <- data.frame(id = unique(conn.simple$to), ncores = 0)

for(a in 1:nrow(new.acc)){
  name <- new.acc$id[a]
  new.acc$ncores[a] <-  sum((name %in% conn.c1$to),
                            (name %in% conn.c2$to),
                            (name %in% conn.c3$to),
                            (name %in% conn.c4$to),
                            (name %in% conn.c5$to),
                            (name %in% conn.c6$to) )
  
  
}


hist(new.acc$ncores)
summary(as.factor(new.acc$ncores))

core.cores <- new.acc[new.acc$id %in% cores$Twitter,]
hist(core.cores$ncores, breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5))
summary(as.factor(core.cores$ncores))


