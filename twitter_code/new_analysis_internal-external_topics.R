# look at the different between internal and external communications in the network
library(igraph)
library(dplyr)
library(parallel)
library(tidyr)
library(ggplot2)

g.trim <- readRDS("../twitter_data/all/graph_advanced.Rds")
tweets.summary <- readRDS("../twitter_data/all/tweets_keywordsearch_advanced.Rds")
tweets <- readRDS("../twitter_data/all/tweets.Rds")
likes <- readRDS("../twitter_data/all/likes")
names(tweets)

tweets <- cbind(tweets,tweets.summary)
tweets <- tweets[,c("screenName","isRetweet",       
                     "ats","hashtags","affordable","architect","bed","bim","build","carbon and climate",
                     "CIBSE","CIH","construction","design","doors and windows",  "eco","economy","efficiency",
                     "electric","energy","engineering","environment","flood","floor","fuel","gas",
                     "green","heating","homeless","house","infrastructure","installer","insulation","JRF",
                     "land","landlord","natfed","neighbourhood","passivehouse","plumbing","poverty","property",
                     "renewables","rent","residential","retrofit","RIBA","RICS","roof","skills",
                     "smart","surveying","sustainability","tax and benefits","timber","trades","keywords.all")]

names(tweets) <- sub(" ","",names(tweets))
names(tweets) <- sub(" ","",names(tweets))

cluster.summary <- readRDS("../twitter_data/all/cluster_summaries_advanced.Rds")
#foo <- cluster.summary[[1]]
#bar <- foo[[1]]

#remove tweets that have no outward connections
retweets = tweets[tweets$isRetweet,]
mentions = tweets[!tweets$isRetweet,]
mentions = mentions[!is.na(mentions$ats),]

#create account to cluster lookup table
clusters = igraph::as_data_frame(g.trim, what = "vertices")
clusters = clusters[,c("name","cluster.friends")]
names(clusters) <- c("name","cluster")

retweets$retweetOf <- sapply(retweets$ats, function(x){return(sub("@","",x[[1]]))})
retweets = left_join(retweets, clusters, by = c("screenName" = "name"))
names(retweets)[names(retweets) == 'cluster'] <- 'cluster.from'
retweets = left_join(retweets, clusters, by = c("retweetOf" = "name"))
names(retweets)[names(retweets) == 'cluster'] <- 'cluster.to'

retweets <- retweets[!is.na(retweets$cluster.from) & !is.na(retweets$cluster.to),]

retweets.summary <- retweets %>% 
                    group_by(cluster.from, cluster.to) %>%
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
                                        retweetscount = length(screenName))


summary(lengths(mentions$ats))
head(mentions$ats)
head(mentions)

mentions$id <- 1:nrow(mentions)
mentions.expanded <- mentions[rep(row.names(mentions), lengths(mentions$ats)),]

simplify.ats <- function(x){
  rownm <- rownames(mentions.expanded)[x]
  rowats <- mentions.expanded$ats[[x]]
  if(grepl("\\.",rownm)){
    #Has point
    numb <- substr(rownm,regexec("\\.",rownm)[[1]][1] +1,nchar(rownm))
    numb <- as.integer(numb)
    mumb = numb + 1
    result <- rowats[numb]
  }else{
    #No point retun single value
    result <- rowats[1]
  }
}

mentions.expanded$ats.simple <- sapply(1:nrow(mentions.expanded),simplify.ats)

mentions.expanded$mentionOf <- sapply(mentions.expanded$ats.simple, function(x){return(sub("@","",x[[1]]))})
mentions.expanded = left_join(mentions.expanded, clusters, by = c("screenName" = "name"))
names(mentions.expanded)[names(mentions.expanded) == 'cluster'] <- 'cluster.from'
mentions.expanded = left_join(mentions.expanded, clusters, by = c("mentionOf" = "name"))
names(mentions.expanded)[names(mentions.expanded) == 'cluster'] <- 'cluster.to'


mentions.summary <- mentions.expanded %>% 
  group_by(cluster.from, cluster.to) %>%
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
            mentionsCount = length(screenName))



retweets.summary.main <- retweets.summary[retweets.summary$cluster.from <= 26 & retweets.summary$cluster.to <= 26,]
mentions.summary.main <- mentions.summary[!is.na(mentions.summary$cluster.from) & !is.na(mentions.summary$cluster.to),]
mentions.summary.main <- mentions.summary.main[mentions.summary.main$cluster.from <= 26 & mentions.summary.main$cluster.to <= 26,]

#change counts to rates
for(i in 3:55){
  retweets.summary.main[,i] <- retweets.summary.main[,i] / retweets.summary.main$retweetscount
}

for(i in 3:55){
  mentions.summary.main[,i] <- mentions.summary.main[,i] / mentions.summary.main$mentionsCount
}

#Add the infro onto the contracted graph

g.contract <- readRDS("../twitter_data/all/graph_contract_advanced.Rds")
layout.contract <- readRDS("../twitter_data/all/layout_contract_advanced.Rds")

#change names to cluster numbers
V(g.contract)$name <- V(g.contract)$cluster.friends

retweets.keywords.internal <- retweets.summary.main[retweets.summary.main$cluster.from == retweets.summary.main$cluster.to,]
retweets.keywords.external <- retweets.summary.main[retweets.summary.main$cluster.from != retweets.summary.main$cluster.to,]
names(retweets.keywords.internal) = paste0("rt",names(retweets.keywords.internal))
names(retweets.keywords.external) = paste0("rt",names(retweets.keywords.external))

mentions.summary.main <- as.data.frame(mentions.summary.main)
mentions.keywords.internal <- mentions.summary.main[mentions.summary.main$cluster.from == mentions.summary.main$cluster.to,]
mentions.keywords.external <- mentions.summary.main[mentions.summary.main$cluster.from != mentions.summary.main$cluster.to,]
names(mentions.keywords.internal) = paste0("mn",names(mentions.keywords.internal))
names(mentions.keywords.external) = paste0("mn",names(mentions.keywords.external))

#join retweets to graph

verts.keywords <- igraph::as_data_frame(g.contract, what = "vertices")
retweets.keywords.internal$rtcluster.from <- as.character(retweets.keywords.internal$rtcluster.from)
verts.keywords <- left_join(verts.keywords, retweets.keywords.internal, by = c("name" = "rtcluster.from"))

verts.keywords[is.na(verts.keywords)] <- 0

V(g.contract)$rtaffordable    <- verts.keywords$rtaffordable
V(g.contract)$rtarchitect     <- verts.keywords$rtarchitect
V(g.contract)$rtbed           <- verts.keywords$rtbed
V(g.contract)$rtbim           <- verts.keywords$rtbim
V(g.contract)$rtbuild         <- verts.keywords$rtbuild
V(g.contract)$rtcarbonandclimate <- verts.keywords$rtcarbonandclimate
V(g.contract)$rtCIBSE         <- verts.keywords$rtCIBSE
V(g.contract)$rtCIH           <- verts.keywords$rtCIH
V(g.contract)$rtconstruction  <- verts.keywords$rtconstruction
V(g.contract)$rtdesign        <- verts.keywords$rtdesign
V(g.contract)$rtdoorsandwindows <- verts.keywords$rtdoorsandwindows
V(g.contract)$rteco             <- verts.keywords$rteco
V(g.contract)$rteconomy         <- verts.keywords$rteconomy
V(g.contract)$rtefficiency      <- verts.keywords$rtefficiency
V(g.contract)$rtelectric        <- verts.keywords$rtelectric
V(g.contract)$rtenergy          <- verts.keywords$rtenergy
V(g.contract)$rtengineering     <- verts.keywords$rtengineering
V(g.contract)$rtenvironment     <- verts.keywords$rtenvironment
V(g.contract)$rtflood           <- verts.keywords$rtflood
V(g.contract)$rtfloor           <- verts.keywords$rtfloor
V(g.contract)$rtfuel            <- verts.keywords$rtfuel
V(g.contract)$rtgas             <- verts.keywords$rtgas
V(g.contract)$rtgreen           <- verts.keywords$rtgreen
V(g.contract)$rtheating         <- verts.keywords$rtheating
V(g.contract)$rthomeless        <- verts.keywords$rthomeless
V(g.contract)$rthouse           <- verts.keywords$rthouse
V(g.contract)$rtinfrastructure  <- verts.keywords$rtinfrastructure
V(g.contract)$rtinstaller       <- verts.keywords$rtinstaller
V(g.contract)$rtinsulation      <- verts.keywords$rtinsulation
V(g.contract)$rtJRF             <- verts.keywords$rtJRF
V(g.contract)$rtland            <- verts.keywords$rtland
V(g.contract)$rtlandlord        <- verts.keywords$rtlandlord
V(g.contract)$rtnatfed          <- verts.keywords$rtnatfed
V(g.contract)$rtneighbourhood   <- verts.keywords$rtneighbourhood
V(g.contract)$rtpassivehouse    <- verts.keywords$rtpassivehouse
V(g.contract)$rtplumbing        <- verts.keywords$rtplumbing
V(g.contract)$rtpoverty         <- verts.keywords$rtpoverty
V(g.contract)$rtproperty        <- verts.keywords$rtproperty
V(g.contract)$rtrenewables      <- verts.keywords$rtrenewables
V(g.contract)$rtrent            <- verts.keywords$rtrent
V(g.contract)$rtresidential     <- verts.keywords$rtresidential
V(g.contract)$rtretrofit        <- verts.keywords$rtretrofit
V(g.contract)$rtRIBA            <- verts.keywords$rtRIBA
V(g.contract)$rtRICS            <- verts.keywords$rtRICS
V(g.contract)$rtroof            <- verts.keywords$rtroof
V(g.contract)$rtskills          <- verts.keywords$rtskills
V(g.contract)$rtsmart           <- verts.keywords$rtsmart
V(g.contract)$rtsurveying       <- verts.keywords$rtsurveying
V(g.contract)$rtsustainability  <- verts.keywords$rtsustainability
V(g.contract)$rttaxandbenefits  <- verts.keywords$rttaxandbenefits
V(g.contract)$rttimber          <- verts.keywords$rttimber
V(g.contract)$rttrades          <- verts.keywords$rttrades
V(g.contract)$rtkeywords.all    <- verts.keywords$rtkeywords.all


edges.keywords <- igraph::as_data_frame(g.contract, what = "edges")
retweets.keywords.external$rtcluster.from <- as.character(retweets.keywords.external$rtcluster.from)
retweets.keywords.external$rtcluster.to <- as.character(retweets.keywords.external$rtcluster.to)
edges.keywords <- left_join(edges.keywords, retweets.keywords.external, by = c("from" = "rtcluster.from", "to" = "rtcluster.to"))

edges.keywords[is.na(edges.keywords)] <- 0

E(g.contract)$rtaffordable    <- edges.keywords$rtaffordable
E(g.contract)$rtarchitect     <- edges.keywords$rtarchitect
E(g.contract)$rtbed           <- edges.keywords$rtbed
E(g.contract)$rtbim           <- edges.keywords$rtbim
E(g.contract)$rtbuild         <- edges.keywords$rtbuild
E(g.contract)$rtcarbonandclimate <- edges.keywords$rtcarbonandclimate
E(g.contract)$rtCIBSE         <- edges.keywords$rtCIBSE
E(g.contract)$rtCIH           <- edges.keywords$rtCIH
E(g.contract)$rtconstruction  <- edges.keywords$rtconstruction
E(g.contract)$rtdesign        <- edges.keywords$rtdesign
E(g.contract)$rtdoorsandwindows <- edges.keywords$rtdoorsandwindows
E(g.contract)$rteco             <- edges.keywords$rteco
E(g.contract)$rteconomy         <- edges.keywords$rteconomy
E(g.contract)$rtefficiency      <- edges.keywords$rtefficiency
E(g.contract)$rtelectric        <- edges.keywords$rtelectric
E(g.contract)$rtenergy          <- edges.keywords$rtenergy
E(g.contract)$rtengineering     <- edges.keywords$rtengineering
E(g.contract)$rtenvironment     <- edges.keywords$rtenvironment
E(g.contract)$rtflood           <- edges.keywords$rtflood
E(g.contract)$rtfloor           <- edges.keywords$rtfloor
E(g.contract)$rtfuel            <- edges.keywords$rtfuel
E(g.contract)$rtgas             <- edges.keywords$rtgas
E(g.contract)$rtgreen           <- edges.keywords$rtgreen
E(g.contract)$rtheating         <- edges.keywords$rtheating
E(g.contract)$rthomeless        <- edges.keywords$rthomeless
E(g.contract)$rthouse           <- edges.keywords$rthouse
E(g.contract)$rtinfrastructure  <- edges.keywords$rtinfrastructure
E(g.contract)$rtinstaller       <- edges.keywords$rtinstaller
E(g.contract)$rtinsulation      <- edges.keywords$rtinsulation
E(g.contract)$rtJRF             <- edges.keywords$rtJRF
E(g.contract)$rtland            <- edges.keywords$rtland
E(g.contract)$rtlandlord        <- edges.keywords$rtlandlord
E(g.contract)$rtnatfed          <- edges.keywords$rtnatfed
E(g.contract)$rtneighbourhood   <- edges.keywords$rtneighbourhood
E(g.contract)$rtpassivehouse    <- edges.keywords$rtpassivehouse
E(g.contract)$rtplumbing        <- edges.keywords$rtplumbing
E(g.contract)$rtpoverty         <- edges.keywords$rtpoverty
E(g.contract)$rtproperty        <- edges.keywords$rtproperty
E(g.contract)$rtrenewables      <- edges.keywords$rtrenewables
E(g.contract)$rtrent            <- edges.keywords$rtrent
E(g.contract)$rtresidential     <- edges.keywords$rtresidential
E(g.contract)$rtretrofit        <- edges.keywords$rtretrofit
E(g.contract)$rtRIBA            <- edges.keywords$rtRIBA
E(g.contract)$rtRICS            <- edges.keywords$rtRICS
E(g.contract)$rtroof            <- edges.keywords$rtroof
E(g.contract)$rtskills          <- edges.keywords$rtskills
E(g.contract)$rtsmart           <- edges.keywords$rtsmart
E(g.contract)$rtsurveying       <- edges.keywords$rtsurveying
E(g.contract)$rtsustainability  <- edges.keywords$rtsustainability
E(g.contract)$rttaxandbenefits  <- edges.keywords$rttaxandbenefits
E(g.contract)$rttimber          <- edges.keywords$rttimber
E(g.contract)$rttrades          <- edges.keywords$rttrades
E(g.contract)$rtkeywords.all    <- edges.keywords$rtkeywords.all

#join mentions to graph

verts.keywords <- igraph::as_data_frame(g.contract, what = "vertices")
mentions.keywords.internal$mncluster.from <- as.character(mentions.keywords.internal$mncluster.from)
verts.keywords <- left_join(verts.keywords, mentions.keywords.internal, by = c("name" = "mncluster.from"))

verts.keywords[is.na(verts.keywords)] <- 0

V(g.contract)$mnaffordable    <- verts.keywords$mnaffordable
V(g.contract)$mnarchitect     <- verts.keywords$mnarchitect
V(g.contract)$mnbed           <- verts.keywords$mnbed
V(g.contract)$mnbim           <- verts.keywords$mnbim
V(g.contract)$mnbuild         <- verts.keywords$mnbuild
V(g.contract)$mncarbonandclimate <- verts.keywords$mncarbonandclimate
V(g.contract)$mnCIBSE         <- verts.keywords$mnCIBSE
V(g.contract)$mnCIH           <- verts.keywords$mnCIH
V(g.contract)$mnconstruction  <- verts.keywords$mnconstruction
V(g.contract)$mndesign        <- verts.keywords$mndesign
V(g.contract)$mndoorsandwindows <- verts.keywords$mndoorsandwindows
V(g.contract)$mneco             <- verts.keywords$mneco
V(g.contract)$mneconomy         <- verts.keywords$mneconomy
V(g.contract)$mnefficiency      <- verts.keywords$mnefficiency
V(g.contract)$mnelectric        <- verts.keywords$mnelectric
V(g.contract)$mnenergy          <- verts.keywords$mnenergy
V(g.contract)$mnengineering     <- verts.keywords$mnengineering
V(g.contract)$mnenvironment     <- verts.keywords$mnenvironment
V(g.contract)$mnflood           <- verts.keywords$mnflood
V(g.contract)$mnfloor           <- verts.keywords$mnfloor
V(g.contract)$mnfuel            <- verts.keywords$mnfuel
V(g.contract)$mngas             <- verts.keywords$mngas
V(g.contract)$mngreen           <- verts.keywords$mngreen
V(g.contract)$mnheating         <- verts.keywords$mnheating
V(g.contract)$mnhomeless        <- verts.keywords$mnhomeless
V(g.contract)$mnhouse           <- verts.keywords$mnhouse
V(g.contract)$mninfrastructure  <- verts.keywords$mninfrastructure
V(g.contract)$mninstaller       <- verts.keywords$mninstaller
V(g.contract)$mninsulation      <- verts.keywords$mninsulation
V(g.contract)$mnJRF             <- verts.keywords$mnJRF
V(g.contract)$mnland            <- verts.keywords$mnland
V(g.contract)$mnlandlord        <- verts.keywords$mnlandlord
V(g.contract)$mnnatfed          <- verts.keywords$mnnatfed
V(g.contract)$mnneighbourhood   <- verts.keywords$mnneighbourhood
V(g.contract)$mnpassivehouse    <- verts.keywords$mnpassivehouse
V(g.contract)$mnplumbing        <- verts.keywords$mnplumbing
V(g.contract)$mnpoverty         <- verts.keywords$mnpoverty
V(g.contract)$mnproperty        <- verts.keywords$mnproperty
V(g.contract)$mnrenewables      <- verts.keywords$mnrenewables
V(g.contract)$mnrent            <- verts.keywords$mnrent
V(g.contract)$mnresidential     <- verts.keywords$mnresidential
V(g.contract)$mnretrofit        <- verts.keywords$mnretrofit
V(g.contract)$mnRIBA            <- verts.keywords$mnRIBA
V(g.contract)$mnRICS            <- verts.keywords$mnRICS
V(g.contract)$mnroof            <- verts.keywords$mnroof
V(g.contract)$mnskills          <- verts.keywords$mnskills
V(g.contract)$mnsmart           <- verts.keywords$mnsmart
V(g.contract)$mnsurveying       <- verts.keywords$mnsurveying
V(g.contract)$mnsustainability  <- verts.keywords$mnsustainability
V(g.contract)$mntaxandbenefits  <- verts.keywords$mntaxandbenefits
V(g.contract)$mntimber          <- verts.keywords$mntimber
V(g.contract)$mntrades          <- verts.keywords$mntrades
V(g.contract)$mnkeywords.all    <- verts.keywords$mnkeywords.all


edges.keywords <- igraph::as_data_frame(g.contract, what = "edges")
mentions.keywords.external$mncluster.from <- as.character(mentions.keywords.external$mncluster.from)
mentions.keywords.external$mncluster.to <- as.character(mentions.keywords.external$mncluster.to)
edges.keywords <- left_join(edges.keywords, mentions.keywords.external, by = c("from" = "mncluster.from", "to" = "mncluster.to"))

edges.keywords[is.na(edges.keywords)] <- 0

E(g.contract)$mnaffordable    <- edges.keywords$mnaffordable
E(g.contract)$mnarchitect     <- edges.keywords$mnarchitect
E(g.contract)$mnbed           <- edges.keywords$mnbed
E(g.contract)$mnbim           <- edges.keywords$mnbim
E(g.contract)$mnbuild         <- edges.keywords$mnbuild
E(g.contract)$mncarbonandclimate <- edges.keywords$mncarbonandclimate
E(g.contract)$mnCIBSE         <- edges.keywords$mnCIBSE
E(g.contract)$mnCIH           <- edges.keywords$mnCIH
E(g.contract)$mnconstruction  <- edges.keywords$mnconstruction
E(g.contract)$mndesign        <- edges.keywords$mndesign
E(g.contract)$mndoorsandwindows <- edges.keywords$mndoorsandwindows
E(g.contract)$mneco             <- edges.keywords$mneco
E(g.contract)$mneconomy         <- edges.keywords$mneconomy
E(g.contract)$mnefficiency      <- edges.keywords$mnefficiency
E(g.contract)$mnelectric        <- edges.keywords$mnelectric
E(g.contract)$mnenergy          <- edges.keywords$mnenergy
E(g.contract)$mnengineering     <- edges.keywords$mnengineering
E(g.contract)$mnenvironment     <- edges.keywords$mnenvironment
E(g.contract)$mnflood           <- edges.keywords$mnflood
E(g.contract)$mnfloor           <- edges.keywords$mnfloor
E(g.contract)$mnfuel            <- edges.keywords$mnfuel
E(g.contract)$mngas             <- edges.keywords$mngas
E(g.contract)$mngreen           <- edges.keywords$mngreen
E(g.contract)$mnheating         <- edges.keywords$mnheating
E(g.contract)$mnhomeless        <- edges.keywords$mnhomeless
E(g.contract)$mnhouse           <- edges.keywords$mnhouse
E(g.contract)$mninfrastructure  <- edges.keywords$mninfrastructure
E(g.contract)$mninstaller       <- edges.keywords$mninstaller
E(g.contract)$mninsulation      <- edges.keywords$mninsulation
E(g.contract)$mnJRF             <- edges.keywords$mnJRF
E(g.contract)$mnland            <- edges.keywords$mnland
E(g.contract)$mnlandlord        <- edges.keywords$mnlandlord
E(g.contract)$mnnatfed          <- edges.keywords$mnnatfed
E(g.contract)$mnneighbourhood   <- edges.keywords$mnneighbourhood
E(g.contract)$mnpassivehouse    <- edges.keywords$mnpassivehouse
E(g.contract)$mnplumbing        <- edges.keywords$mnplumbing
E(g.contract)$mnpoverty         <- edges.keywords$mnpoverty
E(g.contract)$mnproperty        <- edges.keywords$mnproperty
E(g.contract)$mnrenewables      <- edges.keywords$mnrenewables
E(g.contract)$mnrent            <- edges.keywords$mnrent
E(g.contract)$mnresidential     <- edges.keywords$mnresidential
E(g.contract)$mnretrofit        <- edges.keywords$mnretrofit
E(g.contract)$mnRIBA            <- edges.keywords$mnRIBA
E(g.contract)$mnRICS            <- edges.keywords$mnRICS
E(g.contract)$mnroof            <- edges.keywords$mnroof
E(g.contract)$mnskills          <- edges.keywords$mnskills
E(g.contract)$mnsmart           <- edges.keywords$mnsmart
E(g.contract)$mnsurveying       <- edges.keywords$mnsurveying
E(g.contract)$mnsustainability  <- edges.keywords$mnsustainability
E(g.contract)$mntaxandbenefits  <- edges.keywords$mntaxandbenefits
E(g.contract)$mntimber          <- edges.keywords$mntimber
E(g.contract)$mntrades          <- edges.keywords$mntrades
E(g.contract)$mnkeywords.all    <- edges.keywords$mnkeywords.all



#likes

keywords <- read.csv("../twitter_data/keywordsummary_tagged.csv", stringsAsFactors = F)
keywords <- keywords[!keywords$group == "",]

keywords.simple <- keywords %>% group_by(group) %>% summarise(val = list(keyName))
rm(keywords)

# summarise likes keywords

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
  parLapply(cl, likes$text ,scan.tweet, keywords.number = nrow(keywords.simple))
}
cl <- makeCluster(7) #make clusert and set number of cores
clusterExport(cl=cl, varlist=c("keywords.simple"))
clusterExport(cl=cl, c('count.keywords') )
clusterEvalQ(cl, {library(stringr)})
likes.summary <- fun(cl)
stopCluster(cl)
end <- Sys.time()
message(paste0("Finished scanning ",nrow(likes)," tweets in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
rm(cl,fun, start, end)
##########################################################

ats.list <- sapply(likes.summary,function(x) x[1])
hashtags.list <- sapply(likes.summary,function(x) x[2])
counts.list <- sapply(likes.summary,function(x) x[3])
counts.list <- do.call("rbind",counts.list)
counts.total <- rowSums(counts.list)
counts.list <- as.data.frame(counts.list)
names(counts.list) <- keywords.simple$group

likes.summary.df <- data.frame(id = 1:length(likes.summary), ats = NA, hashtags = NA)
likes.summary.df$ats <- ats.list
likes.summary.df$hashtags <- hashtags.list

likes.summary.df <- bind_cols(likes.summary.df, counts.list)
likes.summary.df$keywords.all <- counts.total
likes.summary.df$id <- NULL

saveRDS(likes.summary.df,"../twitter_data/all/likes_keywordsearch_advanced.Rds")

likes <- bind_cols(likes, likes.summary.df)
likes <- likes[,names(likes)[!names(likes) %in% c("text","favorited","favoriteCount","replyToSN",
                                                  "created","truncated","replyToSID","id","replyToUID",
                                                  "statusSource","retweetCount","isRetweet","retweeted",
                                                  "longitude","latitude")]]

likes = left_join(likes, clusters, by = c("screenName" = "name"))
names(likes)[names(likes) == 'cluster'] <- 'cluster.from'
likes = left_join(likes, clusters, by = c("favOf" = "name"))
names(likes)[names(likes) == 'cluster'] <- 'cluster.to'

likes <- likes[!is.na(likes$cluster.from) & !is.na(likes$cluster.to),]

names(likes) <- sub(" ","",names(likes))
names(likes) <- sub(" ","",names(likes))

likes.summary <- likes %>% 
  group_by(cluster.from, cluster.to) %>%
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
            likescount = length(screenName))


saveRDS(mentions.summary,"../twitter_data/all/mentions_summary_advanced.Rds")
saveRDS(retweets.summary,"../twitter_data/all/retweets_summary_advanced.Rds")
saveRDS(likes.summary,"../twitter_data/all/likes_summary_advanced.Rds")

likes.summary.main <- likes.summary[likes.summary$cluster.from <= 26 & likes.summary$cluster.to <= 26,]

#change counts to rates
for(i in 3:55){
  likes.summary.main[,i] <- likes.summary.main[,i] / likes.summary.main$likescount
}



likes.keywords.internal <- likes.summary.main[likes.summary.main$cluster.from == likes.summary.main$cluster.to,]
likes.keywords.external <- likes.summary.main[likes.summary.main$cluster.from != likes.summary.main$cluster.to,]
names(likes.keywords.internal) = paste0("lk",names(likes.keywords.internal))
names(likes.keywords.external) = paste0("lk",names(likes.keywords.external))



verts.keywords <- igraph::as_data_frame(g.contract, what = "vertices")
likes.keywords.internal$lkcluster.from <- as.character(likes.keywords.internal$lkcluster.from)
verts.keywords <- left_join(verts.keywords, likes.keywords.internal, by = c("name" = "lkcluster.from"))

verts.keywords[is.na(verts.keywords)] <- 0

V(g.contract)$lkaffordable    <- verts.keywords$lkaffordable
V(g.contract)$lkarchitect     <- verts.keywords$lkarchitect
V(g.contract)$lkbed           <- verts.keywords$lkbed
V(g.contract)$lkbim           <- verts.keywords$lkbim
V(g.contract)$lkbuild         <- verts.keywords$lkbuild
V(g.contract)$lkcarbonandclimate <- verts.keywords$lkcarbonandclimate
V(g.contract)$lkCIBSE         <- verts.keywords$lkCIBSE
V(g.contract)$lkCIH           <- verts.keywords$lkCIH
V(g.contract)$lkconstruction  <- verts.keywords$lkconstruction
V(g.contract)$lkdesign        <- verts.keywords$lkdesign
V(g.contract)$lkdoorsandwindows <- verts.keywords$lkdoorsandwindows
V(g.contract)$lkeco             <- verts.keywords$lkeco
V(g.contract)$lkeconomy         <- verts.keywords$lkeconomy
V(g.contract)$lkefficiency      <- verts.keywords$lkefficiency
V(g.contract)$lkelectric        <- verts.keywords$lkelectric
V(g.contract)$lkenergy          <- verts.keywords$lkenergy
V(g.contract)$lkengineering     <- verts.keywords$lkengineering
V(g.contract)$lkenvironment     <- verts.keywords$lkenvironment
V(g.contract)$lkflood           <- verts.keywords$lkflood
V(g.contract)$lkfloor           <- verts.keywords$lkfloor
V(g.contract)$lkfuel            <- verts.keywords$lkfuel
V(g.contract)$lkgas             <- verts.keywords$lkgas
V(g.contract)$lkgreen           <- verts.keywords$lkgreen
V(g.contract)$lkheating         <- verts.keywords$lkheating
V(g.contract)$lkhomeless        <- verts.keywords$lkhomeless
V(g.contract)$lkhouse           <- verts.keywords$lkhouse
V(g.contract)$lkinfrastructure  <- verts.keywords$lkinfrastructure
V(g.contract)$lkinstaller       <- verts.keywords$lkinstaller
V(g.contract)$lkinsulation      <- verts.keywords$lkinsulation
V(g.contract)$lkJRF             <- verts.keywords$lkJRF
V(g.contract)$lkland            <- verts.keywords$lkland
V(g.contract)$lklandlord        <- verts.keywords$lklandlord
V(g.contract)$lknatfed          <- verts.keywords$lknatfed
V(g.contract)$lkneighbourhood   <- verts.keywords$lkneighbourhood
V(g.contract)$lkpassivehouse    <- verts.keywords$lkpassivehouse
V(g.contract)$lkplumbing        <- verts.keywords$lkplumbing
V(g.contract)$lkpoverty         <- verts.keywords$lkpoverty
V(g.contract)$lkproperty        <- verts.keywords$lkproperty
V(g.contract)$lkrenewables      <- verts.keywords$lkrenewables
V(g.contract)$lkrent            <- verts.keywords$lkrent
V(g.contract)$lkresidential     <- verts.keywords$lkresidential
V(g.contract)$lkretrofit        <- verts.keywords$lkretrofit
V(g.contract)$lkRIBA            <- verts.keywords$lkRIBA
V(g.contract)$lkRICS            <- verts.keywords$lkRICS
V(g.contract)$lkroof            <- verts.keywords$lkroof
V(g.contract)$lkskills          <- verts.keywords$lkskills
V(g.contract)$lksmart           <- verts.keywords$lksmart
V(g.contract)$lksurveying       <- verts.keywords$lksurveying
V(g.contract)$lksustainability  <- verts.keywords$lksustainability
V(g.contract)$lktaxandbenefits  <- verts.keywords$lktaxandbenefits
V(g.contract)$lktimber          <- verts.keywords$lktimber
V(g.contract)$lktrades          <- verts.keywords$lktrades
V(g.contract)$lkkeywords.all    <- verts.keywords$lkkeywords.all


edges.keywords <- igraph::as_data_frame(g.contract, what = "edges")
likes.keywords.external$lkcluster.from <- as.character(likes.keywords.external$lkcluster.from)
likes.keywords.external$lkcluster.to <- as.character(likes.keywords.external$lkcluster.to)
edges.keywords <- left_join(edges.keywords, likes.keywords.external, by = c("from" = "lkcluster.from", "to" = "lkcluster.to"))

edges.keywords[is.na(edges.keywords)] <- 0

E(g.contract)$lkaffordable    <- edges.keywords$lkaffordable
E(g.contract)$lkarchitect     <- edges.keywords$lkarchitect
E(g.contract)$lkbed           <- edges.keywords$lkbed
E(g.contract)$lkbim           <- edges.keywords$lkbim
E(g.contract)$lkbuild         <- edges.keywords$lkbuild
E(g.contract)$lkcarbonandclimate <- edges.keywords$lkcarbonandclimate
E(g.contract)$lkCIBSE         <- edges.keywords$lkCIBSE
E(g.contract)$lkCIH           <- edges.keywords$lkCIH
E(g.contract)$lkconstruction  <- edges.keywords$lkconstruction
E(g.contract)$lkdesign        <- edges.keywords$lkdesign
E(g.contract)$lkdoorsandwindows <- edges.keywords$lkdoorsandwindows
E(g.contract)$lkeco             <- edges.keywords$lkeco
E(g.contract)$lkeconomy         <- edges.keywords$lkeconomy
E(g.contract)$lkefficiency      <- edges.keywords$lkefficiency
E(g.contract)$lkelectric        <- edges.keywords$lkelectric
E(g.contract)$lkenergy          <- edges.keywords$lkenergy
E(g.contract)$lkengineering     <- edges.keywords$lkengineering
E(g.contract)$lkenvironment     <- edges.keywords$lkenvironment
E(g.contract)$lkflood           <- edges.keywords$lkflood
E(g.contract)$lkfloor           <- edges.keywords$lkfloor
E(g.contract)$lkfuel            <- edges.keywords$lkfuel
E(g.contract)$lkgas             <- edges.keywords$lkgas
E(g.contract)$lkgreen           <- edges.keywords$lkgreen
E(g.contract)$lkheating         <- edges.keywords$lkheating
E(g.contract)$lkhomeless        <- edges.keywords$lkhomeless
E(g.contract)$lkhouse           <- edges.keywords$lkhouse
E(g.contract)$lkinfrastructure  <- edges.keywords$lkinfrastructure
E(g.contract)$lkinstaller       <- edges.keywords$lkinstaller
E(g.contract)$lkinsulation      <- edges.keywords$lkinsulation
E(g.contract)$lkJRF             <- edges.keywords$lkJRF
E(g.contract)$lkland            <- edges.keywords$lkland
E(g.contract)$lklandlord        <- edges.keywords$lklandlord
E(g.contract)$lknatfed          <- edges.keywords$lknatfed
E(g.contract)$lkneighbourhood   <- edges.keywords$lkneighbourhood
E(g.contract)$lkpassivehouse    <- edges.keywords$lkpassivehouse
E(g.contract)$lkplumbing        <- edges.keywords$lkplumbing
E(g.contract)$lkpoverty         <- edges.keywords$lkpoverty
E(g.contract)$lkproperty        <- edges.keywords$lkproperty
E(g.contract)$lkrenewables      <- edges.keywords$lkrenewables
E(g.contract)$lkrent            <- edges.keywords$lkrent
E(g.contract)$lkresidential     <- edges.keywords$lkresidential
E(g.contract)$lkretrofit        <- edges.keywords$lkretrofit
E(g.contract)$lkRIBA            <- edges.keywords$lkRIBA
E(g.contract)$lkRICS            <- edges.keywords$lkRICS
E(g.contract)$lkroof            <- edges.keywords$lkroof
E(g.contract)$lkskills          <- edges.keywords$lkskills
E(g.contract)$lksmart           <- edges.keywords$lksmart
E(g.contract)$lksurveying       <- edges.keywords$lksurveying
E(g.contract)$lksustainability  <- edges.keywords$lksustainability
E(g.contract)$lktaxandbenefits  <- edges.keywords$lktaxandbenefits
E(g.contract)$lktimber          <- edges.keywords$lktimber
E(g.contract)$lktrades          <- edges.keywords$lktrades
E(g.contract)$lkkeywords.all    <- edges.keywords$lkkeywords.all









########

# plotting


map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}


pal <- colorRampPalette( c("red","yellow","springgreen","royalblue"))( 10 )
pal <- c("#D3D3D3",pal)


plot.keywords.contracted <- function(keyword,limit){
  png(filename=paste0("../twitter_plots/advanced/contracted/keyword_",keyword,".png"), width=4, height=4, units = 'in', res = 600, pointsize=4)   
  par(mar = c(0.01,0.01,0.01,0.01));   plot(g.contract,
                                            edge.width = E(g.contract)$retweets / 2000,
                                            vertex.size = V(g.contract)$strength.total / 20000 ,
                                            edge.arrow.size = 1,
                                            edge.curved = 0.2,
                                            vertex.label = ifelse(V(g.contract)$name %in% as.character(1:26),V(g.contract)$name,NA),
                                            vertex.color = map2color( vertex_attr(graph = g.contract, name = keyword, index = V(g.contract)) , pal, c(0,limit)),
                                            edge.color = map2color( edge_attr(graph = g.contract, name = keyword, index = E(g.contract)) , pal, c(0,limit)),
                                            vertex.label.family= "Arial",
                                            vertex.label.color = "black",
                                            vertex.frame.color = "black",
                                            layout = layout.contract, 
                                            rescale = T, 
                                            axes = F); dev.off()
}

#plot.keywords.contracted("retweets.build",0.1)

keyword.types <- names(retweets.keywords.external)
keyword.types <- keyword.types[!keyword.types %in% c("rtcluster.from","rtcluster.to","rtkeywords.all","rtretweetscount")]

for(i in 1:length(keyword.types)){
  print(paste0(Sys.time()," ",keyword.types[i]))
  plot.keywords.contracted(keyword.types[i], 0.05)
}
plot.keywords.contracted("rtkeywords.all", 1)

keyword.types <- names(mentions.keywords.external)
keyword.types <- keyword.types[!keyword.types %in% c("mncluster.from","mncluster.to","mnkeywords.all","mnmentionsCount")]

plot.keywords.contracted <- function(keyword,limit){
  png(filename=paste0("../twitter_plots/advanced/contracted/keyword_",keyword,".png"), width=4, height=4, units = 'in', res = 600, pointsize=4)   
  par(mar = c(0.01,0.01,0.01,0.01));   plot(g.contract,
                                            edge.width = E(g.contract)$mentions / 2000,
                                            vertex.size = V(g.contract)$strength.total / 20000 ,
                                            edge.arrow.size = 0.5,
                                            edge.curved = 0.2,
                                            vertex.color = map2color( vertex_attr(graph = g.contract, name = keyword, index = V(g.contract)) , pal, c(0,limit)),
                                            edge.color = map2color( edge_attr(graph = g.contract, name = keyword, index = E(g.contract)) , pal, c(0,limit)),
                                            vertex.label = ifelse(V(g.contract)$name %in% as.character(1:26),V(g.contract)$name,NA),
                                            vertex.label.family= "Arial",
                                            vertex.label.color = "black",
                                            vertex.frame.color = "black",
                                            layout = layout.contract, 
                                            rescale = T, 
                                            axes = F); dev.off()
}





for(i in 1:length(keyword.types)){
  print(paste0(Sys.time()," ",keyword.types[i]))
  plot.keywords.contracted(keyword.types[i], 0.05)
}

plot.keywords.contracted("mnkeywords.all", 1)

keyword.types <- names(likes.keywords.external)
keyword.types <- keyword.types[!keyword.types %in% c("lkcluster.from","lkcluster.to","lkkeywords.all","lkmentionsCount")]

plot.keywords.contracted <- function(keyword,limit){
  png(filename=paste0("../twitter_plots/advanced/contracted/keyword_",keyword,".png"), width=4, height=4, units = 'in', res = 600, pointsize=4)   
  par(mar = c(0.01,0.01,0.01,0.01));   plot(g.contract,
                                            edge.width = E(g.contract)$likes / 2000,
                                            vertex.size = V(g.contract)$strength.total / 20000 ,
                                            edge.arrow.size = 1,
                                            edge.curved = 0.2,
                                            vertex.color = map2color( vertex_attr(graph = g.contract, name = keyword, index = V(g.contract)) , pal, c(0,limit)),
                                            edge.color = map2color( edge_attr(graph = g.contract, name = keyword, index = E(g.contract)) , pal, c(0,limit)),
                                            vertex.label = ifelse(V(g.contract)$name %in% as.character(1:26),V(g.contract)$name,NA),
                                            vertex.label.family= "Arial",
                                            vertex.label.color = "black",
                                            vertex.frame.color = "black",
                                            layout = layout.contract, 
                                            rescale = T, 
                                            axes = F); dev.off()
}





for(i in 1:length(keyword.types)){
  print(paste0(Sys.time()," ",keyword.types[i]))
  plot.keywords.contracted(keyword.types[i], 0.05)
}

plot.keywords.contracted("lkkeywords.all", 1)

saveRDS(g.contract,"../twitter_data/all/graph_contract_advanced_keywords.Rds")

## plot a master graph of allkeyword usuage

E(g.contract)$allkewords    <- E(g.contract)$lkkeywords.all + E(g.contract)$mnkeywords.all + E(g.contract)$rtkeywords.all
V(g.contract)$allkewords    <- V(g.contract)$lkkeywords.all + V(g.contract)$mnkeywords.all + V(g.contract)$rtkeywords.all

png(filename=paste0("../twitter_plots/advanced/contracted/allkeyword_alltypes.png"), width=4, height=4, units = 'in', res = 600, pointsize=4)   
par(mar = c(0.01,0.01,0.01,0.01));   plot(g.contract,
                                          edge.width = E(g.contract)$weight / 10000,
                                          vertex.size = V(g.contract)$strength.total / 20000 ,
                                          edge.arrow.size = 1,
                                          edge.curved = 0.2,
                                          vertex.color = map2color( vertex_attr(graph = g.contract, name = "allkewords", index = V(g.contract)) , pal, c(0,2)),
                                          edge.color = map2color( edge_attr(graph = g.contract, name = "allkewords", index = E(g.contract)) , pal, c(0,2)),
                                          vertex.label = NA,
                                          vertex.label.family= "Arial",
                                          vertex.label.color = "black",
                                          vertex.frame.color = "black",
                                          layout = layout.contract, 
                                          rescale = T, 
                                          axes = F); dev.off()



#g.contract <- readRDS("../twitter_data/all/graph_contract_advanced_keywords.Rds")

# export the cluster summaries for internal vers external communications

likes.summary2 <- likes.summary
likes.summary2$count <- likes.summary2$likescount 
mentions.summary2 <- mentions.summary
mentions.summary2$count <- mentions.summary2$mentionsCount
retweets.summary2 <- retweets.summary
retweets.summary2$count <- retweets.summary2$retweetscount 

mentions.summary2 <- mentions.summary2[,names(mentions.summary2)[names(mentions.summary2) %in% names(likes.summary2)]]
retweets.summary2 <- retweets.summary2[,names(retweets.summary2)[names(retweets.summary2) %in% names(likes.summary2)]]
likes.summary2 <- likes.summary2[,names(likes.summary2)[names(likes.summary2) %in% names(mentions.summary2)]]

extermal.comms <- rbind(likes.summary2, mentions.summary2)
extermal.comms <- rbind(extermal.comms, retweets.summary2)

extermal.comms.summary <- extermal.comms %>%
                  group_by(cluster.from, cluster.to) %>%
                  summarise_all(funs(sum))

extermal.comms.summary <- extermal.comms.summary[!is.na(extermal.comms.summary$cluster.from) & !is.na(extermal.comms.summary$cluster.to),]
extermal.comms.summary <- extermal.comms.summary[extermal.comms.summary$cluster.from <= 26 & extermal.comms.summary$cluster.to <= 26, ]

internal.comms.summary <- extermal.comms.summary[extermal.comms.summary$cluster.from == extermal.comms.summary$cluster.to,]
external.comms.summary <- extermal.comms.summary[extermal.comms.summary$cluster.from != extermal.comms.summary$cluster.to,]

external.comms.summary <- external.comms.summary %>%
  group_by(cluster.from) %>%
  summarise_all(funs(sum))

external.comms.summary$cluster.to <- NULL

names(external.comms.summary) <- paste0("ext_",names(external.comms.summary))
names(internal.comms.summary) <- paste0("int_",names(internal.comms.summary))

#convert to rates
for(i in 2:54){
  external.comms.summary[,i] <- external.comms.summary[,i] / external.comms.summary$ext_count
}

for(i in 3:55){
  internal.comms.summary[,i] <- internal.comms.summary[,i] / internal.comms.summary$int_count
}



internal.external <- bind_cols(internal.comms.summary,external.comms.summary)
internal.external <- left_join(internal.external)
internal.external <- as.data.frame(internal.external)

saveRDS(internal.external,"../twitter_data/all/internal_external.Rds")

for(i in 3:55){
  intval = internal.external[,i]
  extval = internal.external[,i + 55]
  maxval = max(c(extval,intval))
  maxval = ceiling(maxval/0.01)*0.01
  
  jpeg(filename=paste0("../twitter_plots/advanced/internalexternal/",names(internal.external)[i],".jpg"), width=4, height=4, units = 'in', res = 600, pointsize=8)
  plot(intval, 
       extval, 
       xlim = c(0,maxval), ylim = c(0,maxval),
       xlab = names(internal.external)[i],
       ylab = names(internal.external)[i + 55]);
  text(intval, 
       extval, 
       internal.external$int_cluster.from, cex=0.6, pos=4, col="red");
  abline(0,1);
  dev.off()
  
}



#### make matirx of what people are talking about
verts.contrat = igraph::as_data_frame(g.contract, what = "vertices")
verts.contrat = verts.contrat[verts.contrat$name %in% as.character(1:26),]
edges.contrat = igraph::as_data_frame(g.contract, what = "edges")
edges.contrat = edges.contrat[edges.contrat$from %in% as.character(1:26),]
edges.contrat = edges.contrat[edges.contrat$to %in% as.character(1:26),]

top_keyword = function(data,type,value = F){
  data = data[,names(data)[grepl(type,substr(names(data),1,2))]]
  data = data[,names(data)[!grepl("keywords.all",names(data))]]
  data = data[,names(data)[!grepl("house",names(data))]]
  if(value){
    colMax <- apply(data, 1, function(x) max(x))
    attributes(colMax) = NULL
    return(colMax)
  }else{
    top_name = colnames(data)[max.col(data,ties.method="first")]
    top_name = substring(top_name,3)
    return(top_name)
  }
}

edges.contrat$toprt       = top_keyword(edges.contrat,"rt",F)
edges.contrat$toprt.value = top_keyword(edges.contrat,"rt",T)
edges.contrat$topmn       = top_keyword(edges.contrat,"mn",F)
edges.contrat$topmn.value = top_keyword(edges.contrat,"mn",T)
edges.contrat$toplk       = top_keyword(edges.contrat,"lk",F)
edges.contrat$toplk.value = top_keyword(edges.contrat,"lk",T)

edges.contrat = edges.contrat[,c("from","to","friends","likes","mentions","retweets","weight","toprt","toprt.value","topmn","topmn.value","toplk","toplk.value")]

verts.contrat$toprt       = top_keyword(verts.contrat,"rt",F)
verts.contrat$toprt.value = top_keyword(verts.contrat,"rt",T)
verts.contrat$topmn       = top_keyword(verts.contrat,"mn",F)
verts.contrat$topmn.value = top_keyword(verts.contrat,"mn",T)
verts.contrat$toplk       = top_keyword(verts.contrat,"lk",F)
verts.contrat$toplk.value = top_keyword(verts.contrat,"lk",T)

verts.contrat$from = verts.contrat$name
verts.contrat$to = verts.contrat$name
verts.contrat = verts.contrat[,c("from","to","toprt","toprt.value","topmn","topmn.value","toplk","toplk.value")]

verts.contrat$friends = NA
verts.contrat$likes = NA
verts.contrat$mentions = NA
verts.contrat$retweets = NA
verts.contrat$weight = NA

verts.contrat = verts.contrat[,c("from","to","friends","likes","mentions","retweets","weight","toprt","toprt.value","topmn","topmn.value","toplk","toplk.value")]


cluster.names = read.csv("../twitter_data/cluster_summaries_advanced.csv", stringsAsFactors = F)
cluster.names = cluster.names[,c("id","name")]

top_keywords = rbind(verts.contrat,edges.contrat)
#make interaction matrixes
# make_matrix = function(type, value = FALSE){
#   if(value){
#     field = paste0(type,".value")
#   }else{
#     field = type
#   }
#   
#   top = top_keywords[,c("from","to",field)] %>%
#     spread(key = "to", value = field)
#   rownames(top) = top$from
#   top$from = NULL
#   top = top[order(as.numeric(rownames(top))),]
#   top = top[,order(as.numeric(names(top)))]
#   names(top) = cluster.names$name
#   rownames(top) = cluster.names$name
#   
#   return(top)
# }
# 
# 
# top_rt = make_matrix("toprt", value = FALSE)
# top_rt.value = make_matrix("toprt", value = TRUE)
# top_mn = make_matrix("topmn", value = FALSE)
# top_mn.value = make_matrix("topmn", value = TRUE)
# top_lk = make_matrix("toplk", value = FALSE)
# top_lk.value = make_matrix("toplk", value = TRUE)
# 
# write.csv(top_rt, "../twitter_data/interaction_matrix_rt.csv")
# write.csv(top_rt.value, "../twitter_data/interaction_matrix_rtvalue.csv")
# write.csv(top_mn, "../twitter_data/interaction_matrix_mn.csv")
# write.csv(top_mn.value, "../twitter_data/interaction_matrix_mnvalue.csv")
# write.csv(top_lk, "../twitter_data/interaction_matrix_lk.csv")
# write.csv(top_lk.value, "../twitter_data/interaction_matrix_lkvalue.csv")

#plot
library(reshape2)
top_keywords$from = as.integer(top_keywords$from)
top_keywords$to = as.integer(top_keywords$to)
top_keywords = left_join(top_keywords, cluster.names, by = c("to" = "id"))
top_keywords = left_join(top_keywords, cluster.names, by = c("from" = "id"))
names(top_keywords) = c("from","to","fr","lk","mn","rt","weight","toprt","toprt.value", "topmn","topmn.value", "toplk","toplk.value","name.to","name.from")

#to get the internal communciations counts
g.trim <- readRDS("../twitter_data/all/graph_advanced.Rds")
verts.trim = igraph::as_data_frame(g.trim, "vertices")
verts.trim = verts.trim[,c("name","cluster.friends")]
edge.trim = igraph::as_data_frame(g.trim, "edges")
names(verts.trim) = c("name","cluster.from")
edge.trim = left_join(edge.trim, verts.trim, by = c("from" = "name"))
names(verts.trim) = c("name","cluster.to")
edge.trim = left_join(edge.trim, verts.trim, by = c("to" = "name"))
#foo = top_keywords[,c("name.from","name.to","toprt","toprt.value")]
internal.summary = edge.trim[edge.trim$cluster.from == edge.trim$cluster.to,]
internal.summary = internal.summary[internal.summary$cluster.from <= 26 & internal.summary$cluster.to <= 26,]
internal.summary = internal.summary %>%
                    group_by(cluster.from, cluster.to) %>%
                    summarise(friends = sum(friends),
                              likes  = sum(likes),
                              mentions = sum (mentions),
                              retweets = sum (retweets), 
                              weight = sum(weight))

names(internal.summary) = c("from", "to","fr","lk","mn","rt","weight")

for(i in 1:26){
  top_keywords[i,"fr"] <- as.integer(internal.summary[i,"fr"])
  top_keywords[i,"lk"] <- as.integer(internal.summary[i,"lk"])
  top_keywords[i,"mn"] <- as.integer(internal.summary[i,"mn"])
  top_keywords[i,"rt"] <- as.integer(internal.summary[i,"rt"])
  top_keywords[i,"weight"] <- as.integer(internal.summary[i,"weight"])
}

#mat = as.matrix(top_rt.value)

#reorder_mat <- function(mat){
#  # Use correlation between variables as distance
#  rowsms  = rowSums(mat, na.rm = T)
#  #colsms = colSums(mat, na.rm = T)
#  mat = mat[order(-rowsms),order(-rowsms)]
#  return(mat)
#}
#mat = reorder_mat(mat)
#foo = melt(mat, na.rm = T)


plot_matrix = function(type){
  # get data
  data = top_keywords[,c("name.from", "name.to", type ,paste0("top",type),paste0("top",type,".value"))]
  data$score = round(data[,3] * data[,5],2)
  names(data) = c("from","to","count","lable","rate","value")
  
  datacolours = c("#a50026",
    "#d73027",
    "#f46d43",
    "#fdae61",
    "#fee090",
    "#ffffbf",
    "#e0f3f8",
    "#abd9e9",
    "#74add1",
    "#4575b4",
    "#313695")
  
  #datacolours = c("#D3D3D3", "#FF0000", "#FF5500", "#FFAA00", "#FFFF00", "#AAFF2A", "#54FF54", "#00FF7F", "#15CD9F", "#2B9BC0", "#4169E1")
  
  datamax = max(data$value, na.rm = T)
  
  datavalues = c(0,
                 quantile(data$value, probs = 0.10, na.rm = T)/datamax,
                 quantile(data$value, probs = 0.20, na.rm = T)/datamax,
                 quantile(data$value, probs = 0.30, na.rm = T)/datamax,
                 quantile(data$value, probs = 0.40, na.rm = T)/datamax,
                 quantile(data$value, probs = 0.50, na.rm = T)/datamax,
                 quantile(data$value, probs = 0.60, na.rm = T)/datamax,
                 quantile(data$value, probs = 0.70, na.rm = T)/datamax,
                 quantile(data$value, probs = 0.80, na.rm = T)/datamax,
                 quantile(data$value, probs = 0.90, na.rm = T)/datamax,
                 1)
  attributes(datavalues) = NULL
  
  
  png(filename=paste0("../twitter_plots/advanced/interactions/type_",type,"_2.png"), width=4, height=4, units = 'in', res = 600, pointsize=1)   
  par(mar = c(0.01,0.01,0.01,0.01));
    #Create a ggheatmap
  ggheatmap <- ggplot(data, aes(from, to, fill = value))+
    geom_tile(color = "white")+
    #scale_fill_gradient2(low = "#a50026", mid = "#ffffbf", high = "#4575b4", 
    #                     midpoint = quantile(data$value, probs = 0.90, na.rm = T), limit = c(1,max(data$value, na.rm = T)), space = "Lab", 
    #                     name="Keyword Frequency") +
    scale_fill_gradientn(colours = datacolours, values = datavalues, space = "Lab", na.value = "grey50", guide = "colourbar") +
    theme_minimal()+ # minimal theme
    theme(axis.title = element_text(size = 4)) +
    theme(axis.text.y = element_text(size = 4)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0, size = 4, hjust = 1) ) +
    theme(legend.position=c(-0.15, -0.1),legend.title=element_text(size=4), legend.key.size = unit(0.35,"cm"), legend.text = element_text(size=4)) +
    geom_text(aes(from, to, label = lable), color = "black", size = 1, angle = 30, hjust = 0.3) +
    coord_fixed()
  # Print the heatmap
  print(ggheatmap)
  
  #ggheatmap + 
    

  dev.off()
}

plot_matrix("rt")
plot_matrix("mn")
plot_matrix("lk")
  


#### 
