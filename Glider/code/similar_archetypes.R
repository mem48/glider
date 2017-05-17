# To find siliar artchitypes
comb <- readRDS("data/combined_2013_arch.Rds")
sel <- comb[,c("aacode","aagpd1213","archcode","energytyp","generaltyp","rooftyp","solartyp","walltyp","wintyp","floortyp")]

sel <- sel[with(sel, order(rooftyp,solartyp,walltyp,wintyp,floortyp)),]
uni <- as.data.frame(unique(sel[,c("archcode","energytyp","generaltyp","rooftyp","solartyp","walltyp","wintyp","floortyp")]))


comp <- matrix(nrow = nrow(uni), ncol = nrow(uni))
comp <- as.data.frame(comp)
names(comp) <- uni$archcode
rownames(comp) <- uni$archcode

nrow <- length(uni$archcode)
ncell <-  nrow ** 2

y_last = 1

for(i in 1:ncell){
  y <- floor(i/nrow - 0.00000000000001) + 1 #Column Number
  x <- i - ((y-1) * nrow) #Row Number
  if(y != y_last){print(paste0("Doing column ",y," of ",nrow," at ",Sys.time()))}
  yid <- colnames(comp)[y]
  xid <- rownames(comp)[x]
  xval <- uni[uni$archcode == xid,]
  yval <- uni[uni$archcode == yid,]
  comp[x,y] <- sum(xval$energytyp == yval$energytyp, xval$generaltyp == yval$generaltyp, xval$rooftyp == yval$rooftyp, xval$solartyp == yval$solartyp, xval$walltyp == yval$walltyp, xval$wintyp == yval$wintyp, xval$floortyp == yval$floortyp)
  y_last <- y
}
