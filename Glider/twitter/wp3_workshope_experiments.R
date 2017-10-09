# WP3 Workshop test trims

to.keep.2 <- verts2
to.keep.2$strDeg <- to.keep.2$strength.out / to.keep.2$degree.out
g.test2 <- delete.vertices(gt, which(degree(gt)<2000))

gorder(gt)
removelist <- c("BBCr4today","grantshapps",
                "CarolineLucas", "foreignoffice",
                "UKLabour", "TheGreenParty",
                "natalieben", "guardian", "Independent", "SustMeme", "SadiqKhan", "Ed_Miliband",
                "Channel4News", "BBCNewsnight", "GreenpeaceUK", "guardiannews",
                "wehelp_you_grow","MRAMarketing","SuButcher"
                
                )

core <- read.csv("../Glider/twitter/accounts.csv", stringsAsFactors = F)
core <- core$Twitter
core <- core[!is.na(core)]
core <- core[! core == ""]

#g.test3 <- delete.vertices(gt, V(gt)$name %in% removelist)
g.test3 <- delete.vertices(gt, V(gt)$name %in% core)
gorder(g.test3)
g.test3 <- delete.vertices(g.test3, which(degree(g.test3)<1800))

verts3 <- as_data_frame(g.test3, what="vertices")
verts3$degree.in <- degree(g.test3, mode = "in")
verts3$degree.out <- degree(g.test3, mode = "out")
verts3$strength.in <- strength(g.test3, mode = "in")
verts3$strength.out <- strength(g.test3, mode = "out")

gorder(g.test3)

degree <- degree(g.test3, mode = "total")
between <- betweenness(g.test3)
close <- closeness(g.test3)
summary(between)
summary(close)
V(g.test3)$between <- between/max(between)
V(g.test3)$close <- close/max(close)
summary(V(g.test3)$between)
summary(V(g.test3)$close)
V(g.test3)$degree <- degree / max(degree, na.rm = T)
summary(V(g.test3)$degree)

#Floor min values
#V(g.test)$degree[V(g.test)$degree == min(V(g.test)$degree)] <- 0
#V(g.test)$degree[V(g.test)$degree == min(V(g.test)$degree)] <- 0

#Color scaling function
c_scale <- colorRamp(c('white','red','yellow','cyan','blue'))

#Applying the color scale to edge weights.
#rgb method is to convert colors to a character vector.
V(g.test3)$color = apply(c_scale(V(g.test3)$between), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )


#Create High Resolution Plots
svg(filename="wp3demo-rem-startcore.svg", 
    width=15, 
    height=15, 
    pointsize=10)
par(mar = c(1,1,1,1))
plot(g.test3, 
     edge.width = E(g.test3)$weight/20,
     vertex.size = 3,
     vertex.color = V(g.test3)$color,
     layout = layout_nicely, 
     rescale = T, 
     axes = F)
dev.off()
