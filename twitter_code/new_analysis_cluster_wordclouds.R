# Word clouds

library("wordcloud")
library("RColorBrewer")

clusterres = readRDS("../twitter_data/cluster_summaries_advanced.Rds")

for(i in 1:length(clusterres)){
  kwds = clusterres[[i]][[2]]
  df = data.frame(word = names(kwds), count = kwds, stringsAsFactors = F)
  df$nchar = nchar(df$word)
  df = df[df$nchar > 3,]
  
  png(filename=paste0("../twitter_plots/advanced/wordclouds/cluster_",i,".png"), width=3, height=3, units = 'in', res = 600, pointsize=4)   
  par(mar = c(0.01,0.01,0.01,0.01)); wordcloud(words = df$word, freq = df$count, min.freq = 2,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2")); dev.off()
}

# word clodus of all clusters

clusterres = lapply(clusterres, `[[`, 2)

total = tapply(unlist(clusterres), names(unlist(clusterres)), sum)

df = data.frame(word = names(total), count = total, stringsAsFactors = F)
df$nchar = nchar(df$word)
df = df[df$nchar > 3,]

png(filename=paste0("../twitter_plots/advanced/wordclouds/allclusters.png"), width=3, height=3, units = 'in', res = 600, pointsize=4)   
par(mar = c(0.01,0.01,0.01,0.01)); wordcloud(words = df$word, freq = df$count, min.freq = 2,
                                             max.words=200, random.order=FALSE, rot.per=0.35, 
                                             colors=brewer.pal(8, "Dark2")); dev.off()
