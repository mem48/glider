# Now Analise the text of the tweets
# http://www.rdatamining.com/docs/twitter-analysis-with-r

foo <- tweets$text[1:100000]

# build a corpus, and specify the source to be character vectors
tweetCorpus <- Corpus(VectorSource(foo))

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(removeURL))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(removeNumPunct))

# convert to lower case
tweetCorpus <- tm_map(tweetCorpus, content_transformer(tolower))

# remove stopwords
myStopwords <- stopwords('english')

tweetCorpus <- tm_map(tweetCorpus, removeWords, myStopwords)
# remove extra whitespace
tweetCorpus <- tm_map(tweetCorpus, stripWhitespace)

#tweetCorpus_backup <- tm_map(tweetCorpus, stemDocument) # stem words

#writeLines(strwrap(tweetCorpus[[190]]$content, 60))

#Make Term Doument Matrix

tdm <- TermDocumentMatrix(tweetCorpus, control = list(wordLengths = c(1, Inf)))

#Look for Frequent Terms

freq.terms <- findFreqTerms(tdm, lowfreq = 100)
#tdm.freq  <- removeSparseTerms(tdm, 0.1) # This makes a matrix that is 10% empty space, maximum
#tdm.freq.mat <- 
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 100)
term.freq <- data.frame(term = names(term.freq), freq = term.freq)
