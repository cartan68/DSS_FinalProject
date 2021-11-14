library(tm)
library(caret)

docs <- Corpus(DirSource("./Data/en_US"))
summary(docs)

docs.tdm <- TermDocumentMatrix(docs,
                               control = list(removePunctuation = TRUE,
                                              stopwords = TRUE,
                                              tolower = TRUE,
                                              stemming = FALSE,
                                              removeNumbers = TRUE))

inspect(docs.tdm)

writeLines(as.character(docs[[1]]))

toSpace <- content_transformer(function(x, pattern) {return(gsub(pattern, " ", x))})

docs <- tm_map(docs, toSpace, "_")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, ":")

docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, stripWhitespace)

library(SnowballC)

docs <- tm_map(docs, stemDocument)

docs <- tm_map(docs,content_transformer(gsub), pattern = "organiz", replacement = "organ")

dtm <- DocumentTermMatrix(docs)

inspect(dtm[1, 1001:1007])

freq <- colSums(as.matrix(dtm))

ord <- order(freq, decreasing = TRUE)

freq[head(ord)]
freq[tail(ord)]

dtmr <- DocumentTermMatrix(docs, control = list(wordLengths = c(4, 20),
                                                bounds = list(global = c(3, 27))))

freqr <- colSums(as.matrix(dtmr))

ordr <- order(freqr, decreasing = TRUE)

freqr[head(ordr)]
freqr[tail(ordr)]

findFreqTerms(dtmr, lowfreq = 80)

findAssocs(dtmr, "project", 0.6)
findAssocs(dtmr, "enterpries", 0.6)
findAssocs(dtmr, "system", 0.6)

wf <- data.frame(term = names(freqr), occurrances = freqr)

library(ggplot2)
p <- ggplot(subset(wf, freq > 100), aes(term, occurrences)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

library(wordcloud)
set.seed(42)

wordcloud(names(freqr), freqr, min.freq = 70)
wordcloud(names(freqr), freqr, min.freq = 70, colors = brewer.pal(6, "Dark2"))




