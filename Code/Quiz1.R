


blogsFile <- "./data/en_US/en_US.blogs.txt"
newsFile <- "./data/en_US/en_US.news.txt"
twitterFile <- "./data/en_US/en_US.twitter.txt"

file.size(blogsFile)/(2^20)

tweets <- readLines(twitterFile, skipNul = TRUE)
length(tweets)

blogMax <- max(nchar(readLines(blogsFile, skipNul = TRUE)))
newsMax <- max(nchar(readLines(newsFile, skipNul = TRUE)))
twitterMax <- max(nchar(readLines(twitterFile, skipNul = TRUE)))

fileMax <- max(blogMax, newsMax, twitterMax)


loveLines <- grepl("love", tweets)
hateLines <- grepl("hate", tweets)

loveHateRatio <- sum(loveLines) / sum(hateLines)


biostatsLine <- grep("biostats", tweets)
tweets[biostatsLine]


tweetMatch <- grepl("A computer once beat me at chess, but it was no match for me at kickboxing",
                    tweets)
sum(tweetMatch)
