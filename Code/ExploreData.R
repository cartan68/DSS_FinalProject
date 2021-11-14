library(caret)
library(doParallel)
library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
# library(R.utils)
library(tidyr)
library(tidytext)
library(tm)
library(wordcloud)

blogsFile <- "./data/en_US/en_US.blogs.txt"
newsFile <- "./data/en_US/en_US.news.txt"
twitterFile <- "./data/en_US/en_US.twitter.txt"

blogsSize <- file.size(blogsFile) / 2^20
newsSize <- file.size(newsFile) / 2^20
twitterSize <- file.size(twitterFile)/ 2^20

sampleStats <- tibble(
    source = c("blogs", "news", "twitter"),
    fileSize_MB = c(blogsSize, newsSize, twitterSize)
)

set.seed(1234)
partitionPct <- 1.0

blogs <- readLines(blogsFile, skipNul = TRUE)
blogsLength <- length(blogs)
blogsPartition <- 
    tibble(text = sample(blogs, round(blogsLength * partitionPct, 0), replace = FALSE))
rm("blogs")

news <- readLines(newsFile, skipNul = TRUE)
newsLength <- length(news)
newsPartition <- 
    tibble(text = sample(news, round(newsLength * partitionPct, 0), replace = FALSE))
rm("news")

tweets <- readLines(twitterFile, skipNul = TRUE)
tweetsLength <- length(tweets)
tweetsPartition <- 
    tibble(text = sample(tweets, round(tweetsLength * partitionPct, 0), replace = FALSE))
rm("tweets")

sampleStats <- bind_cols(sampleStats,
                         tibble(lines = c(blogsLength, newsLength, tweetsLength)))

sampleDataSet <- bind_rows(mutate(blogsPartition, source = "blogs"),
                           mutate(newsPartition, source = "news"),
                           mutate(tweetsPartition, source = "twitter"))
sampleDataSet$source <- as.factor(sampleDataSet$source)
rm("blogsPartition", "newsPartition", "tweetsPartition")

# Set up for parallel operations to speed up processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

sampleDataSet$text <- removeNumbers(sampleDataSet$text)
sampleDataSet$text <- removePunctuation(sampleDataSet$text)

sampleTokens <- sampleDataSet %>% 
    unnest_tokens(word, text)
rm("sampleDataSet")

data("stop_words")

sampleTokens <- sampleTokens %>% 
    anti_join(stop_words, by = "word")
rm("stop_words")

blogsTokenCount <- sampleTokens %>% 
    filter(source == "blogs") %>% 
    with(length(word))

newsTokenCount <- sampleTokens %>% 
    filter(source == "news") %>% 
    with(length(word))

twitterTokenCount <- sampleTokens %>% 
    filter(source == "twitter") %>% 
    with(length(word))

sampleStats <- bind_cols(sampleStats,
                         tibble(tokens = c(blogsTokenCount, newsTokenCount, twitterTokenCount)))


sampleTokens <- sampleTokens %>% 
    count(word, source, sort = TRUE)

blogsTokenCount <- sampleTokens %>% 
    filter(source == "blogs") %>% 
    with(length(word))

newsTokenCount <- sampleTokens %>% 
    filter(source == "news") %>% 
    with(length(word))

twitterTokenCount <- sampleTokens %>% 
    filter(source == "twitter") %>% 
    with(length(word))

sampleStats <- bind_cols(sampleStats,
                         tibble(uniqueTokens = c(blogsTokenCount, newsTokenCount, twitterTokenCount)))

# Stop parallel processing
stopCluster(cluster)

sampleTF <- sampleTokens %>% 
    spread(sampleTokens, source, n)

sampleTF$total <- 0

sampleTF$total <- apply(sampleTF[ ,2:4], 1, "sum", na.rm = TRUE)

p1 <- sampleTF %>% 
    top_n(25, blogs) %>% 
    mutate(word = reorder(word, blogs)) %>%
    ggplot(aes(blogs, word)) +
        geom_col(fill = "blue") +
        labs(y = NULL)

p2 <- sampleTF %>% 
    top_n(25, news) %>% 
    mutate(word = reorder(word, news)) %>%
    ggplot(aes(news, word)) +
        geom_col(fill = "red") +
        labs(y = NULL)

p3 <- sampleTF %>% 
    top_n(25, twitter) %>% 
    mutate(word = reorder(word, twitter)) %>%
    ggplot(aes(twitter, word)) +
        geom_col(fill = "green") +
        labs(y = NULL)

p4 <- sampleTF %>% 
    top_n(25, total) %>% 
    mutate(word = reorder(word, total)) %>%
    ggplot(aes(total, word)) +
        geom_col(fill = "purple") +
        labs(y = NULL)

grid.arrange(p1, p2, p3, p4, nrow = 2)

sampleTF <- sampleTF %>% 
    mutate(blogPct = blogs / sum(blogs, na.rm = TRUE))

sampleTF <- sampleTF %>% 
    mutate(newsPct = news / sum(news, na.rm = TRUE))

sampleTF <- sampleTF %>% 
    mutate(twitterPct = twitter / sum(twitter, na.rm = TRUE))

# sampleTF <- sampleTF %>% 
#     mutate(totalPct = total / sum(total, na.rm = TRUE))

sampleTF$totalPct <- apply(sampleTF[ ,6:8], 1, "sum", na.rm = TRUE)

p1 <- sampleTF %>% 
    top_n(25, blogPct) %>% 
    mutate(word = reorder(word, blogPct)) %>%
    ggplot(aes(blogPct, word)) +
        geom_col(fill = "blue") +
        labs(y = NULL)

p2 <- sampleTF %>% 
    top_n(25, newsPct) %>% 
    mutate(word = reorder(word, newsPct)) %>%
    ggplot(aes(newsPct, word)) +
        geom_col(fill = "red") +
        labs(y = NULL)

p3 <- sampleTF %>% 
    top_n(25, twitterPct) %>% 
    mutate(word = reorder(word, twitterPct)) %>%
    ggplot(aes(twitterPct, word)) +
        geom_col(fill = "green") +
        labs(y = NULL)

p4 <- sampleTF %>% 
    top_n(25, totalPct) %>% 
    mutate(word = reorder(word, totalPct)) %>%
    ggplot(aes(totalPct, word)) +
    geom_col(fill = "purple") +
    labs(y = NULL)

grid.arrange(p1, p2, p3, p4, nrow = 2)


sampleTF %>% 
    mutate(word = reorder(word, totalPct)) %>%
    with(wordcloud(word, totalPct, max.words = 50, colors = brewer.pal(8, "Spectral")))
