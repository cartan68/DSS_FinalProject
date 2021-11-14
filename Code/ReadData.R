# require(pdftools)
require(tidytext)
require(dplyr)
require(ggplot2)
require(wordcloud)



text <- read.table("./data/en_US/en_US.twitter.txt", header = TRUE, sep = " ", dec = ".")
text_df <- data_frame(line = 1:26, text = text)

tidy_bitcoins <- text_df %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words)

tidy_bitcoins %>% 
    count(word, sort = TRUE)

tidy_bitcoins %>% 
    count(word, sort = TRUE) %>% 
    filter(n > 40) %>% 
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n)) +
        geom_col() +
        xlab(NULL) +
        coord_flip()

tidy_bitcoins %>% 
    anti_join(stop_words) %>% 
    count(word) %>% 
    with(wordcloud(word, n, max.words = 10, colors = brewer.pal(6, "Dark2")))
    

