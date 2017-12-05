
# 2 Sentiment Anaylysis with tidy data
##2.2 Sentiment analysis
library(janeaustenr)
library(dplyr)
library(stringr)


tidy_books <- austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                   ignore_case = TRUE)))) %>%
    ungroup() %>%
    unnest_tokens(word, text)

# use NRC lexicon
nrcjoy <- get_sentiments("nrc") %>%
    filter(sentiment == "joy")


tidy_books %>%
    filter(book == "Emma") %>%
    inner_join(nrcjoy) %>%
    count(word, sort = T)
    

library(tidyr)
janeaustensentiment <- tidy_books %>%
    inner_join(get_sentiments("bing")) %>% # use bing lexicon to analyze
    count(book, index = linenumber %/% 80, sentiment) %>% # operator %/% equal floor(x/y)
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative) # net sentiment

ggplot(data = janeaustensentiment, aes(index, sentiment, fill = book)) +
    geom_col(show.legend = F) +
    facet_wrap(~book, ncol = 2, scales = "free_x")


## 2.3 compare the 3 sentiment dictionaries: nrc, bing, afinn

pride_prejudice <- tidy_books %>% 
    filter(book == "Pride & Prejudice")

# use affin lexicon to measure the sentiment score
afinn <- pride_prejudice %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(index = linenumber %/% 80) %>% 
    summarise(sentiment = sum(score)) %>% 
    mutate(method = "AFINN")

# use bing and nrc
bing_and_nrc <- bind_rows(pride_prejudice %>% 
                              inner_join(get_sentiments("bing")) %>%
                              mutate(method = "Bing et al."),
                          pride_prejudice %>% 
                              inner_join(get_sentiments("nrc") %>% 
                                             filter(sentiment %in% c("positive", 
                                                                     "negative"))) %>%
                              mutate(method = "NRC")) %>%
    count(method, index = linenumber %/% 80, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

# visualize the sentiment
bind_rows(afinn, 
          bing_and_nrc) %>%
    ggplot(aes(index, sentiment, fill = method)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~method, ncol = 1, scales = "free_y")

## 2.4 Most common positive and negative words
bing_word_counts <- tidy_books %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()


# miss is used as title for young unmarried women
bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()

# create custom stop words list
custom_stop_words <- bind_rows(data_frame(word = c("miss"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words


## 2.5 Wordclouds

library(wordcloud)

tidy_books %>%
    anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words =  100))

library(reshape2)

tidy_books %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                     max.words = 100)

