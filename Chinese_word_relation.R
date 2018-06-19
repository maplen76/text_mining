setwd("D:\\R\\textMining")
Sys.setlocale(category = "LC_ALL", locale = "chs")
# Sys.setlocale("LC_ALL", "zh_cn.utf-8")                                        # set local for macOS   

library(jiebaR)
library(dplyr)
library(openxlsx) # openxlsx perform quick than xlsx package without apply java
library(stringr)
# library(tmcn)

# read tieba review file
reviews <- read.xlsx(xlsxFile = "Review_Level_Tieba.xlsx", sheet = "Sheet1")
# clean data
reviews_tbl <- reviews %>% 
    tbl_df() %>%
    mutate(review_new = str_replace_all(string = review,                        # remove emoji
                                        pattern = '\\p{So}|\\p{Cn}', 
                                        replacement = '')) %>%
    filter(nchar(review_new, type = "bytes") > 2)                               # remove review with 2 bytes

separator <- worker()
new_user_word(separator, c("1V1","2V2","4V4"), c("n","n","n"))                  # build custom word dictionary
stopWords_CN <- readLines('stopwords-zh.txt', encoding = "UTF-8")
    
# user jiebaR to split words
# build function to orgnize word by n consecutive word
sentence_orgnize <- function (sentence, n = 2) {
    s <- sentence
    n <- n
    s_length <- length(s)
    f <- vector()
    
    for (i in 1:s_length) {
        
        temp <- vector()
        s_split <- separator <= s[i]
        nb_word <- length(s_split) - n + 1
        
        for (j in 1:nb_word) {
            temp[j] <- paste0(s_split[j:(j+n-1)], collapse = "_")
        }
        
        f[i] <- paste0(temp, collapse = " ")
        
    }
    
    return(f)
}


# unnest_tokens support chinese not as expected
library(tidytext)
library(tidyr)
tok99 = function(t) str_split(t,"[ ]{1,}")                                      # build special tokenizer function to unnest

# bigrams analysis #############################################################
review_words_bigrams <- reviews_tbl %>%
    mutate(text_2 = sentence_orgnize(review_new, 2)) %>%
    unnest_tokens(word, text_2, token = tok99)

rev_bigrams <- review_words_bigrams %>%
    separate(word, c("word1", "word2"))                                         # splite words into bigrams

rev_bigrams_filter <- rev_bigrams %>%
    filter(!word1 %in% stopWords_CN) %>%
    filter(!word2 %in% stopWords_CN)

rev_bigrams_counts <- rev_bigrams_filter %>%
    count(word1, word2, sort = T)

# visualize 2 grams
library(igraph)
rev_bigrams_graph <- rev_bigrams_counts %>%                                      # construct igraph object
    filter(n > 50) %>%
    graph_from_data_frame()

library(ggraph)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
set.seed(2018)
ggraph(rev_bigrams_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n, edge_colour = n), 
                   show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 3) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()



# co-occur analaysis ###########################################################
review_single <- reviews_tbl %>%
    mutate(text_1 = sentence_orgnize(review_new, 1)) %>%
    unnest_tokens(word, text_1, token = tok99) %>%
    select(-(playtime:upvote)) %>%
    filter(!word %in% stopWords_CN) %>%
    filter(!word == "") %>%
    filter(!word %in% stop_words$word)

review_words <- review_single %>%
    count(review_id,word, sort = T) %>%
    arrange(word)


library(widyr)
word_pairs <- review_single %>%
    pairwise_count(word, review_id, sort = T)

word_correlation <- review_single %>%                                           # figure out the relation betwen words
    group_by(word) %>%
    filter(n() > 20) %>%
    pairwise_cor(word, review_id, sort = T)

word_correlation_graph <- word_correlation %>%
    graph_from_data_frame()

word_correlation %>%
    filter(correlation > .6) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = T) +
    theme_void()


# topic modeling ###############################################################
review_dtm <- review_single %>%
    count(review_id, word, sort = TRUE) %>%
    cast_dtm(review_id, word, n)

result <- FindTopicsNumber(
    review_dtm,
    topics = c(1:10 * 10, 120, 140, 160, 180, 0:3 * 50 + 200),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 6L,
    verbose = TRUE
)

FindTopicsNumber_plot(result)

library(topicmodels)
review_lda <- LDA(review_dtm, k = 6, control = list(seed = 1234))

library(tidytext) # keep load the package at top
review_topic <- tidy(review_lda, matrix = "beta")

review_topic %>%
    group_by(topic) %>%
    top_n(20, beta) %>%
    ungroup() %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = F) +
    facet_wrap(~ topic, scales =  "free") +
    coord_flip()

write.xlsx(x = as.data.frame(review_topic), file = "topics.xlsx", row.names = F)

library(wordcloud)

review_topic_ <- review_topic %>%
    filter(topic == 4)

wordcloud(words = review_topic_$term, 
          freq = review_topic_$beta, 
          random.order = FALSE, 
          max.words = 500,
          colors=brewer.pal(8, "Dark2"))

