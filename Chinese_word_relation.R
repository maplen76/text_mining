library(jiebaR)
library(tidyverse)
library(openxlsx)
Sys.setlocale(category = "LC_ALL", locale = "chs")

# separator$bylines = TRUE # 分行输出
# separator$bylines = TRUE # 分行输出
# segment(tt, jiebar = separator)
# add new word into the separator engine
# new_user_word(worker = separator, words = "这是一个新词", "n")
# segment("这是一个新词", jiebar = separator)
# add new stop word into the separator engine
# readLines("stop.txt", encoding = "UTF-8")
# test_df <- data.frame(review = "这是一个新词")
# test_df %>% 
#    mutate(words = segment(review, jiebar = separator, code = "UTF-8"))


# read tieba review file
reviews <- read.xlsx(xlsxFile = "Review.xlsx", sheet = "Sheet1") %>%
  select(-1)
# clean data
reviews_tbl <- reviews %>%
    tbl_df() %>%
    mutate(review_new = str_replace_all(string = review,                        # remove emoji
                                        pattern = '\\p{So}|\\p{Cn}', 
                                        replacement = '')) %>%
    filter(nchar(review_new, type = "bytes") > 2)                               # remove review with 2 bytes

# build seprator
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

reviews_tbl <- reviews_tbl %>%
  mutate(text = sentence_orgnize(review_new, 2))

# unnest_tokens support chinese not as expected
# build special tokenizer function to unnest
library(tidytext)
library(tidyr)

tok99 = function(t) str_split(t,"[ ]{1,}")
review_words_2 <- reviews_tbl %>%
    unnest_tokens(word, text, token = tok99)

# splite words by 2 ngrams
rev <- review_words_2 %>%
    separate(word, c("word1", "word2"))

rev_filter <- rev %>%
    filter(!word1 %in% stopWords_CN) %>%
    filter(!word2 %in% stopWords_CN)

rev_counts <- rev_filter %>%
    count(word1, word2, sort = T)

library(igraph)
library(ggraph)
rev_graph <- rev_counts %>%                                                     # construct igraph object
    filter(n > 50) %>%
    graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
set.seed(2018)
ggraph(rev_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n, edge_colour = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 3) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()


# topic modeling
review_words_1 <- reviews_tbl %>%
  mutate(text1 = sentence_orgnize(review_new, 1)) %>%
  unnest_tokens(word, text1, token = tok99)

review_words <- review_words_1 %>%
  select(-(playtime:upvote)) %>%
  filter(!word %in% stopWords_CN) %>%
  filter(!word == "") %>%
  filter(!word %in% stop_words$word)

review_words_count <- review_words %>%
    count(review_id, word, sort = TRUE) %>%
    ungroup

review_tf_idf <- review_words_count %>%
  bind_tf_idf(word, review_id, n) %>%
  arrange(desc(tf_idf))

review_tf_idf_dtm <- review_tf_idf %>%
  cast_dtm(review_id, word, n)

library(topicmodels)
review_lda <- LDA(review_tf_idf_dtm, k = 4, control = list(seed = 1234))

# find the number of topics
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

# build wordcloud of each topics
library(wordcloud)
review_topic_ <- review_topic %>%
    filter(topic == 4)
wordcloud(words = review_topic_$term, 
          freq = review_topic_$beta, 
          random.order = FALSE, 
          max.words = 500,
          colors=brewer.pal(8, "Dark2"))
