#####################################
# tidying DocumentTermMatrix objects
#####################################
library(tm)
library(dplyr)
library(tidytext)

data("AssociatedPress", package = "topicmodels")
terms <- Terms(AssociatedPress)

ap_td <- tidy(AssociatedPress) # convert DocumentTermMatrix Objects to tbl_df objects
ap_sentiments <- ap_td %>% # use bing lexicon to implement sentiment analysis
    inner_join(get_sentiments("bing"), by = c(term = "word"))
    
library(ggplot2)
ap_sentiments %>%
    count(sentiment, term, wt = count) %>%
    ungroup() %>%
    filter(n >= 200) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(term, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ylab("Contribution to sentiment") +
    coord_flip()

#######################################################
# tidying document-feature matrix objects
# tidytext::tidy() can be use to tidy both DTM and dfm
#######################################################
data("data_corpus_inaugural", package = "quanteda")

inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = F)    
inaug_td <- tidy(inaug_dfm)    

inaug_tf_idf <- inaug_td %>%
    bind_tf_idf(term, document, count) %>%
    arrange(desc(tf_idf))

year_term_counts <- inaug_td %>%
    extract(document, "year", "(\\d+)", convert = T) %>%
    complete(year, term, fill = list(count = 0)) %>%
    group_by(year) %>%
    mutate(year_total = sum(count))
 
year_term_counts %>%
    filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
    ggplot(aes(year, count / year_total)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ term, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format()) +
    ylab("% frequency of word in inagural address")
    
##############################################    
## 5.2 Casting tidy text data into a matrix
###############################################
# the reason why? 
# some algorithms expect such matrices as input

ap_td %>%
    cast_dtm(document, term, count) # cast tidy data back into DocumentTermMatrix

ap_td %>%
    cast_dfm(term, document, count) # cast tidy data back into Document-feature Matrix

# some tools require sparse matrix
library(Matrix)
m <- ap_td %>%
    cast_sparse(document, term, count) # cast tidy data into sparse Matrix
    
# Example: create DTM of Jane Austen's books    
library(janeaustenr)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)
  
  
 ## 5.3 tidying corpus objects with metadata
# corpus is data structures designed to store document collections before tokenization

data("acq")
acq_td <- tidy(acq) # use tidy() method to construct a table with one row per document, including the metadata as columns alongside the text

# example to find the most common words across the aricles
acq_tokens <- acq_td %>%
    select(-places) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word")

# most common words
acq_tokens %>%
    count(word, sort = TRUE)

# tf-idf
acq_tokens %>%
    count(id, word) %>%
    bind_tf_idf(word, id, n) %>%
    arrange(desc(tf_idf))
 
  
    

    
    
