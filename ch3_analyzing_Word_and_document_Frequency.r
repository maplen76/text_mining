library(dplyr)
library(janeaustenr)
library(tidytext)

## 3.1 Term frequency in Jane Austen's novels

book_words <- austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book,word, sort = T) %>%
    ungroup() %>%
    group_by(book) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    group_by(book) %>%
    arrange(book,desc(n)) %>%
    ungroup()

ggplot(data = book_words, aes(n/total, fill = book)) +
    geom_histogram(show.legend = F) +
    xlim(NA,0.0009) +
    facet_wrap(~book, ncol = 2, scales = "free_y")

## 3.2 Zipf's law
#  the frequency that a word appears is inversely proportional to its rank.
freq_by_rank <- book_words %>% 
    group_by(book) %>% 
    mutate(rank = row_number(), 
           `term frequency` = n/total)

freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`, color = book)) + 
    geom_line(size = 1.2, alpha = 0.8) + 
    scale_x_log10() +
    scale_y_log10()

freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`, color = book)) + 
    geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
    geom_line(size = 1.2, alpha = 0.8) + 
    scale_x_log10() +
    scale_y_log10()

## 3.3 The bind_tf_idf function
# tf_idf measure ther weight of importance of some term

# use bind_tf_idf function to get tf_idf
book_words <- book_words %>%
    bind_tf_idf(word,book,n)

# look the high tf_idf
book_words %>%
    select(-total) %>%
    arrange(desc(tf_idf))

# visualize high tf_idf by books
plot_austen <- book_words %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))

plot_austen %>%
    top_n(20) %>%
    ggplot(aes(word, tf_idf,fill = book)) +
    geom_col() +
    labs(x = NULL, y = "tf_idf") +
    coord_flip() # flip the coordinate to convert column chart as bar chart

# visulize by novels

a <- plot_austen %>% 
    group_by(book) %>% 
    top_n(15) %>% 
    ungroup

plot_austen %>% 
    group_by(book) %>% 
    top_n(15) %>% 
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = book)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~book, ncol = 2, scales = "free") +
    coord_flip()

## 3.4 Acorpus of physics texts


######################################################################################################
## chapter 3 summary
######################################################################################################
library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book, word, sort = TRUE) %>%
    ungroup()

total_words <- book_words %>% 
    group_by(book) %>% 
    summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words <- book_words %>%
    bind_tf_idf(word, book, n)

plot_austen <- book_words %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))

# these high tf-idf words
plot_austen %>% 
    top_n(20) %>%
    ggplot(aes(word, tf_idf, fill = book)) +
    geom_col() +
    labs(x = NULL, y = "tf-idf") +
    coord_flip()

# novels individually
plot_austen %>% 
    group_by(book) %>% 
    top_n(15) %>% 
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = book)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~book, ncol = 2, scales = "free") +
    coord_flip()
