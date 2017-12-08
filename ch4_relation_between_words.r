library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)


count_bigrams <- function(dataset) {
    dataset %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word) %>%
        count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
    set.seed(2016)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    bigrams %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()
}


