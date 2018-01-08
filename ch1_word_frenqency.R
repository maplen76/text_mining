library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(ggthemes)

# how to use unnest_tokens to reach the format 1-token-per-row
# 1.3 Tidying the works of Jane Austen
# tidying the works of Jan Austen

original_books <- austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           temp = str_detect(text,
                             pattern = regex("^chapter [\\divxlc]",
                                                  ignore_case = T)),
           chapter = cumsum(temp)
           ) %>%
    ungroup()

# tokenizations (1-token-per-row)
tidy_books <- original_books %>%
    unnest_tokens(word, text)

# remove stop words such as the, of, to, etc
tidy_books <- tidy_books%>%
    anti_join(stop_words, by = "word")

# count the most used words list
tidy_books %>%
    count(word, sort = T)

# visualize the most used word
tidy_books %>%
    count(word, sort = T) %>%
    filter(n > 600) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()
