# how to use unnest_tokens
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

library(dplyr)
text_df <- data_frame(line = 1:4, text = text)

library(tidytext)

text_df %>%

unnest_tokens(tbl = text_df, output = word, input = text, token = "lines")
    
    unnest_tokens(word,text)

# 1.3 Tidying the works of Jane Austen
# tidying the works of Jan Austen
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)

original_books <- austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           temp = str_detect(text,
                             pattern = regex("^chapter [\\divxlc]",
                                                  ignore_case = T)),
           chapter = cumsum(temp)
           ) %>%
    ungroup()

# tokenizations
tidy_books <- original_books %>%
    unnest_tokens(word, text)

# remove stop words such as  “the”, “of”, “to”, etc
tidy_books <- tidy_books%>%
    anti_join(stop_words, by = "word")

# count the most used words list
tidy_books %>%
    count(word, sort = T)

# visualize the most used word
library(ggplot2)

tidy_books %>%
    count(word, sort = T) %>%
    filter(n > 600) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

# 1.4 the gutenbergr package
install.packages("gutenbergr")


# 1.5 Word Frequencies
library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))
