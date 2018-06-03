library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)

# CH1 tidy text format###########################################################
# how to use unnest_tokens to reach the format 1-token-per-row
# 1.3 Tidying the works of Jane Austen
# tidying the works of Jan Austen

original_books <- austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           temp = str_detect(
               text,
               pattern = regex("^chapter [\\divxlc]", ignore_case = T)),        # to extract the chapter number
           chapter = cumsum(temp)
           ) %>%
    select(-temp)                                                               # remove temp column
    ungroup()

# count term frequnency
tidy_books <- original_books %>%
    unnest_tokens(output = word, input = text) %>%                              # tokenizations (1-token-per-row)
    anti_join(stop_words, by = "word")                                          # remove stopwords such as the, of, to, etc
                           

# visualize the most used word
tidy_books %>%
    count(word, sort = T) %>%                                                   # count the most used words list
    filter(n > 600) %>%
    mutate(word = reorder(word, n)) %>%                                         # reorder the levels of factor
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

# Word Frequencies
library(gutenbergr)
library(tidyr)
library(scales)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))
tidy_hgwells <- hgwells %>%
    unnest_tokens(output = word, input = text) %>%
    anti_join(stop_words)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
    unnest_tokens(output = word, input = text) %>%
    anti_join(stop_words)

frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells,author = "H.G. Wells"),
                       mutate(tidy_books,  author = "Jane Austen")
                       ) %>%
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(author, word) %>%
    group_by(author) %>%
    mutate(proportion = n / sum(n)) %>%                                         # calculate the word frequency
    select(-n) %>%
    spread(author, proportion) %>%
    gather(author, proportion, `Bronte Sisters`:`H.G. Wells`)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4",
                         high = "gray75") +
    facet_wrap(~author, ncol = 2) +
    theme(legend.position="none") +
    labs(y = "Jane Austen", x = NULL)

# how similar and different these sets of word frequencies
# word frequencies are more correlated between the austen and bronte novels 
# than between Austen and H.G. Wells
cor.test(data = frequency[frequency$author == "Bronte Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",],
         ~ proportion + `Jane Austen`)

# CH2 Sentiment Anaylysis with tidy data #########################################

# sentiment analysis reply on sentiment lexicon
##2.2 Sentiment analysis

# use NRC lexicon
nrcjoy <- get_sentiments("nrc") %>%
    filter(sentiment == "joy")

tidy_books %>%
    filter(book == "Emma") %>%
    inner_join(nrcjoy) %>%
    count(word, sort = T)

janeaustensentiment <- tidy_books %>%
    inner_join(get_sentiments("bing")) %>%                                      # use bing lexicon to analyze
    count(book, index = linenumber %/% 80, sentiment) %>%                       # operator %/% equal floor(x/y)
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)                                     # net sentiment

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
bing_and_nrc <- bind_rows(
    pride_prejudice %>%
        inner_join(get_sentiments("bing")) %>%
        mutate(method = "Bing et al."),
    pride_prejudice %>%
        inner_join(get_sentiments("nrc") %>% 
        filter(sentiment %in% c("positive", "negative"))) %>%
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
custom_stop_words <- bind_rows(
    data_frame(word = c("miss"),
               lexicon = c("custom")), 
    stop_words)

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
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%                      # Convert long format to wide format
    comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                     max.words = 100)

## 2.6 Looking at units beyond just words
PandP_sentences <- data_frame(text = prideprejudice) %>% 
    unnest_tokens(sentence, text, token = "sentences")

# the most sad chapter
austen_chapters <- austen_books() %>%
    group_by(book) %>%
    unnest_tokens(chapter, text, token = "regex", 
                  pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%                   # unnest by chapter
    ungroup()

austen_chapters %>%
    group_by(book) %>%
    summarise(chapters = n())                                                   # count the number of chapters by books

bingnegative <- get_sentiments("bing") %>%
    filter(sentiment == "negative")                                             # filter negative lexicon

wordcounts <- tidy_books %>%
    group_by(book, chapter) %>%
    summarize(words = n())

tidy_books %>%
    semi_join(bingnegative) %>%
    group_by(book, chapter) %>%
    summarize(negativewords = n()) %>%
    left_join(wordcounts, by = c("book", "chapter")) %>%
    mutate(ratio = negativewords/words) %>%
    filter(chapter != 0) %>%
    top_n(1) %>%
    ungroup()

# CH3 Analyzing Word and document Frequency#####################################
# CH3 Analyzing Word and document Frequency
## 3.1 Term frequency in Jane Austen's novels
book_words <- austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book,word, sort = T) %>%
    ungroup()



total_words <- book_words %>%                                                   # to count number of words of each book
    group_by(book) %>%
    summarise(total = sum(n))

book_words <- left_join(book_words, total_words)

ggplot(data = book_words, aes(n/total, fill = book)) +                          # n/total is the term frequency
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
    coord_flip()                                                                # flip the coordinate to convert column chart as bar chart

# visulize by novels

a <- plot_austen %>% 
    group_by(book) %>% 
    top_n(15) %>% 
    ungroup ()

plot_austen %>% 
    group_by(book) %>% 
    top_n(15) %>% 
    ungroup() %>%
    ggplot(aes(word, tf_idf, fill = book)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~book, ncol = 2, scales = "free") +
    coord_flip()

## 3.4 Acorpus of physics texts
physics <- gutenberg_download(c(37729, 14725, 13476, 5001), 
                              meta_fields = "author")

physics_words <- physics %>%
    unnest_tokens(word, text) %>%
    count(author, word, sort = TRUE) %>%
    ungroup()

plot_physics <- physics_words %>%
    bind_tf_idf(word, author, n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    mutate(author = factor(author, levels = c("Galilei, Galileo",
                                              "Huygens, Christiaan", 
                                              "Tesla, Nikola",
                                              "Einstein, Albert")))

plot_physics %>% 
    group_by(author) %>% 
    top_n(15, tf_idf) %>% 
    ungroup() %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(word, tf_idf, fill = author)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~author, ncol = 2, scales = "free") +
    coord_flip()

library(stringr)
physics %>% 
    filter(str_detect(text, "eq\\.")) %>% 
    select(text)

#CH4 Relationships between words: n-grams and correlations#####################

library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)

# 2 words
austen_bigrams <- austen_books() %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)

# to see if there are lots of common used stop words such as of the / to be
austen_bigrams %>%
    count(bigram, sort =  T)

# why use function tidyr::seperate 
bigrams_separated <- austen_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")

# filter stop words
bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

# count the combinations
bigram_count <- bigrams_filtered %>%
    count(word1, word2, sort = T)

bigrams_united <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ")

# analyze 3 consecutive words
austen_books() %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word) %>%
    count(word1, word2, word3, sort = T)

# A bigram can also be treated as a term, we can look at tf_idf afterthen
# visualize the tf_idf of bigrams
bigram_tf_df <- bigrams_united %>%
    count(book, bigram) %>%
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf_idf))

bigram_tf_df %>%
    mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
    group_by(book) %>%
    top_n(10) %>%
    ungroup() %>%
    ggplot(aes(x = bigram, y = tf_idf, fill = book)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~book, ncol = 2, scales = "free") +
    coord_flip()

# Using bigrams to provide context in sentiment analysis
bigrams_separated %>%
    filter(word1 == "not") %>%
    count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

not_words <- 
    bigrams_separated %>%
    filter(word1 == "not") %>%
    inner_join(AFINN, by = c(word2 = "word")) %>%
    count(word2, score, sort = T) %>%
    ungroup() %>%
    mutate(contribution = n * score) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*score, fill = n*score >0)) +
    geom_col(show.legend = F) +
    xlab("words preceded by \"not\"") +
    ylab("Sentiment score * number of occurrances") +
    coord_flip()

# not only not no presents negative sentiment
negation_words <-c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
    filter(word1 %in% c("not", "no", "never", "without")) %>%                   # to find the bigram with negated words
    inner_join(AFINN, by = c(word2 = "word")) %>%
    count(word1, word2, score, sort = TRUE) %>%
    ungroup() %>%
    mutate(contribution = n * score,
           word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%  # convert as the word2 as factor as sort factor in terms of contribution
    group_by(word1) %>%
    top_n(12, abs(contribution)) %>%                                            # choose top good and bottom bad
    ggplot(aes(word2, contribution, fill = n * score > 0)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ word1, scales = "free") +
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    xlab("Words preceded by negation term") +
    ylab("Sentiment score * # of occurrences") +
    coord_flip()

# visualizing a network of bigrams with ggraph
library(igraph)
library(ggraph)

bigram_graph <- bigram_count %>%
    filter(n > 20) %>%
    graph_from_data_frame()                                                     # create igraph object

set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.05, "inches"))

bigram_count %>%
    filter(n > 20) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +                                                     # ggraph need igraph object as input
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.04, 'inches')) +
    geom_node_point(color = "lightblue", size = 3) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()

# a function to visualize bigrams in other texts

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

# take example
kjv <- gutenberg_download(10)
kjv_bigrams <- kjv %>%
    count_bigrams()
kjv_bigrams %>%
    filter(n > 40,
           !str_detect(word1, "\\d"),
           !str_detect(word2, "\\d")) %>%
    visualize_bigrams()

# 4.2 Counting and correlating pairs of words with widyr package
austen_section_words <- austen_books() %>%
    filter(book == "Pride & Prejudice") %>%
    mutate(section = row_number() %/% 10) %>%                                   # divide into 10-line section
    filter(section > 0) %>%                                                     # filter top 9 lines
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word)

library(widyr)
word_pairs <- austen_section_words %>%
    pairwise_count(word, section, sort = TRUE)                                  # load widyr package first, count number of sections within which pair of words co-appearing

word_cors <- austen_section_words %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, section, sort = TRUE)

word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# visualize the correlations and cluster of words
set.seed(2016)
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation),  
                 arrow = arrow(length = unit(3, 'mm')), 
                 end_cap = circle(3, 'mm'),
                 show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# CH5 Converting to and from non-tidy formats###############################################################################

# most of the existing R tools for natural language processing, besides the tidytext package, 
# aren’t compatible with this tidy format
# DTM objects cannot be used directly with tidy tools
# tidy data frames cannot be used as input for most text mining packages

## 5.1 Tidying a document-term matrix
library(tm)
data("AssociatedPress", package = "topicmodels")                                # access the data AssocaitedPress from package topicmodels
terms <- Terms(AssociatedPress)

library(dplyr)
library(tidytext)

# tidying DTM objects
ap_td <- tidy(AssociatedPress)                                                  # convert DTM Objects AssociatedPress to tidy tbl_df objects
ap_sentiments <- ap_td %>%                                                      # use bing lexicon to implement sentiment analysis
    inner_join(get_sentiments("bing"), by = c(term = "word"))

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
    
# tidying dfm (document-feature matrix) objects
# tidytext::tidy()
data("data_corpus_inaugural", package = "quanteda")                             # access corpus data from package quanteda
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = F)                  # construct DFM objects
inaug_td <- tidy(inaug_dfm)                                                     # convert DMF objects to tidy data frames

inaug_tf_idf <- inaug_td %>%
    bind_tf_idf(term, document, count) %>%                                      # calculate TF-IDF
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

## 5.2 Casting tidy text data into a matrix
# the reason why? becasue some packages need DTM/DFM as input
ap_td %>%
    cast_dtm(document, term, count)                                             # construct tidy data into DTM objects

ap_td %>%
    cast_dfm(term, document, count)                                             # construct tidy data into DFM objects

# some tools require sparse matrix
library(Matrix)
m <- ap_td %>%
    cast_sparse(document, term, count)                                          # cast tidy data into sparse Matrix

# Example to create DTM
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

# example to mining financial articles
library(tm)
library(tm.plugin.webmining)
library(purrr)
Sys.setenv(https_proxy = "http://10.196.0.50:3128")

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX")

download_articles <- function(symbol) {
    WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}

stock_articles <- data_frame(company = company) %>%
    mutate(corpus = map(symbol, download_articles))                             

# CH6 Topic Modeling############################################################
library(topicmodels)
library(tidytext)
library(tidyverse)

data("AssociatedPress")
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))              # LDA need DTM as input
ap_topics <- tidy(ap_lda, matrix = "beta")                                      # per-topic-per-word probabilities, called β (“beta”)

ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

ap_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = F) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()

beta_spread <- ap_topics %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1))

ap_documents <- tidy(ap_lda, matrix = "gamma")                                  # per-document-per-topic, called γ (“gamma”)

tidy(AssociatedPress) %>%
    filter(document == 6) %>%
    arrange(desc(count))

# 6.2 Example: the great library heist
titles <- c("Twenty Thousand Leagues under the Sea", 
            "The War of the Worlds",
            "Pride and Prejudice", 
            "Great Expectations")

library(gutenbergr)
books <- gutenberg_works(title %in% titles) %>%
    gutenberg_download(meta_fields = "title")                                   # download books from gutenberg

library(stringr)

# divide into documents, each representing one chapter
by_chapter <- books %>%
    group_by(title) %>%
    mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
    ungroup() %>%
    filter(chapter > 0) %>%
    unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
    unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
    anti_join(stop_words) %>%
    count(document, word, sort = TRUE) %>%
    ungroup()

chapters_dtm <- word_counts %>%
    cast_dtm(document, word, n)                                                 # construct DTM objects

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))

chapter_topics <- tidy(chapters_lda, matrix = "beta")

top_terms <- chapter_topics %>%
    group_by(topic) %>%
    top_n(5, beta) %>%                                                          # find the top 5 terms within each topic
    ungroup() %>%
    arrange(topic, -beta)

top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()

# Per-document classification
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")

chapters_gamma <- chapters_gamma %>%
    separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma %>%
    mutate(title = reorder(title, gamma * topic)) %>%
    ggplot(aes(factor(topic), gamma)) +
    geom_boxplot() +
    facet_wrap(~ title)

chapter_classifications <- chapters_gamma %>%
    group_by(title, chapter) %>%
    top_n(1, gamma) %>%
    ungroup()

book_topics <- chapter_classifications %>%                                      
    count(title, topic) %>%                                                     # count number of chapters belongs within topics
    group_by(title) %>%
    top_n(1, n) %>%
    ungroup() %>%
    transmute(consensus = title, topic)

chapter_classifications %>%
    inner_join(book_topics, by = "topic") %>%
    filter(title != consensus)

assignments <- augment(chapters_lda, data = chapters_dtm)                       # assign the raw document to each topics

assignments <- assignments %>%
    separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
    inner_join(book_topics, by = c(".topic" = "topic"))

assignments %>%
    count(title, consensus, wt = count) %>%
    group_by(title) %>%
    mutate(percent = n / sum(n)) %>%
    ggplot(aes(consensus, title, fill = percent)) +
    geom_tile() +
    scale_fill_gradient2(high = "red", label = scales::percent_format()) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid = element_blank()) +
    labs(x = "Book words were assigned to",
         y = "Book words came from",
         fill = "% of assignments")

wrong_words <- assignments %>%
    filter(title != consensus)                                                  # what were the most commonly mistaken words?

wrong_words %>%
    count(title, consensus, term, wt = count) %>%
    ungroup() %>%
    arrange(desc(n))

word_counts %>%
    filter(word == "flopson")                                                   # "flopson" never appear in "Pride and Prejudice" but it's assigned to

# 6.3 Alternative LDA implementations
library(mallet)

# create a vector with one string per chapter
collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)


