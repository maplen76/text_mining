library(tm.plugin.webmining)
library(purrr)

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX")

download_articles <- function(symbol) {
    WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}

stock_articles <- data_frame(company = company,
                             symbol = symbol) %>%
    mutate(corpus = map(symbol, download_articles))

stock_tokens <- stock_articles %>%
    unnest(map(corpus, tidy)) %>%
    unnest_tokens(word, text) %>%
    select(company, datetimestamp, word, id, heading)

library(stringr)

stock_tf_idf <- stock_tokens %>%
    count(company, word) %>%
    filter(!str_detect(word, "\\d+")) %>%
    bind_tf_idf(word, company, n) %>%
  arrange(-tf_idf)

stock_tokens %>%
    anti_join(stop_words, by = "word") %>%
    count(word, id, sort = TRUE) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(contribution = sum(n * score)) %>%
    top_n(12, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    ggplot(aes(word, contribution)) +
    geom_col() +
    coord_flip() +
    labs(y = "Frequency of word * AFINN score")

stock_tokens %>%
    count(word) %>%
    inner_join(get_sentiments("loughran"), by = "word") %>%
    group_by(sentiment) %>%
    top_n(5, n) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ sentiment, scales = "free") +
    ylab("Frequency of this word in the recent financial articles")

stock_sentiment_count <- stock_tokens %>%
    inner_join(get_sentiments("loughran"), by = "word") %>%
    count(sentiment, company) %>%
    spread(sentiment, n, fill = 0)

stock_sentiment_count %>%
    mutate(score = (positive - negative) / (positive + negative)) %>%
    mutate(company = reorder(company, score)) %>%
    ggplot(aes(company, score, fill = score > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(x = "Company",
         y = "Positivity score among 20 recent news articles")
