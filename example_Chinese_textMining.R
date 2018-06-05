library(jiebaR)
library(tidyverse)
library(xlsx)
Sys.setlocale(category = "LC_ALL", locale = "chs")
separator <- worker()

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




# read reivew file #
reviews <- read.xlsx(file = "F:\\PoC\\PoCRawData\\fh_output+20180522_2\\Review_Level.xlsx",
                     sheetName = "Sheet1",
                     stringsAsFactors = FALSE,
                     encoding = "UTF-8")

reviews_tbl <- reviews %>% 
    tbl_df() %>%
    mutate(review_new = str_replace_all(string = review,                        # remove emoji
                                        pattern = '\\p{So}|\\p{Cn}', 
                                        replacement = '')) %>%
    filter(nchar(review_new, type = "bytes") > 2)                             # remove review with 2 bytes 

# user jiebaR to split words
for (i in 1:nrow(reviews_tbl)) {
    a <- paste0(separator <= reviews_tbl$review_new[i], collapse = " ")
    reviews_tbl$text[i] <- a
    
    if (i == nrow(reviews_tbl)) {
        print("finsh")
    }
}

# unnest_tokens support chinese not as expected
# build special tokenizer function to unnest
tok99 = function(t) str_split(t,"[ ]{1,}")
review_words <- reviews_tbl %>%
    unnest_tokens(word, text, token=tok99)

review_words <- review_words %>%
    select(-1, -(playtime:upvote))

# read stopwords 
stopWords_CN <- readLines('stopwords-zh.txt', encoding = "UTF-8")
review_words <- review_words %>%
    filter(!word %in% stopWords_CN) %>%
    filter(!word == "") %>%
    filter(!word %in% stop_words$word)

# 
wds <-review_words %>%
    count(review_id,word, sort = T) %>%
    arrange(word)

review_words_count <- review_words %>%
    count(review_id, word, sort = TRUE)
    
review_dtm <- review_words_count %>%
    cast_dtm(review_id, word, n)

library(topicmodels)
review_lda <- LDA(review_dtm, k = 3, control = list(seed = 1234))

review_topic <- tidy(review_lda, matrix = "beta")











