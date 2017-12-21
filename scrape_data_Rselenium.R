
# manually start selenium server
# D:\software>java -Dwebdriver.gecko.driver="D:\software\geckodriver.exe" -jar selenium-server-standalone-3.8.1.jar
# D:\software>java -Dwebdriver.gecko.driver="D:\software\chromedriver.exe" -jar selenium-server-standalone-3.8.1.jar

# D:\software>java -Dwebdriver.gecko.driver="D:\software\IEDriverServer.exe" -jar selenium-server-standalone-3.8.1.jar
# IE is available finally

library(dplyr)
library(RSelenium)
library(stringr)
library(XML)
library(rvest)

# connect local server
remDr <- remoteDriver(browserName = "internet explorer")
remDr$open()
remDr$navigate("https://www.microsoft.com/en-us/store/p/wheel-of-fortune/br76vbtv0nk0")

# scrape overall Ratings and Reviews
Rating_xbox_store <- 
    remDr$findElement('xpath','//*[@id="ratings-reviews"]/div[1]/div[1]/div/span')$getElementText() %>%
    as.numeric()

nb_ratings_players <- 
    remDr$findElement('xpath','//*[@id="ratings-reviews"]/div[1]/div[1]/div/div/div/span')$getElementText() %>%
    as.numeric()

# Scrape reviews distribution
star_a <-'//*[@id="ratings-reviews"]/div[1]/div[1]/ul/li['
star_b <- ']/a/span[1]'

per_a <- '//*[@id="ratings-reviews"]/div[1]/div[1]/ul/li['
per_b <- ']/a/div/div/span'

rating_df <- data.frame()

for (i in 1:5) {
    star_xpath <-  paste0(star_a,i,star_b)
    percentage_xpath <- paste0(per_a,i,per_b)
    
    star <- remDr$findElement('xpath', star_xpath)$getElementText() %>%
        as.numeric()
    
    percentage <- remDr$findElement('xpath', percentage_xpath)$getElementText() %>%
        as.character() %>%
        str_replace('%', '') %>%
        as.numeric()
    
    df <- data.frame(star = star, percentage = percentage)
    rating_df <- rbind.data.frame(rating_df, df)
    print(rating_df)
}


# pagination
nb_reviews <- remDr$findElement(using = 'class', "context-pagination")$getElementText() %>%
    as.character() %>%
    str_extract_all(pattern =' \\d+ ') %>%
    as.numeric()

 path_review <- '//*[@id="reviewsPagingSection"]/div[3]/div['
 path_review_user <-    ']/div[1]/p[2]'
 path_review_date <-    ']/div[1]/p[1]'
 path_review_star <-    ']/div[1]/div/p/span[1]'
 path_review_title <-   ']/div[2]/div[1]/h5'
 path_review_detail <-  ']/div[2]/div[1]/div[1]/p[1]'
 path_review_helpful <- ']/div[2]/div[2]/p'
 
# ceiling(nb_reviews/10)

for (i in 1:ceiling(nb_reviews/10)) {
    pg <- remDr$findElement(using = 'class', "context-pagination")$getElementText() %>%
        as.character() %>%
        str_extract_all(pattern ='\\d+-\\d+') %>%
        as.character()
    
    j <- 1 # identifier of each reviews on each page (each page have at most 10 reviews)
    
    repeat {
        
        review_id <- as.character(j)
        
        review_user <- remDr$findElement(using = 'xpath', paste0(path_review,review_id,path_review_user))$getElementText() %>%
            as.character()
        
        review_date <-  remDr$findElement(using = 'xpath', paste0(path_review,review_id,path_review_date))$getElementText() %>%
            as.character()
        
        # use getElementAttribute("textContent") to get invisible text
        review_star <-  remDr$findElement(using = 'xpath', paste0(path_review,review_id,path_review_star))$getElementAttribute("textContent") %>%
            as.numeric()
        
        review_title <- remDr$findElement(using = 'xpath', paste0(path_review,review_id,path_review_title))$getElementText() %>%
            as.character()
        
        review_detail <-  remDr$findElement(using = 'xpath', paste0(path_review,review_id,path_review_detail))$getElementText() %>%
            as.character()
        
        review_helpful <-  remDr$findElement(using = 'xpath', paste0(path_review,review_id,path_review_helpful))$getElementText() %>%
            as.character()
        
        comments <- data.frame(page = pg,
                               user = review_user, 
                               date = review_date, 
                               star = review_star,
                               title = review_title,
                               detail = review_detail,
                               helpful = review_helpful,
                               stringsAsFactors = F
        )
        
        review_table <- rbind.data.frame(review_table, comments)
        
        j <- j + 1
        
        if (length(review_user) == 0 | j > 10) {
            break
        }
    }
    
    # navigte to next page
    next_button <- remDr$findElement(using = 'id', value = "reviewsPageNextAnchor")
    remDr$mouseMoveToLocation(webElement = next_button)
    remDr$click(1)
    remDr$sendKeysToActiveElement(list(key = 'down_arrow', key = 'down_arrow', key = 'enter'))
    
    Sys.sleep(20) # wait 20 seconds to loading next page
    
}


# close the server/client
remDr$close()

