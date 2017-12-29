library(RSelenium)
shell.exec(paste0("D:/Rselenium/StartRseleniumFirefox.bat")) # start local selenium server
Sys.sleep(5)

# have to install the lastest Rtools from https://cran.r-project.org/bin/windows/Rtools/ at first
# shut down R and restart computer
fprof <- makeFirefoxProfile(list("network.proxy.type" = 4L)) # set the firefox connection setting as Auto-detext proxy settins
remDr <- remoteDriver(extraCapabilities = fprof)
remDr$open()

remDr <- remoteDriver(browserName = "firefox") # only IE internet explorer works fine
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

page_list <- c()
for (i in 1:ceiling(nb_reviews/10)) {
    left <- i*10 - 9
    right <- ifelse(i*10 > nb_reviews, nb_reviews, i*10)
    page_list <- c(page_list, paste0(left,"-",right))
}

path_review <- '//*[@id="reviewsPagingSection"]/div[3]/div['
path_user <-    ']/div[1]/p[2]'
path_date <-    ']/div[1]/p[1]'
path_star <-    ']/div[1]/div/p/span[1]'
path_title <-   ']/div[2]/div[1]/h5'
path_detail <-  ']/div[2]/div[1]/div[1]/p[1]'
path_helpful <- ']/div[2]/div[2]/p'

review_attr <- c("user", "date", "star", "title", "detail", "helpful")

# ceiling(nb_reviews/10)
review_table  <- data.frame()

for (i in 1:ceiling(nb_reviews/10)) {
    
    pg <- remDr$findElement(using = 'class', "context-pagination")$getElementText() %>%
        as.character() %>%
        str_extract_all(pattern ='\\d+-\\d+') %>%
        as.character()
    
    print(pg)
    
    # identify number of reviews on current page
    nb_reviews_page <- length(remDr$findElements(using = "class", "cli_review"))
    
    for (j in 1:nb_reviews_page) {
        
        review_id <- as.character(j)
        print(paste0(pg,": ",review_id))
        
        # extract value of each attribute
        for (i in review_attr) {
            
            metric_review_xxx <- paste0("review_", i)
            path_attr <- eval(parse(text = paste0("path_",i)))
            
            if (i == "star") {
                # use getElementAttribute("textContent") to get invisible text
                value <-  remDr$findElement(using = 'xpath', paste0(path_review,review_id,path_attr))$getElementAttribute("textContent") %>%
                    as.numeric()
                assign(metric_review_xxx, value)
            } else {
                value <- remDr$findElement(using = 'xpath', paste0(path_review,review_id,path_attr))$getElementText() %>%
                    as.character()
                assign(metric_review_xxx, value)        
            }
        }
        
        # create data frame to store all attributes
        comments <- data.frame(page = pg,
                               user = review_user,
                               date = review_date,
                               star = review_star,
                               title = review_title,
                               detail = review_detail,
                               helpful = review_helpful,
                               stringsAsFactors = F
        )
        
        # store reviews into data frame review_table
        review_table <- rbind.data.frame(review_table, comments)
    }
    
    next_button <- remDr$findElement(using = 'css selector', value = "#reviewsPageNextAnchor")
    
    # injecting JavaScrript to scroll the page down to the next_buton location
    script <- 'arguments[0].scrollIntoView(true)'
    remDr$executeScript(script = script,args = list(next_button) )
    
    # alternate method to scroll page to bottom
    # top when key = "home"; scroll just a bit when key = "down_arrow"
    # next_button$sendKeysToElement(list(key = "end"))
    
    remDr$mouseMoveToLocation(webElement = next_button)
    remDr$click(buttonId = 0)
    Sys.sleep(30)
}

print(review_table)

# close the server/client
remDr$close()



