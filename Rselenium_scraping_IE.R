# download web drivers from https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-saucelabs.html#id1aa
# download selenium stand alone driver from https://goo.gl/hvDPsK or http://selenium-release.storage.googleapis.com/index.html

# there is another way to start Rselenium server, but need to connect gaming gateway at first
# rD <- rsDriver(verbose = T, browser = "internet explorer")
# remDr <- rD$client
setwd('F:\\Console_WOF\\RApps\\wof_store_rating')

library(dplyr, verbose = F)
library(stringr)
library(XML)
library(rvest, verbose = F)
library(RSelenium)

# IE solution
# have to cd the location where drivers stored when create bat file
shell.exec(paste0("D:/Rselenium/StartRseleniumIE.bat")) # start local selenium server
Sys.sleep(5)

remDr <- remoteDriver(browserName = "internet explorer") # only IE internet explorer works fine
remDr$open()
url_store <- "https://www.microsoft.com/en-us/p/wheel-of-fortune/br76vbtv0nk0?activetab=pivot:reviewstab"
remDr$navigate(url_store)

remDr$mouseMoveToLocation(x = 1,y = 1)
remDr$click(buttonId = 0)
remDr$click(buttonId = 0)
remDr$click(buttonId = 0)
remDr$click(buttonId = 0)
remDr$click(buttonId = 0)

print('scrape overall Ratings and Reviews')

# 20181210 old path //*[@id="ratings-reviews"]/div[1]/div[1]/div/span
Rating_xbox_store <- 
    remDr$findElement('xpath','//section[3]/div/div/div/div/div/span')$getElementText() %>%
    as.numeric()

# 20181210 old path //*[@id="ratings-reviews"]/div[1]/div[1]/div/div/div/span
nb_ratings_players <- 
    remDr$findElement('xpath','//section/div/div/div/div/div/div/span')$getElementText() %>%
    as.numeric()

print('Scrape reviews distribution')
star_a <-'//div[2]/div/div/ul/li[' # old path //*[@id="ratings-reviews"]/div[1]/div[1]/ul/li[
star_b <- ']/a'

# per_a <- '//*[@id="react_8eBZBXM6AUKCxpOW8s1iQ"]/div/ul/li[' # old path //*[@id="ratings-reviews"]/div[1]/div[1]/ul/li[
# per_b <- ']/a/div/div/div'

rating_df <- data.frame()

for (i in 1:5) {
    star_xpath <-  paste0(star_a,i,star_b)
#   percentage_xpath <- paste0(per_a,i,per_b)
    
    star_rate <- remDr$findElement('xpath', star_xpath)$getElementText() 
    
    star <-  star_rate %>% str_extract("[1-9]") %>% as.numeric()
    percentage <- star_rate %>% str_extract(".[0-9]+") %>% as.numeric()
       
 #   percentage <- remDr$findElement('xpath', percentage_xpath)$getElementText() %>%
 #       as.character() %>%
 #       str_replace('%', '') %>%
 #       as.numeric()
    
    df <- data.frame(star = star, percentage = percentage)
    rating_df <- rbind.data.frame(rating_df, df)
    print(rating_df)
}

# pagination
nb_reviews_heading <- remDr$findElement(using = 'xpath', "/html/body/section/section/section[3]/div/div[4]/div/div/div[1]/h2")$getElementText() %>%
    as.character() %>%
    str_extract_all(pattern ='[0-9]+')

nb_reviews <- nb_reviews_heading[[1]][3] %>% as.numeric()

page_list <- c()
for (i in 1:ceiling(nb_reviews/10)) {
    left <- i*10 - 9
    right <- ifelse(i*10 > nb_reviews, nb_reviews, i*10)
    page_list <- c(page_list, paste0(left,"-",right))
}

 path_review <-  '/html/body/section/section/section[3]/div/div[4]/div/div/div[4]/div[' # old xpath header before 4/19/2018: '//*[@id="reviewsPagingSection"]/div[3]/div['
 path_user <-    ']/div/div[1]/h3'
 path_date <-    ']/div/div[2]/div[2]/span[2]'
 path_star <-    ']/div/div[2]/div[1]/div'
 path_title <-   ']/div/div[2]/h3'
 path_detail <-  ']/div/div[2]/p' # old xpath header before 4/19/2018: ']/div[2]/div[1]/div[1]/p[1]'
 path_helpful <- ']/div/div[2]/div[3]/div'

review_attr <- c("user", "date", "star", "title", "detail", "helpful")
 
# ceiling(nb_reviews/10)
review_table  <- data.frame()

for (i in 1:ceiling(nb_reviews/10)) {
    
    pg <- remDr$findElement(using = 'xpath', "/html/body/section/section/section[3]/div/div[4]/div/div/div[1]/h2")$getElementText() %>%
        as.character() 
    print(pg)
    # identify number of reviews on current page
    nb_reviews_page_top <- str_extract_all(string = pg, pattern ='[0-9]+')[[1]][1] %>% as.numeric()
    nb_reviews_page_bottom <- str_extract_all(string = pg, pattern ='[0-9]+')[[1]][2] %>% as.numeric()
    nb_reviews_page <- length(nb_reviews_page_top:nb_reviews_page_bottom)
    
    for (j in 1:nb_reviews_page) {
        
        review_id <- as.character(j)
        print(paste0(pg,": ",review_id))
        
        # extract value of each attribute
        for (i in review_attr) {
            
            metric_review_xxx <- paste0("review_", i)
            path_attr <- eval(parse(text = paste0("path_",i)))
            
            if (i == "star") {
                # use getElementAttribute("textContent") to get invisible text
                value <-  remDr$findElement(using = 'xpath', paste0(path_review,review_id,path_attr))$getElementText() %>%
                    str_extract('[1-5]') %>%
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
    
    next_button <- remDr$findElement(using = 'xpath', value = "/html/body/section/section/section[3]/div/div[4]/div/div/div[5]/ul/li[2]/button/span[1]")
    
    # injecting JavaScrript to scroll the page down to the next_buton location
    script <- 'arguments[0].scrollIntoView(true)'
    remDr$executeScript(script = script,args = list(next_button))
    
    # alternate method to scroll page to bottom
    # top when key = "home"; scroll just a bit when key = "down_arrow"
    # next_button$sendKeysToElement(list(key = "end"))
    
    remDr$mouseMoveToLocation(webElement = next_button)
    remDr$doubleclick(buttonId = 0)
    Sys.sleep(15)
}

# close the server/client
remDr$close()

# convert string to date
review_table <- review_table %>%
    mutate(date = as.Date(date, "%m/%d/%Y"),
           title = str_replace(title, pattern = "Review title of ", replacement = ""),
           user = str_replace(user, pattern = "Reviewed By", replacement = "")
           ) %>%
    arrange(desc(date))

for (i in 1:5) {
    metric <- paste0("most_recent_", as.character(i))
    assign(metric, review_table$detail[i])
}

scraping_time <- Sys.time() %>% as.character()
system('taskkill /fi "WindowTitle eq taskeng.exe"')                             # close the CMD window



