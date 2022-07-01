### This file will contain your main code.
### Feel free to rename it, or split it into several files.
###
### Your final product should contain the code along the following lines:
### the main part of it should be a function (e.g. 'repTable') that takes
### address as the argument, and returns a markdown table of representatives.

##    ---------- Google Civic Platform ----------
## 1. create the google civic platform request and httr::GET() the result
##    you need to include your api key in the request.  See the documentation
##    https://developers.google.com/civic-information/
##    in particular the reference section.
##    There is also console where you can experiment with requests and see what do
##    these return.
##
##    Note: you can submit the requests through your browser.  If unsure, or if
##    httr::GET gives you an error, you may always put the address in your browser's
##    address bar.  If correct, it will display the corresponding JSON data.  If
##    incorrect, you get an error message.

## 2. extract the elected officials' data from the result
##    The data contains many relevant variables, including normalized address,
##    'offices' and 'officials'.  In order to attach the officials (people)
##    with offices (jobs), I recommend to use dplyr joins (what would be the key?)
##    More about joins in
##    http://r4ds.had.co.nz/relational-data.html

## 3. transform the data into a well formatted table
##    I recommend you transform the data into markdown strings.  For instance,
##    to display a html link as a link in the markdown file, you may want to
##    embed it between "[](" and ")".  You may format emails as
##    "[john@example.com](mailto:john@example.com)" to make these into a link.

library("httr")
library("jsonlite")
library("dplyr")
library("tidyr")

source("~/Documents/keys.R")

getReps <- function(address) {
  base <- "https://www.googleapis.com/civicinfo/v2/representatives"
  
  query <- list(address=address, key=google.key)
  
  res <- GET(base, query=query)
  content(res)
  
  data <- content(res, "text") %>%
    fromJSON()
  
  offices <- data.frame(data$offices) %>%
    select(name, officialIndices) %>%
    unnest() %>%
    rename(position = name)
  
  officials <- data.frame(data$officials)
  officials <- officials %>% 
    select(-address, -channels) %>%
    mutate(officialIndices = 0:(nrow(officials)-1))
  
  joined <- right_join(offices, officials, by="officialIndices") %>%
    select(-officialIndices)
  
  finalTable <- joined %>%
    replace(is.na(joined), "-") %>%
    replace(joined == "NULL", "-")
  
  finalTable$photoUrl <- ifelse(finalTable$photoUrl != "-", 
                                paste0("![](", finalTable$photoUrl, ")"), 
                                finalTable$photoUrl)
  
  finalTable$emails <- ifelse(finalTable$emails != "-", 
                              paste0("[", finalTable$emails, "](mailto:", 
                                     finalTable$emails, ")"), finalTable$emails)
  
  finalTable$name <- ifelse(finalTable$urls != "-", 
                            paste0("[", finalTable$name, "](", finalTable$urls, ")"), 
                            finalTable$name)
  
  finalTable <- finalTable %>%
    select(-urls) %>%
    rename(photo = photoUrl)
  
  return(finalTable)
}
