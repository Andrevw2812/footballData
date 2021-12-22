library(tidyverse)
library(rvest)
library(RSelenium)
library(gt)
# Useful Link: http://joshuamccrain.com/tutorials/web_scraping_R_selenium.html

# Get FS Odds for the day

landingPageUrl <- "https://www.flashscore.com/"

matchLineSel <- ".event__match--oneLine"
oddsTabSel <- "#live-table > div.tabs > div.tabs__group > div:nth-child(3) > div"


rm(rD, remDr)

rD <- rsDriver(port = netstat::free_port(), browser = "firefox")
remDr <- rD$client

remDr$navigate(landingPageUrl)
Sys.sleep(5)
oddsTab <- remDr$findElement('css', oddsTabSel)
oddsTab$clickElement()

# Get Page Source
landingPageSource <- remDr$getPageSource()
landingPageSourceA <- unlist(landingPageSource)
remDr$closeall()
matchesDetails <- read_html(landingPageSourceA) %>% html_elements(matchLineSel) 

# Get 80 matches
matchesDetailsId <- matchesDetails[1:80] %>% html_attr('id') %>% str_sub(., 5, -1)
matchesUrls <- paste0("https://www.flashscore.com/match/", matchesDetailsId, "/")
matchesDetailsData <- matchesDetails[1:80] %>% html_text2()
matchesDetailsData <- matchesDetailsData %>% str_replace_all("\n-\n", "\n")
matchesDetailsDataAll <- as.data.frame(matchesDetailsData) %>% separate(., col = matchesDetailsData ,into = c("Time", "Home", "Away", "HOdds", "DOdds", "AOdds"), sep="\n") 

matchesTable <- cbind(matchesDetailsDataAll, matchesDetailsId, matchesUrls)

matchesTable <- matchesTable %>% filter(Home!="FRO") %>% filter(Time!="Finished") %>% filter(DOdds!="-") %>% filter(!is.na(as.double(HOdds)))
matchesTable$HOdds <- as.double(matchesTable$HOdds)
matchesTable$DOdds <- as.double(matchesTable$DOdds)
matchesTable$AOdds <- as.double(matchesTable$AOdds)

matchesTable$HObservation <- if_else(matchesTable$HOdds<=1.8,"Home Odds On","")

matchesTable$AObservation <- if_else(matchesTable$AOdds<=1.8,"Away Odds On","")
matchesTable$EObservation <- if_else(matchesTable$HObservation==""&matchesTable$AObservation=="", "Possible Even Match", "")

matchesTable$Observations <- paste0(matchesTable$HObservation, matchesTable$AObservation, matchesTable$EObservation)

matchesTable <- matchesTable %>% select(Time, Home, Away, HOdds, DOdds, AOdds, matchesDetailsId, matchesUrls, Observations) %>% arrange(Time)

matchesTableGT <- matchesTable %>% select(Time, Home, Away, HOdds, DOdds, AOdds, Observations) %>% gt() %>% tab_header(paste0("Matches for ", Sys.Date())) %>% tab_options(table.width = "90%") %>% cols_align(align = "center", everything()) %>% opt_row_striping()


rm(landingPageSource, landingPageSourceA)
