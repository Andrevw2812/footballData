library(tidyverse)
library(RSelenium)
library(rvest)
library(gt)

allTeamsFS
allLeaguesFS

rD <- rsDriver(port = netstat::free_port(), browser = "firefox")
remDr <- rD$client

remDr$navigate(allLeaguesFS$fixtureUrls[21])

fixturePgSrc <- remDr$getPageSource()
fixtureMatchId <- fixturePgSrc %>% unlist() %>% read_html() %>% html_elements(".event__match--twoLine") %>% html_attr('id') %>% str_sub(5, -1)
fixtureDetails <- fixturePgSrc %>% unlist() %>% read_html() %>% html_elements(".event__match--twoLine") %>% html_text2()

fixturesTbl <- as.data.frame(fixtureDetails) %>% separate(., col = fixtureDetails, into = c("DateTime", "HomeTeam", "AwayTeam", "NA", "NA1"), sep = "\n")

fixturesTbl <- fixturesTbl %>% select("DateTime", "HomeTeam", "AwayTeam")

fixtureUrl <- paste0("https://www.flashscore.com/match/", fixtureMatchId,"/#match-summary/match-summary")
fixtureOddsUrl <- paste0("https://www.flashscore.com/match/",fixtureMatchId,"/#odds-comparison/1x2-odds/full-time")


fixturesTbl <- cbind(fixturesTbl, fixtureUrl,fixtureOddsUrl, fixtureMatchId)


japanJOneLeagueFixturesFS <- fixturesTbl
head(japanJOneLeagueFixturesFS)

remDr$closeall()
rm(remDr, rD)
