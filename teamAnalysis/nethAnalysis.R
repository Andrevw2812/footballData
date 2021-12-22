library(tidyverse)
library(gt)


# NethTeamName <- ""
NethTeamNameRating <- srsTableneth %>% filter(., nethTeam == NethTeamName)
NethTeamNameRating <- as.double(NethTeamNameRating$teamRating)

nethTeamH.matchData <- function(NethTeamName) {
  nethTeamData <- filter(neth, neth$HomeTeam == NethTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  nethHTeamW <- nethTeamData %>% filter(FTR == "H") %>% nrow()
  nethHTeamD <- nethTeamData %>% filter(FTR == "D") %>% nrow()
  nethHTeamL <- nethTeamData %>% filter(FTR == "A") %>% nrow()
  nethHWDL <<- paste0(nethHTeamW," - ", nethHTeamD, " - ", nethHTeamL)
  
}
nethHData <- nethTeamH.matchData(NethTeamName)


nethTeamA.matchData <- function(NethTeamName) {
  nethTeamData <- filter(neth, neth$AwayTeam == NethTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  nethATeamW <- nethTeamData %>% filter(FTR == "A") %>% nrow()
  nethATeamD <- nethTeamData %>% filter(FTR == "D") %>% nrow()
  nethATeamL <- nethTeamData %>% filter(FTR == "H") %>% nrow()
  nethAWDL <<- paste0(nethATeamW," - ", nethATeamD, " - ", nethATeamL)
  
}
nethAData <- nethTeamA.matchData(NethTeamName)

nethHomeTeamData <- filter(neth, neth$HomeTeam == NethTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
nethAwayTeamData <- filter(neth, neth$AwayTeam == NethTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

nethHomeTeamData$FTR[nethHomeTeamData$FTR == "H"] <- "W"
nethHomeTeamData$FTR[nethHomeTeamData$FTR == "A"] <- "L"
nethAwayTeamData$FTR[nethAwayTeamData$FTR == "H"] <- "L"
nethAwayTeamData$FTR[nethAwayTeamData$FTR == "A"] <- "W"

nethHomeTeamData <- nethHomeTeamData %>% arrange(desc(Date))
nethAwayTeamData <- nethAwayTeamData %>% arrange(desc(Date))

nethHDataTbl <- gt(nethHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", NethTeamName)) %>%
  tab_options(table.width = "85%") %>%
  tab_style(
    style = list(cell_fill(color = "seagreen"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      rows = FTR=="W")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "red"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      rows = FTR=="L")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "orange"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      rows = FTR=="D")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "white"),
                 cell_text(color = "black", weight = "normal")),
    locations = cells_body(
      columns = Date)
  )

nethADataTbl <- gt(nethAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", NethTeamName)) %>%
  tab_options(table.width = "85%") %>%
  tab_style(
    style = list(cell_fill(color = "seagreen"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      rows = FTR=="W")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "red"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      rows = FTR=="L")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "orange"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      rows = FTR=="D")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "white"),
                 cell_text(color = "black", weight = "normal")),
    locations = cells_body(
      columns = Date)
  )

nethTeamTen.matchData <- function(NethTeamName) {
  nethTeamDataTen <- filter(neth, HomeTeam==NethTeamName|AwayTeam==NethTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  nethTeamDataTen
}
nethTeamDataTen <- nethTeamTen.matchData(NethTeamName)

nethTeamDataTenH <- nethTeamDataTen %>% filter(HomeTeam==NethTeamName)
nethTeamDataTenA <- nethTeamDataTen %>% filter(AwayTeam==NethTeamName)

nethTeamDataTenH$FTR[nethTeamDataTenH$FTR == "H"] <- "W"
nethTeamDataTenH$FTR[nethTeamDataTenH$FTR == "A"] <- "L"
nethTeamDataTenA$FTR[nethTeamDataTenA$FTR == "H"] <- "L"
nethTeamDataTenA$FTR[nethTeamDataTenA$FTR == "A"] <- "W"

nethTeamDataTen <- rbind(nethTeamDataTenH, nethTeamDataTenA) %>% arrange(desc(Date))



nethLastTenTbl <- gt(nethTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", NethTeamName)) %>%
  tab_options(table.width = "85%") %>%
  tab_style(
    style = list(cell_fill(color = "seagreen"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      rows = FTR=="W")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "red"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      rows = FTR=="L")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "orange"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      rows = FTR=="D")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "white"),
                 cell_text(color = "black", weight = "normal")),
    locations = cells_body(
      columns = Date)
  )

nethTeamH.goalData <- function(NethTeamName) {
  nethHTeamGoals <- filter(neth, neth$HomeTeam == NethTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  nethHTeamGoalsFirstH <- nethHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  nethHTeamGoalsFirstH <- as.character(nethHTeamGoalsFirstH) %>% paste0(., "%")
  nethHTeamGoalsSecondH <- as.data.frame(nethHTeamGoals$FTHG - nethHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  nethHTeamGoalsSecondH <- as.character(nethHTeamGoalsSecondH) %>% paste0(., "%")
  nethHomeGoalData <<- data.frame(nethHTeamGoalsFirstH, nethHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(nethHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(nethHomeGoalData)
}
nethHGoalData <- nethTeamH.goalData(NethTeamName)

nethTeamA.goalData <- function(NethTeamName) {
  nethATeamGoals <- filter(neth, neth$AwayTeam == NethTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  nethATeamGoalsFirstH <- nethATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  nethATeamGoalsFirstH <- as.character(nethATeamGoalsFirstH) %>% paste0(., "%")
  nethATeamGoalsSecondH <- as.data.frame(nethATeamGoals$FTHG - nethATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  nethATeamGoalsSecondH <- as.character(nethATeamGoalsSecondH) %>% paste0(., "%")
  nethAwayGoalData <<- data.frame(nethATeamGoalsFirstH, nethATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(nethAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(nethAwayGoalData)
}
nethAGoalData <- nethTeamA.goalData(NethTeamName)




nethHGoalDataTbl <- nethHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

nethAGoalDataTbl <- nethAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

