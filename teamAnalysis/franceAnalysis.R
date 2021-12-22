library(tidyverse)
library(gt)


# FranceTeamName <- ""

FranceTeamNameRating <- srsTablefrance %>% filter(., franceTeam == FranceTeamName)
FranceTeamNameRating <- as.double(FranceTeamNameRating$teamRating)

franceTeamH.matchData <- function(FranceTeamName) {
  franceTeamData <- filter(france, france$HomeTeam == FranceTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  franceHTeamW <- franceTeamData %>% filter(FTR == "H") %>% nrow()
  franceHTeamD <- franceTeamData %>% filter(FTR == "D") %>% nrow()
  franceHTeamL <- franceTeamData %>% filter(FTR == "A") %>% nrow()
  franceHWDL <<- paste0(franceHTeamW," - ", franceHTeamD, " - ", franceHTeamL)
  
}
franceHData <- franceTeamH.matchData(FranceTeamName)


franceTeamA.matchData <- function(FranceTeamName) {
  franceTeamData <- filter(france, france$AwayTeam == FranceTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  franceATeamW <- franceTeamData %>% filter(FTR == "A") %>% nrow()
  franceATeamD <- franceTeamData %>% filter(FTR == "D") %>% nrow()
  franceATeamL <- franceTeamData %>% filter(FTR == "H") %>% nrow()
  franceAWDL <<- paste0(franceATeamW," - ", franceATeamD, " - ", franceATeamL)
  
}
franceAData <- franceTeamA.matchData(FranceTeamName)

franceHomeTeamData <- filter(france, france$HomeTeam == FranceTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
franceAwayTeamData <- filter(france, france$AwayTeam == FranceTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

franceHomeTeamData$FTR[franceHomeTeamData$FTR == "H"] <- "W"
franceHomeTeamData$FTR[franceHomeTeamData$FTR == "A"] <- "L"
franceAwayTeamData$FTR[franceAwayTeamData$FTR == "H"] <- "L"
franceAwayTeamData$FTR[franceAwayTeamData$FTR == "A"] <- "W"

franceHomeTeamData <- franceHomeTeamData %>% arrange(desc(Date))
franceAwayTeamData <- franceAwayTeamData %>% arrange(desc(Date))

franceHDataTbl <- gt(franceHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", FranceTeamName)) %>%
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

franceADataTbl <- gt(franceAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", FranceTeamName)) %>%
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

franceTeamTen.matchData <- function(FranceTeamName) {
  franceTeamDataTen <- filter(france, HomeTeam==FranceTeamName|AwayTeam==FranceTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  franceTeamDataTen
}
franceTeamDataTen <- franceTeamTen.matchData(FranceTeamName)

franceTeamDataTenH <- franceTeamDataTen %>% filter(HomeTeam==FranceTeamName)
franceTeamDataTenA <- franceTeamDataTen %>% filter(AwayTeam==FranceTeamName)

franceTeamDataTenH$FTR[franceTeamDataTenH$FTR == "H"] <- "W"
franceTeamDataTenH$FTR[franceTeamDataTenH$FTR == "A"] <- "L"
franceTeamDataTenA$FTR[franceTeamDataTenA$FTR == "H"] <- "L"
franceTeamDataTenA$FTR[franceTeamDataTenA$FTR == "A"] <- "W"

franceTeamDataTen <- rbind(franceTeamDataTenH, franceTeamDataTenA) %>% arrange(desc(Date))



franceLastTenTbl <- gt(franceTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", FranceTeamName)) %>%
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

franceTeamH.goalData <- function(FranceTeamName) {
  franceHTeamGoals <- filter(france, france$HomeTeam == FranceTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  franceHTeamGoalsFirstH <- franceHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  franceHTeamGoalsFirstH <- as.character(franceHTeamGoalsFirstH) %>% paste0(., "%")
  franceHTeamGoalsSecondH <- as.data.frame(franceHTeamGoals$FTHG - franceHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  franceHTeamGoalsSecondH <- as.character(franceHTeamGoalsSecondH) %>% paste0(., "%")
  franceHomeGoalData <<- data.frame(franceHTeamGoalsFirstH, franceHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(franceHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(franceHomeGoalData)
}
franceHGoalData <- franceTeamH.goalData(FranceTeamName)

franceTeamA.goalData <- function(FranceTeamName) {
  franceATeamGoals <- filter(france, france$AwayTeam == FranceTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  franceATeamGoalsFirstH <- franceATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  franceATeamGoalsFirstH <- as.character(franceATeamGoalsFirstH) %>% paste0(., "%")
  franceATeamGoalsSecondH <- as.data.frame(franceATeamGoals$FTHG - franceATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  franceATeamGoalsSecondH <- as.character(franceATeamGoalsSecondH) %>% paste0(., "%")
  franceAwayGoalData <<- data.frame(franceATeamGoalsFirstH, franceATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(franceAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(franceAwayGoalData)
}
franceAGoalData <- franceTeamA.goalData(FranceTeamName)




franceHGoalDataTbl <- franceHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

franceAGoalDataTbl <- franceAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())