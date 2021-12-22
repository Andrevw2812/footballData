
library(tidyverse)
library(gt)


# EngpremTeamName <- ""

EngpremTeamNameRating <- srsTableengprem %>% filter(., engpremTeam == EngpremTeamName)
EngpremTeamNameRating <- as.double(EngpremTeamNameRating$teamRating)


engpremTeamH.matchData <- function(EngpremTeamName) {
  engpremTeamData <- filter(engprem, engprem$HomeTeam == EngpremTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  engpremHTeamW <- engpremTeamData %>% filter(FTR == "H") %>% nrow()
  engpremHTeamD <- engpremTeamData %>% filter(FTR == "D") %>% nrow()
  engpremHTeamL <- engpremTeamData %>% filter(FTR == "A") %>% nrow()
  engpremHWDL <<- paste0(engpremHTeamW," - ", engpremHTeamD, " - ", engpremHTeamL)
  
}
engpremHData <- engpremTeamH.matchData(EngpremTeamName)


engpremTeamA.matchData <- function(EngpremTeamName) {
  engpremTeamData <- filter(engprem, engprem$AwayTeam == EngpremTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  engpremATeamW <- engpremTeamData %>% filter(FTR == "A") %>% nrow()
  engpremATeamD <- engpremTeamData %>% filter(FTR == "D") %>% nrow()
  engpremATeamL <- engpremTeamData %>% filter(FTR == "H") %>% nrow()
  engpremAWDL <<- paste0(engpremATeamW," - ", engpremATeamD, " - ", engpremATeamL)
  
}
engpremAData <- engpremTeamA.matchData(EngpremTeamName)

engpremHomeTeamData <- filter(engprem, engprem$HomeTeam == EngpremTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
engpremAwayTeamData <- filter(engprem, engprem$AwayTeam == EngpremTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

engpremHomeTeamData$FTR[engpremHomeTeamData$FTR == "H"] <- "W"
engpremHomeTeamData$FTR[engpremHomeTeamData$FTR == "A"] <- "L"
engpremAwayTeamData$FTR[engpremAwayTeamData$FTR == "H"] <- "L"
engpremAwayTeamData$FTR[engpremAwayTeamData$FTR == "A"] <- "W"

engpremHomeTeamData <- engpremHomeTeamData %>% arrange(desc(Date))
engpremAwayTeamData <- engpremAwayTeamData %>% arrange(desc(Date))

engpremHDataTbl <- gt(engpremHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", EngpremTeamName)) %>%
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

engpremADataTbl <- gt(engpremAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", EngpremTeamName)) %>%
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

engpremTeamTen.matchData <- function(EngpremTeamName) {
  engpremTeamDataTen <- filter(engprem, HomeTeam==EngpremTeamName|AwayTeam==EngpremTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  engpremTeamDataTen
}
engpremTeamDataTen <- engpremTeamTen.matchData(EngpremTeamName)

engpremTeamDataTenH <- engpremTeamDataTen %>% filter(HomeTeam==EngpremTeamName)
engpremTeamDataTenA <- engpremTeamDataTen %>% filter(AwayTeam==EngpremTeamName)

engpremTeamDataTenH$FTR[engpremTeamDataTenH$FTR == "H"] <- "W"
engpremTeamDataTenH$FTR[engpremTeamDataTenH$FTR == "A"] <- "L"
engpremTeamDataTenA$FTR[engpremTeamDataTenA$FTR == "H"] <- "L"
engpremTeamDataTenA$FTR[engpremTeamDataTenA$FTR == "A"] <- "W"

engpremTeamDataTen <- rbind(engpremTeamDataTenH, engpremTeamDataTenA) %>% arrange(desc(Date))



engpremLastTenTbl <- gt(engpremTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", EngpremTeamName)) %>%
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

engpremTeamH.goalData <- function(EngpremTeamName) {
  engpremHTeamGoals <- filter(engprem, engprem$HomeTeam == EngpremTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  engpremHTeamGoalsFirstH <- engpremHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  engpremHTeamGoalsFirstH <- as.character(engpremHTeamGoalsFirstH) %>% paste0(., "%")
  engpremHTeamGoalsSecondH <- as.data.frame(engpremHTeamGoals$FTHG - engpremHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  engpremHTeamGoalsSecondH <- as.character(engpremHTeamGoalsSecondH) %>% paste0(., "%")
  engpremHomeGoalData <<- data.frame(engpremHTeamGoalsFirstH, engpremHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(engpremHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(engpremHomeGoalData)
}
engpremHGoalData <- engpremTeamH.goalData(EngpremTeamName)

engpremTeamA.goalData <- function(EngpremTeamName) {
  engpremATeamGoals <- filter(engprem, engprem$AwayTeam == EngpremTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  engpremATeamGoalsFirstH <- engpremATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  engpremATeamGoalsFirstH <- as.character(engpremATeamGoalsFirstH) %>% paste0(., "%")
  engpremATeamGoalsSecondH <- as.data.frame(engpremATeamGoals$FTHG - engpremATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  engpremATeamGoalsSecondH <- as.character(engpremATeamGoalsSecondH) %>% paste0(., "%")
  engpremAwayGoalData <<- data.frame(engpremATeamGoalsFirstH, engpremATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(engpremAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(engpremAwayGoalData)
}
engpremAGoalData <- engpremTeamA.goalData(EngpremTeamName)




engpremHGoalDataTbl <- engpremHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

engpremAGoalDataTbl <- engpremAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())