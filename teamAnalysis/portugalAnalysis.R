library(tidyverse)
library(gt)


# PortugalTeamName <- ""
PortugalTeamNameRating <- srsTableportugal %>% filter(., portugalTeam == PortugalTeamName)
PortugalTeamNameRating <- as.double(PortugalTeamNameRating$teamRating)

portugalTeamH.matchData <- function(PortugalTeamName) {
  portugalTeamData <- filter(portugal, portugal$HomeTeam == PortugalTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  portugalHTeamW <- portugalTeamData %>% filter(FTR == "H") %>% nrow()
  portugalHTeamD <- portugalTeamData %>% filter(FTR == "D") %>% nrow()
  portugalHTeamL <- portugalTeamData %>% filter(FTR == "A") %>% nrow()
  portugalHWDL <<- paste0(portugalHTeamW," - ", portugalHTeamD, " - ", portugalHTeamL)
  
}
portugalHData <- portugalTeamH.matchData(PortugalTeamName)


portugalTeamA.matchData <- function(PortugalTeamName) {
  portugalTeamData <- filter(portugal, portugal$AwayTeam == PortugalTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  portugalATeamW <- portugalTeamData %>% filter(FTR == "A") %>% nrow()
  portugalATeamD <- portugalTeamData %>% filter(FTR == "D") %>% nrow()
  portugalATeamL <- portugalTeamData %>% filter(FTR == "H") %>% nrow()
  portugalAWDL <<- paste0(portugalATeamW," - ", portugalATeamD, " - ", portugalATeamL)
  
}
portugalAData <- portugalTeamA.matchData(PortugalTeamName)

portugalHomeTeamData <- filter(portugal, portugal$HomeTeam == PortugalTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
portugalAwayTeamData <- filter(portugal, portugal$AwayTeam == PortugalTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

portugalHomeTeamData$FTR[portugalHomeTeamData$FTR == "H"] <- "W"
portugalHomeTeamData$FTR[portugalHomeTeamData$FTR == "A"] <- "L"
portugalAwayTeamData$FTR[portugalAwayTeamData$FTR == "H"] <- "L"
portugalAwayTeamData$FTR[portugalAwayTeamData$FTR == "A"] <- "W"

portugalHomeTeamData <- portugalHomeTeamData %>% arrange(desc(Date))
portugalAwayTeamData <- portugalAwayTeamData %>% arrange(desc(Date))

portugalHDataTbl <- gt(portugalHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", PortugalTeamName)) %>%
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

portugalADataTbl <- gt(portugalAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", PortugalTeamName)) %>%
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

portugalTeamTen.matchData <- function(PortugalTeamName) {
  portugalTeamDataTen <- filter(portugal, HomeTeam==PortugalTeamName|AwayTeam==PortugalTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  portugalTeamDataTen
}
portugalTeamDataTen <- portugalTeamTen.matchData(PortugalTeamName)

portugalTeamDataTenH <- portugalTeamDataTen %>% filter(HomeTeam==PortugalTeamName)
portugalTeamDataTenA <- portugalTeamDataTen %>% filter(AwayTeam==PortugalTeamName)

portugalTeamDataTenH$FTR[portugalTeamDataTenH$FTR == "H"] <- "W"
portugalTeamDataTenH$FTR[portugalTeamDataTenH$FTR == "A"] <- "L"
portugalTeamDataTenA$FTR[portugalTeamDataTenA$FTR == "H"] <- "L"
portugalTeamDataTenA$FTR[portugalTeamDataTenA$FTR == "A"] <- "W"

portugalTeamDataTen <- rbind(portugalTeamDataTenH, portugalTeamDataTenA) %>% arrange(desc(Date))



portugalLastTenTbl <- gt(portugalTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", PortugalTeamName)) %>%
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

portugalTeamH.goalData <- function(PortugalTeamName) {
  portugalHTeamGoals <- filter(portugal, portugal$HomeTeam == PortugalTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  portugalHTeamGoalsFirstH <- portugalHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  portugalHTeamGoalsFirstH <- as.character(portugalHTeamGoalsFirstH) %>% paste0(., "%")
  portugalHTeamGoalsSecondH <- as.data.frame(portugalHTeamGoals$FTHG - portugalHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  portugalHTeamGoalsSecondH <- as.character(portugalHTeamGoalsSecondH) %>% paste0(., "%")
  portugalHomeGoalData <<- data.frame(portugalHTeamGoalsFirstH, portugalHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(portugalHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(portugalHomeGoalData)
}
portugalHGoalData <- portugalTeamH.goalData(PortugalTeamName)

portugalTeamA.goalData <- function(PortugalTeamName) {
  portugalATeamGoals <- filter(portugal, portugal$AwayTeam == PortugalTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  portugalATeamGoalsFirstH <- portugalATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  portugalATeamGoalsFirstH <- as.character(portugalATeamGoalsFirstH) %>% paste0(., "%")
  portugalATeamGoalsSecondH <- as.data.frame(portugalATeamGoals$FTHG - portugalATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  portugalATeamGoalsSecondH <- as.character(portugalATeamGoalsSecondH) %>% paste0(., "%")
  portugalAwayGoalData <<- data.frame(portugalATeamGoalsFirstH, portugalATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(portugalAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(portugalAwayGoalData)
}
portugalAGoalData <- portugalTeamA.goalData(PortugalTeamName)




portugalHGoalDataTbl <- portugalHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

portugalAGoalDataTbl <- portugalAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())
