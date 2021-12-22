
library(tidyverse)
library(gt)


# EngchampTeamName <- ""

EngchampTeamNameRating <- srsTableengchamp %>% filter(., engchampTeam == EngchampTeamName)
EngchampTeamNameRating <- as.double(EngchampTeamNameRating$teamRating)

engchampTeamH.matchData <- function(EngchampTeamName) {
  engchampTeamData <- filter(engchamp, engchamp$HomeTeam == EngchampTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  engchampHTeamW <- engchampTeamData %>% filter(FTR == "H") %>% nrow()
  engchampHTeamD <- engchampTeamData %>% filter(FTR == "D") %>% nrow()
  engchampHTeamL <- engchampTeamData %>% filter(FTR == "A") %>% nrow()
  engchampHWDL <<- paste0(engchampHTeamW," - ", engchampHTeamD, " - ", engchampHTeamL)
  
}
engchampHData <- engchampTeamH.matchData(EngchampTeamName)


engchampTeamA.matchData <- function(EngchampTeamName) {
  engchampTeamData <- filter(engchamp, engchamp$AwayTeam == EngchampTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  engchampATeamW <- engchampTeamData %>% filter(FTR == "A") %>% nrow()
  engchampATeamD <- engchampTeamData %>% filter(FTR == "D") %>% nrow()
  engchampATeamL <- engchampTeamData %>% filter(FTR == "H") %>% nrow()
  engchampAWDL <<- paste0(engchampATeamW," - ", engchampATeamD, " - ", engchampATeamL)
  
}
engchampAData <- engchampTeamA.matchData(EngchampTeamName)

engchampHomeTeamData <- filter(engchamp, engchamp$HomeTeam == EngchampTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
engchampAwayTeamData <- filter(engchamp, engchamp$AwayTeam == EngchampTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

engchampHomeTeamData$FTR[engchampHomeTeamData$FTR == "H"] <- "W"
engchampHomeTeamData$FTR[engchampHomeTeamData$FTR == "A"] <- "L"
engchampAwayTeamData$FTR[engchampAwayTeamData$FTR == "H"] <- "L"
engchampAwayTeamData$FTR[engchampAwayTeamData$FTR == "A"] <- "W"

engchampHomeTeamData <- engchampHomeTeamData %>% arrange(desc(Date))
engchampAwayTeamData <- engchampAwayTeamData %>% arrange(desc(Date))

engchampHDataTbl <- gt(engchampHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", EngchampTeamName)) %>%
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

engchampADataTbl <- gt(engchampAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", EngchampTeamName)) %>%
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

engchampTeamTen.matchData <- function(EngchampTeamName) {
  engchampTeamDataTen <- filter(engchamp, HomeTeam==EngchampTeamName|AwayTeam==EngchampTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  engchampTeamDataTen
}
engchampTeamDataTen <- engchampTeamTen.matchData(EngchampTeamName)

engchampTeamDataTenH <- engchampTeamDataTen %>% filter(HomeTeam==EngchampTeamName)
engchampTeamDataTenA <- engchampTeamDataTen %>% filter(AwayTeam==EngchampTeamName)

engchampTeamDataTenH$FTR[engchampTeamDataTenH$FTR == "H"] <- "W"
engchampTeamDataTenH$FTR[engchampTeamDataTenH$FTR == "A"] <- "L"
engchampTeamDataTenA$FTR[engchampTeamDataTenA$FTR == "H"] <- "L"
engchampTeamDataTenA$FTR[engchampTeamDataTenA$FTR == "A"] <- "W"

engchampTeamDataTen <- rbind(engchampTeamDataTenH, engchampTeamDataTenA) %>% arrange(desc(Date))



engchampLastTenTbl <- gt(engchampTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", EngchampTeamName)) %>%
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

engchampTeamH.goalData <- function(EngchampTeamName) {
  engchampHTeamGoals <- filter(engchamp, engchamp$HomeTeam == EngchampTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  engchampHTeamGoalsFirstH <- engchampHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  engchampHTeamGoalsFirstH <- as.character(engchampHTeamGoalsFirstH) %>% paste0(., "%")
  engchampHTeamGoalsSecondH <- as.data.frame(engchampHTeamGoals$FTHG - engchampHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  engchampHTeamGoalsSecondH <- as.character(engchampHTeamGoalsSecondH) %>% paste0(., "%")
  engchampHomeGoalData <<- data.frame(engchampHTeamGoalsFirstH, engchampHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(engchampHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(engchampHomeGoalData)
}
engchampHGoalData <- engchampTeamH.goalData(EngchampTeamName)

engchampTeamA.goalData <- function(EngchampTeamName) {
  engchampATeamGoals <- filter(engchamp, engchamp$AwayTeam == EngchampTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  engchampATeamGoalsFirstH <- engchampATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  engchampATeamGoalsFirstH <- as.character(engchampATeamGoalsFirstH) %>% paste0(., "%")
  engchampATeamGoalsSecondH <- as.data.frame(engchampATeamGoals$FTHG - engchampATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  engchampATeamGoalsSecondH <- as.character(engchampATeamGoalsSecondH) %>% paste0(., "%")
  engchampAwayGoalData <<- data.frame(engchampATeamGoalsFirstH, engchampATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(engchampAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(engchampAwayGoalData)
}
engchampAGoalData <- engchampTeamA.goalData(EngchampTeamName)




engchampHGoalDataTbl <- engchampHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

engchampAGoalDataTbl <- engchampAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())
