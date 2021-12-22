library(tidyverse)
library(gt)


# ScotpremTeamName <- ""
ScotpremTeamNameRating <- srsTablescotprem %>% filter(., scotpremTeam == ScotpremTeamName)
ScotpremTeamNameRating <- as.double(ScotpremTeamNameRating$teamRating)


scotpremTeamH.matchData <- function(ScotpremTeamName) {
  scotpremTeamData <- filter(scotprem, scotprem$HomeTeam == ScotpremTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  scotpremHTeamW <- scotpremTeamData %>% filter(FTR == "H") %>% nrow()
  scotpremHTeamD <- scotpremTeamData %>% filter(FTR == "D") %>% nrow()
  scotpremHTeamL <- scotpremTeamData %>% filter(FTR == "A") %>% nrow()
  scotpremHWDL <<- paste0(scotpremHTeamW," - ", scotpremHTeamD, " - ", scotpremHTeamL)
  
}
scotpremHData <- scotpremTeamH.matchData(ScotpremTeamName)


scotpremTeamA.matchData <- function(ScotpremTeamName) {
  scotpremTeamData <- filter(scotprem, scotprem$AwayTeam == ScotpremTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  scotpremATeamW <- scotpremTeamData %>% filter(FTR == "A") %>% nrow()
  scotpremATeamD <- scotpremTeamData %>% filter(FTR == "D") %>% nrow()
  scotpremATeamL <- scotpremTeamData %>% filter(FTR == "H") %>% nrow()
  scotpremAWDL <<- paste0(scotpremATeamW," - ", scotpremATeamD, " - ", scotpremATeamL)
  
}
scotpremAData <- scotpremTeamA.matchData(ScotpremTeamName)

scotpremHomeTeamData <- filter(scotprem, scotprem$HomeTeam == ScotpremTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
scotpremAwayTeamData <- filter(scotprem, scotprem$AwayTeam == ScotpremTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

scotpremHomeTeamData$FTR[scotpremHomeTeamData$FTR == "H"] <- "W"
scotpremHomeTeamData$FTR[scotpremHomeTeamData$FTR == "A"] <- "L"
scotpremAwayTeamData$FTR[scotpremAwayTeamData$FTR == "H"] <- "L"
scotpremAwayTeamData$FTR[scotpremAwayTeamData$FTR == "A"] <- "W"

scotpremHomeTeamData <- scotpremHomeTeamData %>% arrange(desc(Date))
scotpremAwayTeamData <- scotpremAwayTeamData %>% arrange(desc(Date))

scotpremHDataTbl <- gt(scotpremHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", ScotpremTeamName)) %>%
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

scotpremADataTbl <- gt(scotpremAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", ScotpremTeamName)) %>%
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

scotpremTeamTen.matchData <- function(ScotpremTeamName) {
  scotpremTeamDataTen <- filter(scotprem, HomeTeam==ScotpremTeamName|AwayTeam==ScotpremTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  scotpremTeamDataTen
}
scotpremTeamDataTen <- scotpremTeamTen.matchData(ScotpremTeamName)

scotpremTeamDataTenH <- scotpremTeamDataTen %>% filter(HomeTeam==ScotpremTeamName)
scotpremTeamDataTenA <- scotpremTeamDataTen %>% filter(AwayTeam==ScotpremTeamName)

scotpremTeamDataTenH$FTR[scotpremTeamDataTenH$FTR == "H"] <- "W"
scotpremTeamDataTenH$FTR[scotpremTeamDataTenH$FTR == "A"] <- "L"
scotpremTeamDataTenA$FTR[scotpremTeamDataTenA$FTR == "H"] <- "L"
scotpremTeamDataTenA$FTR[scotpremTeamDataTenA$FTR == "A"] <- "W"

scotpremTeamDataTen <- rbind(scotpremTeamDataTenH, scotpremTeamDataTenA) %>% arrange(desc(Date))



scotpremLastTenTbl <- gt(scotpremTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", ScotpremTeamName)) %>%
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

scotpremTeamH.goalData <- function(ScotpremTeamName) {
  scotpremHTeamGoals <- filter(scotprem, scotprem$HomeTeam == ScotpremTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  scotpremHTeamGoalsFirstH <- scotpremHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  scotpremHTeamGoalsFirstH <- as.character(scotpremHTeamGoalsFirstH) %>% paste0(., "%")
  scotpremHTeamGoalsSecondH <- as.data.frame(scotpremHTeamGoals$FTHG - scotpremHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  scotpremHTeamGoalsSecondH <- as.character(scotpremHTeamGoalsSecondH) %>% paste0(., "%")
  scotpremHomeGoalData <<- data.frame(scotpremHTeamGoalsFirstH, scotpremHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(scotpremHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(scotpremHomeGoalData)
}
scotpremHGoalData <- scotpremTeamH.goalData(ScotpremTeamName)

scotpremTeamA.goalData <- function(ScotpremTeamName) {
  scotpremATeamGoals <- filter(scotprem, scotprem$AwayTeam == ScotpremTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  scotpremATeamGoalsFirstH <- scotpremATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  scotpremATeamGoalsFirstH <- as.character(scotpremATeamGoalsFirstH) %>% paste0(., "%")
  scotpremATeamGoalsSecondH <- as.data.frame(scotpremATeamGoals$FTHG - scotpremATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  scotpremATeamGoalsSecondH <- as.character(scotpremATeamGoalsSecondH) %>% paste0(., "%")
  scotpremAwayGoalData <<- data.frame(scotpremATeamGoalsFirstH, scotpremATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(scotpremAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(scotpremAwayGoalData)
}
scotpremAGoalData <- scotpremTeamA.goalData(ScotpremTeamName)




scotpremHGoalDataTbl <- scotpremHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

scotpremAGoalDataTbl <- scotpremAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

