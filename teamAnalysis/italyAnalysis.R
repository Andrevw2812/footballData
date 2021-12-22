library(tidyverse)
library(gt)


# ItalyTeamName <- ""

ItalyTeamNameRating <- srsTableitaly %>% filter(., italyTeam == ItalyTeamName)
ItalyTeamNameRating <- as.double(ItalyTeamNameRating$teamRating)

italyTeamH.matchData <- function(ItalyTeamName) {
  italyTeamData <- filter(italy, italy$HomeTeam == ItalyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  italyHTeamW <- italyTeamData %>% filter(FTR == "H") %>% nrow()
  italyHTeamD <- italyTeamData %>% filter(FTR == "D") %>% nrow()
  italyHTeamL <- italyTeamData %>% filter(FTR == "A") %>% nrow()
  italyHWDL <<- paste0(italyHTeamW," - ", italyHTeamD, " - ", italyHTeamL)
  
}
italyHData <- italyTeamH.matchData(ItalyTeamName)


italyTeamA.matchData <- function(ItalyTeamName) {
  italyTeamData <- filter(italy, italy$AwayTeam == ItalyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  italyATeamW <- italyTeamData %>% filter(FTR == "A") %>% nrow()
  italyATeamD <- italyTeamData %>% filter(FTR == "D") %>% nrow()
  italyATeamL <- italyTeamData %>% filter(FTR == "H") %>% nrow()
  italyAWDL <<- paste0(italyATeamW," - ", italyATeamD, " - ", italyATeamL)
  
}
italyAData <- italyTeamA.matchData(ItalyTeamName)

italyHomeTeamData <- filter(italy, italy$HomeTeam == ItalyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
italyAwayTeamData <- filter(italy, italy$AwayTeam == ItalyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

italyHomeTeamData$FTR[italyHomeTeamData$FTR == "H"] <- "W"
italyHomeTeamData$FTR[italyHomeTeamData$FTR == "A"] <- "L"
italyAwayTeamData$FTR[italyAwayTeamData$FTR == "H"] <- "L"
italyAwayTeamData$FTR[italyAwayTeamData$FTR == "A"] <- "W"

italyHomeTeamData <- italyHomeTeamData %>% arrange(desc(Date))
italyAwayTeamData <- italyAwayTeamData %>% arrange(desc(Date))

italyHDataTbl <- gt(italyHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", ItalyTeamName)) %>%
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

italyADataTbl <- gt(italyAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", ItalyTeamName)) %>%
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

italyTeamTen.matchData <- function(ItalyTeamName) {
  italyTeamDataTen <- filter(italy, HomeTeam==ItalyTeamName|AwayTeam==ItalyTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  italyTeamDataTen
}
italyTeamDataTen <- italyTeamTen.matchData(ItalyTeamName)

italyTeamDataTenH <- italyTeamDataTen %>% filter(HomeTeam==ItalyTeamName)
italyTeamDataTenA <- italyTeamDataTen %>% filter(AwayTeam==ItalyTeamName)

italyTeamDataTenH$FTR[italyTeamDataTenH$FTR == "H"] <- "W"
italyTeamDataTenH$FTR[italyTeamDataTenH$FTR == "A"] <- "L"
italyTeamDataTenA$FTR[italyTeamDataTenA$FTR == "H"] <- "L"
italyTeamDataTenA$FTR[italyTeamDataTenA$FTR == "A"] <- "W"

italyTeamDataTen <- rbind(italyTeamDataTenH, italyTeamDataTenA) %>% arrange(desc(Date))



italyLastTenTbl <- gt(italyTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", ItalyTeamName)) %>%
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

italyTeamH.goalData <- function(ItalyTeamName) {
  italyHTeamGoals <- filter(italy, italy$HomeTeam == ItalyTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  italyHTeamGoalsFirstH <- italyHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  italyHTeamGoalsFirstH <- as.character(italyHTeamGoalsFirstH) %>% paste0(., "%")
  italyHTeamGoalsSecondH <- as.data.frame(italyHTeamGoals$FTHG - italyHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  italyHTeamGoalsSecondH <- as.character(italyHTeamGoalsSecondH) %>% paste0(., "%")
  italyHomeGoalData <<- data.frame(italyHTeamGoalsFirstH, italyHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(italyHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(italyHomeGoalData)
}
italyHGoalData <- italyTeamH.goalData(ItalyTeamName)

italyTeamA.goalData <- function(ItalyTeamName) {
  italyATeamGoals <- filter(italy, italy$AwayTeam == ItalyTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  italyATeamGoalsFirstH <- italyATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  italyATeamGoalsFirstH <- as.character(italyATeamGoalsFirstH) %>% paste0(., "%")
  italyATeamGoalsSecondH <- as.data.frame(italyATeamGoals$FTHG - italyATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  italyATeamGoalsSecondH <- as.character(italyATeamGoalsSecondH) %>% paste0(., "%")
  italyAwayGoalData <<- data.frame(italyATeamGoalsFirstH, italyATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(italyAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(italyAwayGoalData)
}
italyAGoalData <- italyTeamA.goalData(ItalyTeamName)




italyHGoalDataTbl <- italyHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

italyAGoalDataTbl <- italyAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

