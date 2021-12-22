
library(tidyverse)
library(gt)


# BelgiumTeamName <- "Standard"

BelgiumTeamNameRating <- srsTablebelgium %>% filter(., belgiumTeam == BelgiumTeamName)
BelgiumTeamNameRating <- as.double(BelgiumTeamNameRating$teamRating)

belgiumTeamH.matchData <- function(BelgiumTeamName) {
  belgiumTeamData <- filter(belgium, belgium$HomeTeam == BelgiumTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  belgiumHTeamW <- belgiumTeamData %>% filter(FTR == "H") %>% nrow()
  belgiumHTeamD <- belgiumTeamData %>% filter(FTR == "D") %>% nrow()
  belgiumHTeamL <- belgiumTeamData %>% filter(FTR == "A") %>% nrow()
  belgiumHWDL <<- paste0(belgiumHTeamW," - ", belgiumHTeamD, " - ", belgiumHTeamL)
}
belgiumHData <- belgiumTeamH.matchData(BelgiumTeamName)

belgiumTeamH.goalData <- function(BelgiumTeamName) {
  belgiumHTeamGoals <- filter(belgium, belgium$HomeTeam == BelgiumTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  belgiumHTeamGoalsFirstH <- belgiumHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  belgiumHTeamGoalsFirstH <- as.character(belgiumHTeamGoalsFirstH) %>% paste0(., "%")
  belgiumHTeamGoalsSecondH <- as.data.frame(belgiumHTeamGoals$FTHG - belgiumHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  belgiumHTeamGoalsSecondH <- as.character(belgiumHTeamGoalsSecondH) %>% paste0(., "%")
  belgiumHomeGoalData <<- data.frame(belgiumHTeamGoalsFirstH, belgiumHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(belgiumHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(belgiumHomeGoalData)
}
belgiumHGoalData <- belgiumTeamH.goalData(BelgiumTeamName)


belgiumTeamA.matchData <- function(BelgiumTeamName) {
  belgiumTeamData <- filter(belgium, belgium$AwayTeam == BelgiumTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  belgiumATeamW <- belgiumTeamData %>% filter(FTR == "A") %>% nrow()
  belgiumATeamD <- belgiumTeamData %>% filter(FTR == "D") %>% nrow()
  belgiumATeamL <- belgiumTeamData %>% filter(FTR == "H") %>% nrow()
  belgiumAWDL <<- paste0(belgiumATeamW," - ", belgiumATeamD, " - ", belgiumATeamL)
  
}
belgiumAData <- belgiumTeamA.matchData(BelgiumTeamName)

belgiumTeamA.goalData <- function(BelgiumTeamName) {
  belgiumATeamGoals <- filter(belgium, belgium$AwayTeam == BelgiumTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  belgiumATeamGoalsFirstH <- belgiumATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  belgiumATeamGoalsFirstH <- as.character(belgiumATeamGoalsFirstH) %>% paste0(., "%")
  belgiumATeamGoalsSecondH <- as.data.frame(belgiumATeamGoals$FTHG - belgiumATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  belgiumATeamGoalsSecondH <- as.character(belgiumATeamGoalsSecondH) %>% paste0(., "%")
  belgiumAwayGoalData <<- data.frame(belgiumATeamGoalsFirstH, belgiumATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(belgiumAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(belgiumAwayGoalData)
}
belgiumAGoalData <- belgiumTeamA.goalData(BelgiumTeamName)


belgiumHomeTeamData <- filter(belgium, belgium$HomeTeam == BelgiumTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
belgiumAwayTeamData <- filter(belgium, belgium$AwayTeam == BelgiumTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

belgiumHomeTeamData$FTR[belgiumHomeTeamData$FTR == "H"] <- "W"
belgiumHomeTeamData$FTR[belgiumHomeTeamData$FTR == "A"] <- "L"
belgiumAwayTeamData$FTR[belgiumAwayTeamData$FTR == "H"] <- "L"
belgiumAwayTeamData$FTR[belgiumAwayTeamData$FTR == "A"] <- "W"

belgiumHomeTeamData <- belgiumHomeTeamData %>% arrange(desc(Date))
belgiumAwayTeamData <- belgiumAwayTeamData %>% arrange(desc(Date))

belgiumHDataTbl <- gt(belgiumHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", BelgiumTeamName)) %>%
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

belgiumADataTbl <- gt(belgiumAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", BelgiumTeamName)) %>%
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

belgiumTeamTen.matchData <- function(BelgiumTeamName) {
  belgiumTeamDataTen <- filter(belgium, HomeTeam==BelgiumTeamName|AwayTeam==BelgiumTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  belgiumTeamDataTen
}
belgiumTeamDataTen <- belgiumTeamTen.matchData(BelgiumTeamName)

belgiumTeamDataTenH <- belgiumTeamDataTen %>% filter(HomeTeam==BelgiumTeamName)
belgiumTeamDataTenA <- belgiumTeamDataTen %>% filter(AwayTeam==BelgiumTeamName)

belgiumTeamDataTenH$FTR[belgiumTeamDataTenH$FTR == "H"] <- "W"
belgiumTeamDataTenH$FTR[belgiumTeamDataTenH$FTR == "A"] <- "L"
belgiumTeamDataTenA$FTR[belgiumTeamDataTenA$FTR == "H"] <- "L"
belgiumTeamDataTenA$FTR[belgiumTeamDataTenA$FTR == "A"] <- "W"

belgiumTeamDataTen <- rbind(belgiumTeamDataTenH, belgiumTeamDataTenA) %>% arrange(desc(Date))



belgiumLastTenTbl <- gt(belgiumTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", BelgiumTeamName)) %>%
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


belgiumHGoalDataTbl <- belgiumHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

belgiumAGoalDataTbl <- belgiumAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())
