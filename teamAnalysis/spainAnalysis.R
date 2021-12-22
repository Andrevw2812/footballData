library(tidyverse)
library(gt)


# SpainTeamName <- ""
SpainTeamNameRating <- srsTablespain %>% filter(., spainTeam == SpainTeamName)
SpainTeamNameRating <- as.double(SpainTeamNameRating$teamRating)

spainTeamH.matchData <- function(SpainTeamName) {
  spainTeamData <- filter(spain, spain$HomeTeam == SpainTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  spainHTeamW <- spainTeamData %>% filter(FTR == "H") %>% nrow()
  spainHTeamD <- spainTeamData %>% filter(FTR == "D") %>% nrow()
  spainHTeamL <- spainTeamData %>% filter(FTR == "A") %>% nrow()
  spainHWDL <<- paste0(spainHTeamW," - ", spainHTeamD, " - ", spainHTeamL)
  
}
spainHData <- spainTeamH.matchData(SpainTeamName)


spainTeamA.matchData <- function(SpainTeamName) {
  spainTeamData <- filter(spain, spain$AwayTeam == SpainTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  spainATeamW <- spainTeamData %>% filter(FTR == "A") %>% nrow()
  spainATeamD <- spainTeamData %>% filter(FTR == "D") %>% nrow()
  spainATeamL <- spainTeamData %>% filter(FTR == "H") %>% nrow()
  spainAWDL <<- paste0(spainATeamW," - ", spainATeamD, " - ", spainATeamL)
  
}
spainAData <- spainTeamA.matchData(SpainTeamName)

spainHomeTeamData <- filter(spain, spain$HomeTeam == SpainTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
spainAwayTeamData <- filter(spain, spain$AwayTeam == SpainTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

spainHomeTeamData$FTR[spainHomeTeamData$FTR == "H"] <- "W"
spainHomeTeamData$FTR[spainHomeTeamData$FTR == "A"] <- "L"
spainAwayTeamData$FTR[spainAwayTeamData$FTR == "H"] <- "L"
spainAwayTeamData$FTR[spainAwayTeamData$FTR == "A"] <- "W"

spainHomeTeamData <- spainHomeTeamData %>% arrange(desc(Date))
spainAwayTeamData <- spainAwayTeamData %>% arrange(desc(Date))

spainHDataTbl <- gt(spainHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", SpainTeamName)) %>%
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

spainADataTbl <- gt(spainAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", SpainTeamName)) %>%
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

spainTeamTen.matchData <- function(SpainTeamName) {
  spainTeamDataTen <- filter(spain, HomeTeam==SpainTeamName|AwayTeam==SpainTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  spainTeamDataTen
}
spainTeamDataTen <- spainTeamTen.matchData(SpainTeamName)

spainTeamDataTenH <- spainTeamDataTen %>% filter(HomeTeam==SpainTeamName)
spainTeamDataTenA <- spainTeamDataTen %>% filter(AwayTeam==SpainTeamName)

spainTeamDataTenH$FTR[spainTeamDataTenH$FTR == "H"] <- "W"
spainTeamDataTenH$FTR[spainTeamDataTenH$FTR == "A"] <- "L"
spainTeamDataTenA$FTR[spainTeamDataTenA$FTR == "H"] <- "L"
spainTeamDataTenA$FTR[spainTeamDataTenA$FTR == "A"] <- "W"

spainTeamDataTen <- rbind(spainTeamDataTenH, spainTeamDataTenA) %>% arrange(desc(Date))



spainLastTenTbl <- gt(spainTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", SpainTeamName)) %>%
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

spainTeamH.goalData <- function(SpainTeamName) {
  spainHTeamGoals <- filter(spain, spain$HomeTeam == SpainTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  spainHTeamGoalsFirstH <- spainHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  spainHTeamGoalsFirstH <- as.character(spainHTeamGoalsFirstH) %>% paste0(., "%")
  spainHTeamGoalsSecondH <- as.data.frame(spainHTeamGoals$FTHG - spainHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  spainHTeamGoalsSecondH <- as.character(spainHTeamGoalsSecondH) %>% paste0(., "%")
  spainHomeGoalData <<- data.frame(spainHTeamGoalsFirstH, spainHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(spainHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(spainHomeGoalData)
}
spainHGoalData <- spainTeamH.goalData(SpainTeamName)

spainTeamA.goalData <- function(SpainTeamName) {
  spainATeamGoals <- filter(spain, spain$AwayTeam == SpainTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  spainATeamGoalsFirstH <- spainATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  spainATeamGoalsFirstH <- as.character(spainATeamGoalsFirstH) %>% paste0(., "%")
  spainATeamGoalsSecondH <- as.data.frame(spainATeamGoals$FTHG - spainATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  spainATeamGoalsSecondH <- as.character(spainATeamGoalsSecondH) %>% paste0(., "%")
  spainAwayGoalData <<- data.frame(spainATeamGoalsFirstH, spainATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(spainAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(spainAwayGoalData)
}
spainAGoalData <- spainTeamA.goalData(SpainTeamName)




spainHGoalDataTbl <- spainHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

spainAGoalDataTbl <- spainAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())
