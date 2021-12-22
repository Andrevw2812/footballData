library(tidyverse)
library(gt)


# GreeceTeamName <- ""

GreeceTeamNameRating <- srsTablegreece %>% filter(., greeceTeam == GreeceTeamName)
GreeceTeamNameRating <- as.double(GreeceTeamNameRating$teamRating)


greeceTeamH.matchData <- function(GreeceTeamName) {
  greeceTeamData <- filter(greece, greece$HomeTeam == GreeceTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  greeceHTeamW <- greeceTeamData %>% filter(FTR == "H") %>% nrow()
  greeceHTeamD <- greeceTeamData %>% filter(FTR == "D") %>% nrow()
  greeceHTeamL <- greeceTeamData %>% filter(FTR == "A") %>% nrow()
  greeceHWDL <<- paste0(greeceHTeamW," - ", greeceHTeamD, " - ", greeceHTeamL)
  
}
greeceHData <- greeceTeamH.matchData(GreeceTeamName)


greeceTeamA.matchData <- function(GreeceTeamName) {
  greeceTeamData <- filter(greece, greece$AwayTeam == GreeceTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  greeceATeamW <- greeceTeamData %>% filter(FTR == "A") %>% nrow()
  greeceATeamD <- greeceTeamData %>% filter(FTR == "D") %>% nrow()
  greeceATeamL <- greeceTeamData %>% filter(FTR == "H") %>% nrow()
  greeceAWDL <<- paste0(greeceATeamW," - ", greeceATeamD, " - ", greeceATeamL)
  
}
greeceAData <- greeceTeamA.matchData(GreeceTeamName)

greeceHomeTeamData <- filter(greece, greece$HomeTeam == GreeceTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
greeceAwayTeamData <- filter(greece, greece$AwayTeam == GreeceTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

greeceHomeTeamData$FTR[greeceHomeTeamData$FTR == "H"] <- "W"
greeceHomeTeamData$FTR[greeceHomeTeamData$FTR == "A"] <- "L"
greeceAwayTeamData$FTR[greeceAwayTeamData$FTR == "H"] <- "L"
greeceAwayTeamData$FTR[greeceAwayTeamData$FTR == "A"] <- "W"

greeceHomeTeamData <- greeceHomeTeamData %>% arrange(desc(Date))
greeceAwayTeamData <- greeceAwayTeamData %>% arrange(desc(Date))

greeceHDataTbl <- gt(greeceHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", GreeceTeamName)) %>%
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

greeceADataTbl <- gt(greeceAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", GreeceTeamName)) %>%
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

greeceTeamTen.matchData <- function(GreeceTeamName) {
  greeceTeamDataTen <- filter(greece, HomeTeam==GreeceTeamName|AwayTeam==GreeceTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  greeceTeamDataTen
}
greeceTeamDataTen <- greeceTeamTen.matchData(GreeceTeamName)

greeceTeamDataTenH <- greeceTeamDataTen %>% filter(HomeTeam==GreeceTeamName)
greeceTeamDataTenA <- greeceTeamDataTen %>% filter(AwayTeam==GreeceTeamName)

greeceTeamDataTenH$FTR[greeceTeamDataTenH$FTR == "H"] <- "W"
greeceTeamDataTenH$FTR[greeceTeamDataTenH$FTR == "A"] <- "L"
greeceTeamDataTenA$FTR[greeceTeamDataTenA$FTR == "H"] <- "L"
greeceTeamDataTenA$FTR[greeceTeamDataTenA$FTR == "A"] <- "W"

greeceTeamDataTen <- rbind(greeceTeamDataTenH, greeceTeamDataTenA) %>% arrange(desc(Date))



greeceLastTenTbl <- gt(greeceTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", GreeceTeamName)) %>%
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

greeceTeamH.goalData <- function(GreeceTeamName) {
  greeceHTeamGoals <- filter(greece, greece$HomeTeam == GreeceTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  greeceHTeamGoalsFirstH <- greeceHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  greeceHTeamGoalsFirstH <- as.character(greeceHTeamGoalsFirstH) %>% paste0(., "%")
  greeceHTeamGoalsSecondH <- as.data.frame(greeceHTeamGoals$FTHG - greeceHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  greeceHTeamGoalsSecondH <- as.character(greeceHTeamGoalsSecondH) %>% paste0(., "%")
  greeceHomeGoalData <<- data.frame(greeceHTeamGoalsFirstH, greeceHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(greeceHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(greeceHomeGoalData)
}
greeceHGoalData <- greeceTeamH.goalData(GreeceTeamName)

greeceTeamA.goalData <- function(GreeceTeamName) {
  greeceATeamGoals <- filter(greece, greece$AwayTeam == GreeceTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  greeceATeamGoalsFirstH <- greeceATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  greeceATeamGoalsFirstH <- as.character(greeceATeamGoalsFirstH) %>% paste0(., "%")
  greeceATeamGoalsSecondH <- as.data.frame(greeceATeamGoals$FTHG - greeceATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  greeceATeamGoalsSecondH <- as.character(greeceATeamGoalsSecondH) %>% paste0(., "%")
  greeceAwayGoalData <<- data.frame(greeceATeamGoalsFirstH, greeceATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(greeceAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(greeceAwayGoalData)
}
greeceAGoalData <- greeceTeamA.goalData(GreeceTeamName)




greeceHGoalDataTbl <- greeceHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

greeceAGoalDataTbl <- greeceAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())
