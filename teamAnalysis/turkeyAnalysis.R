library(tidyverse)
library(gt)


# TurkeyTeamName <- ""
TurkeyTeamNameRating <- srsTableturkey %>% filter(., turkeyTeam == TurkeyTeamName)
TurkeyTeamNameRating <- as.double(TurkeyTeamNameRating$teamRating)

turkeyTeamH.matchData <- function(TurkeyTeamName) {
  turkeyTeamData <- filter(turkey, turkey$HomeTeam == TurkeyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  turkeyHTeamW <- turkeyTeamData %>% filter(FTR == "H") %>% nrow()
  turkeyHTeamD <- turkeyTeamData %>% filter(FTR == "D") %>% nrow()
  turkeyHTeamL <- turkeyTeamData %>% filter(FTR == "A") %>% nrow()
  turkeyHWDL <<- paste0(turkeyHTeamW," - ", turkeyHTeamD, " - ", turkeyHTeamL)
  
}
turkeyHData <- turkeyTeamH.matchData(TurkeyTeamName)


turkeyTeamA.matchData <- function(TurkeyTeamName) {
  turkeyTeamData <- filter(turkey, turkey$AwayTeam == TurkeyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  turkeyATeamW <- turkeyTeamData %>% filter(FTR == "A") %>% nrow()
  turkeyATeamD <- turkeyTeamData %>% filter(FTR == "D") %>% nrow()
  turkeyATeamL <- turkeyTeamData %>% filter(FTR == "H") %>% nrow()
  turkeyAWDL <<- paste0(turkeyATeamW," - ", turkeyATeamD, " - ", turkeyATeamL)
  
}
turkeyAData <- turkeyTeamA.matchData(TurkeyTeamName)

turkeyHomeTeamData <- filter(turkey, turkey$HomeTeam == TurkeyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
turkeyAwayTeamData <- filter(turkey, turkey$AwayTeam == TurkeyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

turkeyHomeTeamData$FTR[turkeyHomeTeamData$FTR == "H"] <- "W"
turkeyHomeTeamData$FTR[turkeyHomeTeamData$FTR == "A"] <- "L"
turkeyAwayTeamData$FTR[turkeyAwayTeamData$FTR == "H"] <- "L"
turkeyAwayTeamData$FTR[turkeyAwayTeamData$FTR == "A"] <- "W"

turkeyHomeTeamData <- turkeyHomeTeamData %>% arrange(desc(Date))
turkeyAwayTeamData <- turkeyAwayTeamData %>% arrange(desc(Date))

turkeyHDataTbl <- gt(turkeyHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", TurkeyTeamName)) %>%
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

turkeyADataTbl <- gt(turkeyAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", TurkeyTeamName)) %>%
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

turkeyTeamTen.matchData <- function(TurkeyTeamName) {
  turkeyTeamDataTen <- filter(turkey, HomeTeam==TurkeyTeamName|AwayTeam==TurkeyTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  turkeyTeamDataTen
}
turkeyTeamDataTen <- turkeyTeamTen.matchData(TurkeyTeamName)

turkeyTeamDataTenH <- turkeyTeamDataTen %>% filter(HomeTeam==TurkeyTeamName)
turkeyTeamDataTenA <- turkeyTeamDataTen %>% filter(AwayTeam==TurkeyTeamName)

turkeyTeamDataTenH$FTR[turkeyTeamDataTenH$FTR == "H"] <- "W"
turkeyTeamDataTenH$FTR[turkeyTeamDataTenH$FTR == "A"] <- "L"
turkeyTeamDataTenA$FTR[turkeyTeamDataTenA$FTR == "H"] <- "L"
turkeyTeamDataTenA$FTR[turkeyTeamDataTenA$FTR == "A"] <- "W"

turkeyTeamDataTen <- rbind(turkeyTeamDataTenH, turkeyTeamDataTenA) %>% arrange(desc(Date))



turkeyLastTenTbl <- gt(turkeyTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", TurkeyTeamName)) %>%
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

turkeyTeamH.goalData <- function(TurkeyTeamName) {
  turkeyHTeamGoals <- filter(turkey, turkey$HomeTeam == TurkeyTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  turkeyHTeamGoalsFirstH <- turkeyHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  turkeyHTeamGoalsFirstH <- as.character(turkeyHTeamGoalsFirstH) %>% paste0(., "%")
  turkeyHTeamGoalsSecondH <- as.data.frame(turkeyHTeamGoals$FTHG - turkeyHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  turkeyHTeamGoalsSecondH <- as.character(turkeyHTeamGoalsSecondH) %>% paste0(., "%")
  turkeyHomeGoalData <<- data.frame(turkeyHTeamGoalsFirstH, turkeyHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(turkeyHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(turkeyHomeGoalData)
}
turkeyHGoalData <- turkeyTeamH.goalData(TurkeyTeamName)

turkeyTeamA.goalData <- function(TurkeyTeamName) {
  turkeyATeamGoals <- filter(turkey, turkey$AwayTeam == TurkeyTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  turkeyATeamGoalsFirstH <- turkeyATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  turkeyATeamGoalsFirstH <- as.character(turkeyATeamGoalsFirstH) %>% paste0(., "%")
  turkeyATeamGoalsSecondH <- as.data.frame(turkeyATeamGoals$FTHG - turkeyATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  turkeyATeamGoalsSecondH <- as.character(turkeyATeamGoalsSecondH) %>% paste0(., "%")
  turkeyAwayGoalData <<- data.frame(turkeyATeamGoalsFirstH, turkeyATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(turkeyAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(turkeyAwayGoalData)
}
turkeyAGoalData <- turkeyTeamA.goalData(TurkeyTeamName)




turkeyHGoalDataTbl <- turkeyHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

turkeyAGoalDataTbl <- turkeyAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

