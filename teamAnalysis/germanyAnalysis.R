library(tidyverse)
library(gt)


# GermanyTeamName <- ""

GermanyTeamNameRating <- srsTablegermany %>% filter(., germanyTeam == GermanyTeamName)
GermanyTeamNameRating <- as.double(GermanyTeamNameRating$teamRating)

germanyTeamH.matchData <- function(GermanyTeamName) {
  germanyTeamData <- filter(germany, germany$HomeTeam == GermanyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  germanyHTeamW <- germanyTeamData %>% filter(FTR == "H") %>% nrow()
  germanyHTeamD <- germanyTeamData %>% filter(FTR == "D") %>% nrow()
  germanyHTeamL <- germanyTeamData %>% filter(FTR == "A") %>% nrow()
  germanyHWDL <<- paste0(germanyHTeamW," - ", germanyHTeamD, " - ", germanyHTeamL)
  
}
germanyHData <- germanyTeamH.matchData(GermanyTeamName)


germanyTeamA.matchData <- function(GermanyTeamName) {
  germanyTeamData <- filter(germany, germany$AwayTeam == GermanyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
  germanyATeamW <- germanyTeamData %>% filter(FTR == "A") %>% nrow()
  germanyATeamD <- germanyTeamData %>% filter(FTR == "D") %>% nrow()
  germanyATeamL <- germanyTeamData %>% filter(FTR == "H") %>% nrow()
  germanyAWDL <<- paste0(germanyATeamW," - ", germanyATeamD, " - ", germanyATeamL)
  
}
germanyAData <- germanyTeamA.matchData(GermanyTeamName)

germanyHomeTeamData <- filter(germany, germany$HomeTeam == GermanyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))
germanyAwayTeamData <- filter(germany, germany$AwayTeam == GermanyTeamName) %>% slice_tail(n=5) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% arrange(desc(Date))

germanyHomeTeamData$FTR[germanyHomeTeamData$FTR == "H"] <- "W"
germanyHomeTeamData$FTR[germanyHomeTeamData$FTR == "A"] <- "L"
germanyAwayTeamData$FTR[germanyAwayTeamData$FTR == "H"] <- "L"
germanyAwayTeamData$FTR[germanyAwayTeamData$FTR == "A"] <- "W"

germanyHomeTeamData <- germanyHomeTeamData %>% arrange(desc(Date))
germanyAwayTeamData <- germanyAwayTeamData %>% arrange(desc(Date))

germanyHDataTbl <- gt(germanyHomeTeamData) %>%
  tab_header(paste0("Last 5 Home Matches: ", GermanyTeamName)) %>%
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

germanyADataTbl <- gt(germanyAwayTeamData) %>%
  tab_header(paste0("Last 5 Away Matches: ", GermanyTeamName)) %>%
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

germanyTeamTen.matchData <- function(GermanyTeamName) {
  germanyTeamDataTen <- filter(germany, HomeTeam==GermanyTeamName|AwayTeam==GermanyTeamName) %>% slice_tail(n=10) %>% arrange(desc(Date)) %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  germanyTeamDataTen
}
germanyTeamDataTen <- germanyTeamTen.matchData(GermanyTeamName)

germanyTeamDataTenH <- germanyTeamDataTen %>% filter(HomeTeam==GermanyTeamName)
germanyTeamDataTenA <- germanyTeamDataTen %>% filter(AwayTeam==GermanyTeamName)

germanyTeamDataTenH$FTR[germanyTeamDataTenH$FTR == "H"] <- "W"
germanyTeamDataTenH$FTR[germanyTeamDataTenH$FTR == "A"] <- "L"
germanyTeamDataTenA$FTR[germanyTeamDataTenA$FTR == "H"] <- "L"
germanyTeamDataTenA$FTR[germanyTeamDataTenA$FTR == "A"] <- "W"

germanyTeamDataTen <- rbind(germanyTeamDataTenH, germanyTeamDataTenA) %>% arrange(desc(Date))



germanyLastTenTbl <- gt(germanyTeamDataTen) %>%
  tab_header(paste0("Last 10 Matches: ", GermanyTeamName)) %>%
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

germanyTeamH.goalData <- function(GermanyTeamName) {
  germanyHTeamGoals <- filter(germany, germany$HomeTeam == GermanyTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  germanyHTeamGoalsFirstH <- germanyHTeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  germanyHTeamGoalsFirstH <- as.character(germanyHTeamGoalsFirstH) %>% paste0(., "%")
  germanyHTeamGoalsSecondH <- as.data.frame(germanyHTeamGoals$FTHG - germanyHTeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  germanyHTeamGoalsSecondH <- as.character(germanyHTeamGoalsSecondH) %>% paste0(., "%")
  germanyHomeGoalData <<- data.frame(germanyHTeamGoalsFirstH, germanyHTeamGoalsSecondH, stringsAsFactors = FALSE)
  names(germanyHomeGoalData) <- c("1st H HomeGoals", "2nd H HomeGoals")
  return(germanyHomeGoalData)
}
germanyHGoalData <- germanyTeamH.goalData(GermanyTeamName)

germanyTeamA.goalData <- function(GermanyTeamName) {
  germanyATeamGoals <- filter(germany, germany$AwayTeam == GermanyTeamName) %>% slice_tail(n=5) %>% select(FTHG, HTHG)
  germanyATeamGoalsFirstH <- germanyATeamGoals %>% filter(HTHG > 0) %>% nrow()/5*100
  germanyATeamGoalsFirstH <- as.character(germanyATeamGoalsFirstH) %>% paste0(., "%")
  germanyATeamGoalsSecondH <- as.data.frame(germanyATeamGoals$FTHG - germanyATeamGoals$HTHG) %>% filter(. >0) %>% nrow()/5*100
  germanyATeamGoalsSecondH <- as.character(germanyATeamGoalsSecondH) %>% paste0(., "%")
  germanyAwayGoalData <<- data.frame(germanyATeamGoalsFirstH, germanyATeamGoalsSecondH, stringsAsFactors = FALSE)
  names(germanyAwayGoalData) <- c("1st H AwayGoals", "2nd H AwayGoals")
  return(germanyAwayGoalData)
}
germanyAGoalData <- germanyTeamA.goalData(GermanyTeamName)




germanyHGoalDataTbl <- germanyHGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())

germanyAGoalDataTbl <- germanyAGoalData %>% gt() %>% tab_options(table.width = "65%") %>% cols_align(align = "center", columns = everything())
