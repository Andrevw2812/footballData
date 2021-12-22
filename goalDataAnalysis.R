library(tidyverse)
library(rvest)
library(gt)



# Get Goal Data Home, Away and Combined last 10 games
#England Premiership
getengpremGoalData <- function(x) {
  engpremTeam <- x
  # Teams Home Games
  totalHGames <- engprem %>% filter(HomeTeam==engpremTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- engprem %>% filter(HomeTeam==engpremTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- engprem %>% filter(HomeTeam==engpremTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- engprem %>% filter(HomeTeam==engpremTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- engprem %>% filter(AwayTeam==engpremTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- engprem %>% filter(AwayTeam==engpremTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- engprem %>% filter(AwayTeam==engpremTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- engprem %>% filter(AwayTeam==engpremTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- engprem %>% filter(AwayTeam==engpremTeam|HomeTeam==engpremTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- engpremTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsengpremGoalDataF <<- sapply(engpremTeams$engpremTeam, getengpremGoalData, simplify = TRUE)
allTeamsengpremGoalDataF <<- t(allTeamsengpremGoalDataF)
# rownames(allTeamsengpremGoalDataF) <<- NULL

allTeamsengpremGoalDataF <<- as.data.frame(allTeamsengpremGoalDataF, row.names = NULL)


allTeamsengpremGoalDataFGT <<- allTeamsengpremGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")

totalGoalsengpremDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())

#engpremDFGT <<- allTeamsengpremGoalDataF %>% filter(TeamName==EngpremTeamName) %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())

#England Championship
getengchampGoalData <- function(x) {
  engchampTeam <- x
  # Teams Home Games
  totalHGames <- engchamp %>% filter(HomeTeam==engchampTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- engchamp %>% filter(HomeTeam==engchampTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- engchamp %>% filter(HomeTeam==engchampTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- engchamp %>% filter(HomeTeam==engchampTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- engchamp %>% filter(AwayTeam==engchampTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- engchamp %>% filter(AwayTeam==engchampTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- engchamp %>% filter(AwayTeam==engchampTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- engchamp %>% filter(AwayTeam==engchampTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- engchamp %>% filter(AwayTeam==engchampTeam|HomeTeam==engchampTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- engchampTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsengchampGoalDataF <<- sapply(engchampTeams$engchampTeam, getengchampGoalData, simplify = TRUE)
allTeamsengchampGoalDataF <<- t(allTeamsengchampGoalDataF)
# rownames(allTeamsengchampGoalDataF) <<- NULL

allTeamsengchampGoalDataF <<- as.data.frame(allTeamsengchampGoalDataF, row.names = NULL)


allTeamsengchampGoalDataFGT <<- allTeamsengchampGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")

totalGoalsengchampDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())

#Belgium
getbelgiumGoalData <- function(x) {
  belgiumTeam <- x
  # Teams Home Games
  totalHGames <- belgium %>% filter(HomeTeam==belgiumTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- belgium %>% filter(HomeTeam==belgiumTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- belgium %>% filter(HomeTeam==belgiumTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- belgium %>% filter(HomeTeam==belgiumTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- belgium %>% filter(AwayTeam==belgiumTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- belgium %>% filter(AwayTeam==belgiumTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- belgium %>% filter(AwayTeam==belgiumTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- belgium %>% filter(AwayTeam==belgiumTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- belgium %>% filter(AwayTeam==belgiumTeam|HomeTeam==belgiumTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- belgiumTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsbelgiumGoalDataF <<- sapply(belgiumTeams$belgiumTeam, getbelgiumGoalData, simplify = TRUE)
allTeamsbelgiumGoalDataF <<- t(allTeamsbelgiumGoalDataF)
# rownames(allTeamsbelgiumGoalDataF) <<- NULL

allTeamsbelgiumGoalDataF <<- as.data.frame(allTeamsbelgiumGoalDataF, row.names = NULL)


allTeamsbelgiumGoalDataFGT <<- allTeamsbelgiumGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")

totalGoalsbelgiumDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())


#France
getfranceGoalData <- function(x) {
  franceTeam <- x
  # Teams Home Games
  totalHGames <- france %>% filter(HomeTeam==franceTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- france %>% filter(HomeTeam==franceTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- france %>% filter(HomeTeam==franceTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- france %>% filter(HomeTeam==franceTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- france %>% filter(AwayTeam==franceTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- france %>% filter(AwayTeam==franceTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- france %>% filter(AwayTeam==franceTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- france %>% filter(AwayTeam==franceTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- france %>% filter(AwayTeam==franceTeam|HomeTeam==franceTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- franceTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsfranceGoalDataF <<- sapply(franceTeams$franceTeam, getfranceGoalData, simplify = TRUE)
allTeamsfranceGoalDataF <<- t(allTeamsfranceGoalDataF)
# rownames(allTeamsfranceGoalDataF) <<- NULL

allTeamsfranceGoalDataF <<- as.data.frame(allTeamsfranceGoalDataF, row.names = NULL)


allTeamsfranceGoalDataFGT <<- allTeamsfranceGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")

totalGoalsfranceDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())

# Germany
getgermanyGoalData <- function(x) {
  germanyTeam <- x
  # Teams Home Games
  totalHGames <- germany %>% filter(HomeTeam==germanyTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- germany %>% filter(HomeTeam==germanyTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- germany %>% filter(HomeTeam==germanyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- germany %>% filter(HomeTeam==germanyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- germany %>% filter(AwayTeam==germanyTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- germany %>% filter(AwayTeam==germanyTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- germany %>% filter(AwayTeam==germanyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- germany %>% filter(AwayTeam==germanyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- germany %>% filter(AwayTeam==germanyTeam|HomeTeam==germanyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- germanyTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsgermanyGoalDataF <<- sapply(germanyTeams$germanyTeam, getgermanyGoalData, simplify = TRUE)
allTeamsgermanyGoalDataF <<- t(allTeamsgermanyGoalDataF)
# rownames(allTeamsgermanyGoalDataF) <<- NULL

allTeamsgermanyGoalDataF <<- as.data.frame(allTeamsgermanyGoalDataF, row.names = NULL)


allTeamsgermanyGoalDataFGT <<- allTeamsgermanyGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")

totalGoalsgermanyDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())

# Greece
getgreeceGoalData <- function(x) {
  greeceTeam <- x
  # Teams Home Games
  totalHGames <- greece %>% filter(HomeTeam==greeceTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- greece %>% filter(HomeTeam==greeceTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- greece %>% filter(HomeTeam==greeceTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- greece %>% filter(HomeTeam==greeceTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- greece %>% filter(AwayTeam==greeceTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- greece %>% filter(AwayTeam==greeceTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- greece %>% filter(AwayTeam==greeceTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- greece %>% filter(AwayTeam==greeceTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- greece %>% filter(AwayTeam==greeceTeam|HomeTeam==greeceTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- greeceTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsgreeceGoalDataF <<- sapply(greeceTeams$greeceTeam, getgreeceGoalData, simplify = TRUE)
allTeamsgreeceGoalDataF <<- t(allTeamsgreeceGoalDataF)
# rownames(allTeamsgreeceGoalDataF) <<- NULL

allTeamsgreeceGoalDataF <<- as.data.frame(allTeamsgreeceGoalDataF, row.names = NULL)


allTeamsgreeceGoalDataFGT <<- allTeamsgreeceGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")

totalGoalsgreeceDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())


# Italy
getitalyGoalData <- function(x) {
  italyTeam <- x
  # Teams Home Games
  totalHGames <- italy %>% filter(HomeTeam==italyTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- italy %>% filter(HomeTeam==italyTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- italy %>% filter(HomeTeam==italyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- italy %>% filter(HomeTeam==italyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- italy %>% filter(AwayTeam==italyTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- italy %>% filter(AwayTeam==italyTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- italy %>% filter(AwayTeam==italyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- italy %>% filter(AwayTeam==italyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- italy %>% filter(AwayTeam==italyTeam|HomeTeam==italyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- italyTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsitalyGoalDataF <<- sapply(italyTeams$italyTeam, getitalyGoalData, simplify = TRUE)
allTeamsitalyGoalDataF <<- t(allTeamsitalyGoalDataF)
# rownames(allTeamsitalyGoalDataF) <<- NULL

allTeamsitalyGoalDataF <<- as.data.frame(allTeamsitalyGoalDataF, row.names = NULL)


allTeamsitalyGoalDataFGT <<- allTeamsitalyGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")

totalGoalsitalyDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())

# Netherlands
getnethGoalData <- function(x) {
  nethTeam <- x
  # Teams Home Games
  totalHGames <- neth %>% filter(HomeTeam==nethTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- neth %>% filter(HomeTeam==nethTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- neth %>% filter(HomeTeam==nethTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- neth %>% filter(HomeTeam==nethTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- neth %>% filter(AwayTeam==nethTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- neth %>% filter(AwayTeam==nethTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- neth %>% filter(AwayTeam==nethTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- neth %>% filter(AwayTeam==nethTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- neth %>% filter(AwayTeam==nethTeam|HomeTeam==nethTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- nethTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsnethGoalDataF <<- sapply(nethTeams$nethTeam, getnethGoalData, simplify = TRUE)
allTeamsnethGoalDataF <<- t(allTeamsnethGoalDataF)
# rownames(allTeamsnethGoalDataF) <<- NULL

allTeamsnethGoalDataF <<- as.data.frame(allTeamsnethGoalDataF, row.names = NULL)


allTeamsnethGoalDataFGT <<- allTeamsnethGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")

totalGoalsnethDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())

# Portugal
getportugalGoalData <- function(x) {
  portugalTeam <- x
  # Teams Home Games
  totalHGames <- portugal %>% filter(HomeTeam==portugalTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- portugal %>% filter(HomeTeam==portugalTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- portugal %>% filter(HomeTeam==portugalTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- portugal %>% filter(HomeTeam==portugalTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- portugal %>% filter(AwayTeam==portugalTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- portugal %>% filter(AwayTeam==portugalTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- portugal %>% filter(AwayTeam==portugalTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- portugal %>% filter(AwayTeam==portugalTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- portugal %>% filter(AwayTeam==portugalTeam|HomeTeam==portugalTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- portugalTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsportugalGoalDataF <<- sapply(portugalTeams$portugalTeam, getportugalGoalData, simplify = TRUE)
allTeamsportugalGoalDataF <<- t(allTeamsportugalGoalDataF)
# rownames(allTeamsportugalGoalDataF) <<- NULL

allTeamsportugalGoalDataF <<- as.data.frame(allTeamsportugalGoalDataF, row.names = NULL)


allTeamsportugalGoalDataFGT <<- allTeamsportugalGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")

totalGoalsportugalDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())

# Scotland
getscotpremGoalData <- function(x) {
  scotpremTeam <- x
  # Teams Home Games
  totalHGames <- scotprem %>% filter(HomeTeam==scotpremTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- scotprem %>% filter(HomeTeam==scotpremTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- scotprem %>% filter(HomeTeam==scotpremTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- scotprem %>% filter(HomeTeam==scotpremTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- scotprem %>% filter(AwayTeam==scotpremTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- scotprem %>% filter(AwayTeam==scotpremTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- scotprem %>% filter(AwayTeam==scotpremTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- scotprem %>% filter(AwayTeam==scotpremTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- scotprem %>% filter(AwayTeam==scotpremTeam|HomeTeam==scotpremTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- scotpremTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsscotpremGoalDataF <<- sapply(scotpremTeams$scotpremTeam, getscotpremGoalData, simplify = TRUE)
allTeamsscotpremGoalDataF <<- t(allTeamsscotpremGoalDataF)
# rownames(allTeamsscotpremGoalDataF) <<- NULL

allTeamsscotpremGoalDataF <<- as.data.frame(allTeamsscotpremGoalDataF, row.names = NULL)


allTeamsscotpremGoalDataFGT <<- allTeamsscotpremGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")

totalGoalsscotpremDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())

# Spain
getspainGoalData <- function(x) {
  spainTeam <- x
  # Teams Home Games
  totalHGames <- spain %>% filter(HomeTeam==spainTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- spain %>% filter(HomeTeam==spainTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- spain %>% filter(HomeTeam==spainTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- spain %>% filter(HomeTeam==spainTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- spain %>% filter(AwayTeam==spainTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- spain %>% filter(AwayTeam==spainTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- spain %>% filter(AwayTeam==spainTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- spain %>% filter(AwayTeam==spainTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- spain %>% filter(AwayTeam==spainTeam|HomeTeam==spainTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- spainTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsspainGoalDataF <<- sapply(spainTeams$spainTeam, getspainGoalData, simplify = TRUE)
allTeamsspainGoalDataF <<- t(allTeamsspainGoalDataF)
# rownames(allTeamsspainGoalDataF) <<- NULL

allTeamsspainGoalDataF <<- as.data.frame(allTeamsspainGoalDataF, row.names = NULL)


allTeamsspainGoalDataFGT <<- allTeamsspainGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")

totalGoalsspainDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())

# Turkey
getturkeyGoalData <- function(x) {
  turkeyTeam <- x
  # Teams Home Games
  totalHGames <- turkey %>% filter(HomeTeam==turkeyTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  hGoalsHScored <- turkey %>% filter(HomeTeam==turkeyTeam) %>% select(FTHG) %>% slice_tail(n=10)
  hGoalsHScored <- hGoalsHScored %>% sum()
  hGoalsAll <- turkey %>% filter(HomeTeam==turkeyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGoalsAll <- hGoalsAll %>% sum()
  hGamesBTTS <- turkey %>% filter(HomeTeam==turkeyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  hGamesBTTS$BTTS <- ifelse((hGamesBTTS$FTHG>0 & hGamesBTTS$FTAG>0), 1, 0)
  hGamesBTTSCount <- hGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioHBTTS <- round(hGamesBTTSCount/totalHGames, 2)*100
  
  # Teams Away Games
  totalAGames <- turkey %>% filter(AwayTeam==turkeyTeam) %>% select(FTHG) %>% slice_tail(n=10) %>% nrow()
  aGoalsAScored <- turkey %>% filter(AwayTeam==turkeyTeam) %>% select(FTAG) %>% slice_tail(n=10)
  aGoalsAScored <- aGoalsAScored %>% sum()
  aGoalsAll <- turkey %>% filter(AwayTeam==turkeyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGoalsAll <- aGoalsAll %>% sum()
  aGamesBTTS <- turkey %>% filter(AwayTeam==turkeyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=10)
  aGamesBTTS$BTTS <- ifelse((aGamesBTTS$FTHG>0 & aGamesBTTS$FTAG>0), 1, 0)
  aGamesBTTSCount <- aGamesBTTS %>% filter(BTTS>0) %>% nrow()
  ratioABTTS <- round(aGamesBTTSCount/totalAGames, 2)*100
  
  # Games Analysis
  totalGames <<- turkey %>% filter(AwayTeam==turkeyTeam|HomeTeam==turkeyTeam) %>% select(FTHG, FTAG) %>% slice_tail(n=20)
  totalGamesCount <<- totalGames %>% nrow()
  totalGoalsDF <<- data.frame(matrix(data = NA, nrow = 1, ncol = 10), stringsAsFactors = FALSE)
  names(totalGoalsDF) <<- c("TeamName", "TotalGames", "0.5", "0.5Ratio", "1.5", "1.5Ratio", "2.5", "2.5Ratio", "3.5", "3.5Ratio")
  totalGoalsDF$TeamName <<- turkeyTeam
  totalGoalsDF$TotalGames <<- totalGamesCount
  totalGames$GoalCount <<- totalGames$FTHG+totalGames$FTAG
  totalGoalsDF$`0.5` <<- totalGames %>% filter(GoalCount>0.5) %>% nrow()
  totalGoalsDF$`0.5Ratio` <<- paste0(round(totalGoalsDF$`0.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`1.5` <<- totalGames %>% filter(GoalCount>1.5) %>% nrow()
  totalGoalsDF$`1.5Ratio` <<- paste0(round(totalGoalsDF$`1.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`2.5` <<- totalGames %>% filter(GoalCount>2.5) %>% nrow()
  totalGoalsDF$`2.5Ratio` <<- paste0(round(totalGoalsDF$`2.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  totalGoalsDF$`3.5` <<- totalGames %>% filter(GoalCount>3.5) %>% nrow()
  totalGoalsDF$`3.5Ratio` <<- paste0(round(totalGoalsDF$`3.5`/totalGoalsDF$TotalGames, 2)*100, "%")
  return(totalGoalsDF)
  # allTeamsGoalData <- data.frame()
  allTeamsGoalData <<- rbind(allTeamsGoalData, totalGoalsDF)
  
}

allTeamsturkeyGoalDataF <<- sapply(turkeyTeams$turkeyTeam, getturkeyGoalData, simplify = TRUE)
allTeamsturkeyGoalDataF <<- t(allTeamsturkeyGoalDataF)
# rownames(allTeamsturkeyGoalDataF) <<- NULL

allTeamsturkeyGoalDataF <<- as.data.frame(allTeamsturkeyGoalDataF, row.names = NULL)

totalGoalsturkeyDFGT <<- totalGoalsDF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything())

allTeamsturkeyGoalDataFGT <<- allTeamsturkeyGoalDataF %>% gt() %>% tab_header("Total Goals Analysis", subtitle = "Based on Over indicated Goals") %>% tab_options(table.width = "85%") %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything())) %>% cols_align(align = "center", columns = everything()) %>% cols_align(align = "left", columns = "TeamName")