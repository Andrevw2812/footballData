
leagueGoalData <- function(x){
  selLeague <- x
  # HT
  htHGoals <- selLeague %>% select(HTHG) %>% sum()
  htAGoals <- selLeague %>% select(HTAG) %>% sum()
  htAllGoals <- htHGoals+htAGoals
  
  # FT
  ftHGoals <- selLeague %>% select(FTHG) %>% sum()
  ftAGoals <- selLeague %>% select(FTAG) %>% sum()
  ftAllGoals <- ftHGoals+ftAGoals
  
  # 2nd Half
  shHGoals <- selLeague %>% select(HTHG, FTHG)
  shHGoals <- (shHGoals$FTHG - shHGoals$HTHG) %>% sum()
  shAGoals <- selLeague %>% select(HTAG, FTAG)
  shAGoals <- (shAGoals$FTAG - shAGoals$HTAG) %>% sum()
  shAllGoals <- shHGoals+shAGoals
  
  
  # Match goals
  allGoals <- selLeague %>% select(HTHG, HTAG, FTHG, FTAG)
  
  allGoals$FH <- allGoals$HTHG+allGoals$HTAG
  allGoals$FT <- allGoals$FTHG+allGoals$FTAG
  allGoals$SH <- allGoals$FT-allGoals$FH
  
  allGoals$whichHalf <- if_else(allGoals$FH > allGoals$SH, "First Half",if_else(allGoals$FH < allGoals$SH, "Second Half", "Equal"))
  
  fhCount <- allGoals %>% filter(whichHalf == "First Half") %>% nrow()
  shCount <- allGoals %>% filter(whichHalf == "Second Half") %>% nrow()
  equalCount <- allGoals %>% filter(whichHalf == "Equal") %>% nrow()
  
  leagueGoalInfo <- cbind(selLeague$LeagueName, htHGoals, htAGoals, htAllGoals, shHGoals, shAGoals, shAllGoals, ftHGoals, ftAGoals, ftAllGoals, fhCount, shCount, equalCount)
  leagueGoalInfo <- as_tibble(leagueGoalInfo)
  return(leagueGoalInfo[1, ])

}

belgiumGoalData <- leagueGoalData(belgium)
engchampGoalData <- leagueGoalData(engchamp)
engpremGoalData <- leagueGoalData(engprem)
franceGoalData <- leagueGoalData(france)
germanyGoalData <- leagueGoalData(germany)
greeceGoalData <- leagueGoalData(greece)
italyGoalData <- leagueGoalData(italy)
nethGoalData <- leagueGoalData(neth)
portugalGoalData <- leagueGoalData(portugal)
scotpremGoalData <- leagueGoalData(scotprem)
spainGoalData <- leagueGoalData(spain)
turkeyGoalData <- leagueGoalData(turkey)

allLeagueGoalData <- rbind(belgiumGoalData, engchampGoalData, engpremGoalData, franceGoalData, germanyGoalData, greeceGoalData, italyGoalData, nethGoalData, portugalGoalData, scotpremGoalData, spainGoalData, turkeyGoalData)

allLeagueGoalData <- allLeagueGoalData %>% rename("LeagueName" = V1)





