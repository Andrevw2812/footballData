

# Custom Rating Test
# Rating Points
# H Win = 1
# H Draw = 0
# A Win = 2
# A Draw = 0.5

ratingTestData <- engprem 
ratingTestData<- ratingTestData %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

ratingTestData$hScore <- ifelse(ratingTestData$FTR=="H", 1, 0)
ratingTestData$aScore <- ifelse(ratingTestData$FTR=="A", 2, 0.5)

engpremTeams

engpremTeamRating <- function(x) {
  engpremTeam <- x
  hScoreMean <- ratingTestData %>% filter(HomeTeam==engpremTeam) %>% select(hScore)
  hScoreMean <- round(mean(hScoreMean$hScore), 2)
  aScoreMean <- ratingTestData %>% filter(AwayTeam==engpremTeam) %>% select(aScore)
  aScoreMean <- round(mean(aScoreMean$aScore), 2)
  ratingsDF <- data.frame(engpremTeam, hScoreMean, aScoreMean)
  return(ratingsDF)
}

ratingsFinal <- sapply(engpremTeams$engpremTeam, engpremTeamRating, simplify = TRUE)
ratingsFinal <- t(ratingsFinal)
rownames(ratingsFinal) <- NULL
ratingsFinal <- as.data.frame(ratingsFinal)
ratingsFinal$combScore <- as.double(ratingsFinal[,2])+as.double(ratingsFinal[,3])

ratingsFinal <- ratingsFinal %>% arrange(desc(combScore))

#New SRS Calcs

#===== Ratings =====

ratingsFinal$SRSNew <- as.double(ratingsFinal$combScore)+((1/10)*(sum(ratingsFinal$combScore)- ratingsFinal$combScore))
ratingsFinal$SRSNew <- round(ratingsFinal$SRSNew, 2)

#========