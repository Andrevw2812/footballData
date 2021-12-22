library(tidyverse)

# Get all League games - these need to be updated each time app is run

engprem <- read_csv("https://www.football-data.co.uk/mmz4281/2122/E0.csv")
engprem$Date <- as.Date(engprem$Date, "%d/%m/%Y")
engprem$LeagueName <- c("UK - Premiership")
engprem$LeagueCode <- "engprem"
engprem <- engprem %>% relocate(LeagueName, .before = Div)
engpremTeams <- c(unlist(engprem$HomeTeam), unlist(engprem$AwayTeam))
engpremTeams <- unique(engpremTeams) %>% sort()

#===== Ratings =====
engpremTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
engpremTeams <- engprem$HomeTeam %>% as.data.frame()
names(engpremTeams) <- "engpremTeam"
engpremTeams <- rbind(engpremTeams, engprem$AwayTeam) %>% unique() %>% arrange(engpremTeam)


getGoals <- function(x){
  engpremTeam <- x
  homeGoals <- engprem %>% filter(engprem$HomeTeam == engpremTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- engprem %>% filter(engprem$HomeTeam == engpremTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- engprem %>% filter(engprem$AwayTeam == engpremTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- engprem %>% filter(engprem$AwayTeam == engpremTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(engpremTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTableengprem <- sapply(engpremTeams$engpremTeam, getGoals, simplify = TRUE)
srsTableengprem <- t(srsTableengprem)

rownames(srsTableengprem) <- NULL
srsTableengprem <- srsTableengprem %>% as.data.frame()

srsTableengpremRatingsSum <- srsTableengprem %>% select(teamGoalDiff)
srsTableengpremRatingsSum <- as.double(srsTableengpremRatingsSum$teamGoalDiff) %>% sum()

srsTableengprem$teamRating <- as.double(srsTableengprem$teamGoalDiff)+(1/10)*(srsTableengpremRatingsSum-as.double(srsTableengprem$teamGoalDiff))
srsTableengprem$teamRating <- round(srsTableengprem$teamRating, 2)

srsTableengprem <- srsTableengprem %>% arrange(desc(teamRating))






#========



engchamp <- read_csv("https://www.football-data.co.uk/mmz4281/2122/E1.csv")
engchamp$Date <- as.Date(engchamp$Date, "%d/%m/%Y")
engchamp$LeagueName <- c("UK - Championship")
engchamp$LeagueCode <- "engchamp"
engchamp <- engchamp %>% relocate(LeagueName, .before = Div)
engchampTeams <- c(unlist(engchamp$HomeTeam), unlist(engchamp$AwayTeam))
engchampTeams <- unique(engchampTeams) %>% sort()
#===== Ratings =====
engchampTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
engchampTeams <- engchamp$HomeTeam %>% as.data.frame()
names(engchampTeams) <- "engchampTeam"
engchampTeams <- rbind(engchampTeams, engchamp$AwayTeam) %>% unique() %>% arrange(engchampTeam)


getGoals <- function(x){
  engchampTeam <- x
  homeGoals <- engchamp %>% filter(engchamp$HomeTeam == engchampTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- engchamp %>% filter(engchamp$HomeTeam == engchampTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- engchamp %>% filter(engchamp$AwayTeam == engchampTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- engchamp %>% filter(engchamp$AwayTeam == engchampTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(engchampTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTableengchamp <- sapply(engchampTeams$engchampTeam, getGoals, simplify = TRUE)
srsTableengchamp <- t(srsTableengchamp)

rownames(srsTableengchamp) <- NULL
srsTableengchamp <- srsTableengchamp %>% as.data.frame()

srsTableengchampRatingsSum <- srsTableengchamp %>% select(teamGoalDiff)
srsTableengchampRatingsSum <- as.double(srsTableengchampRatingsSum$teamGoalDiff) %>% sum()

srsTableengchamp$teamRating <- as.double(srsTableengchamp$teamGoalDiff)+(1/10)*(srsTableengchampRatingsSum-as.double(srsTableengchamp$teamGoalDiff))
srsTableengchamp$teamRating <- round(srsTableengchamp$teamRating, 2)

srsTableengchamp <- srsTableengchamp %>% arrange(desc(teamRating))

#========

scotprem <- read_csv("https://www.football-data.co.uk/mmz4281/2122/SC0.csv")
scotprem$Date <- as.Date(scotprem$Date, "%d/%m/%Y")
scotprem$LeagueName <- c("Scottish Premier League")
scotprem$LeagueCode <- "scotprem"
scotprem <- scotprem %>% relocate(LeagueName, .before = Div)
scotpremTeams <- c(unlist(scotprem$HomeTeam), unlist(scotprem$AwayTeam))
scotpremTeams <- unique(scotpremTeams) %>% sort()

#===== Ratings =====
scotpremTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
scotpremTeams <- scotprem$HomeTeam %>% as.data.frame()
names(scotpremTeams) <- "scotpremTeam"
scotpremTeams <- rbind(scotpremTeams, scotprem$AwayTeam) %>% unique() %>% arrange(scotpremTeam)


getGoals <- function(x){
  scotpremTeam <- x
  homeGoals <- scotprem %>% filter(scotprem$HomeTeam == scotpremTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- scotprem %>% filter(scotprem$HomeTeam == scotpremTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- scotprem %>% filter(scotprem$AwayTeam == scotpremTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- scotprem %>% filter(scotprem$AwayTeam == scotpremTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(scotpremTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTablescotprem <- sapply(scotpremTeams$scotpremTeam, getGoals, simplify = TRUE)
srsTablescotprem <- t(srsTablescotprem)

rownames(srsTablescotprem) <- NULL
srsTablescotprem <- srsTablescotprem %>% as.data.frame()

srsTablescotpremRatingsSum <- srsTablescotprem %>% select(teamGoalDiff)
srsTablescotpremRatingsSum <- as.double(srsTablescotpremRatingsSum$teamGoalDiff) %>% sum()

srsTablescotprem$teamRating <- as.double(srsTablescotprem$teamGoalDiff)+(1/10)*(srsTablescotpremRatingsSum-as.double(srsTablescotprem$teamGoalDiff))
srsTablescotprem$teamRating <- round(srsTablescotprem$teamRating, 2)

srsTablescotprem <- srsTablescotprem %>% arrange(desc(teamRating))

#========

germany <- read_csv("https://www.football-data.co.uk/mmz4281/2122/D1.csv")
germany$Date <- as.Date(germany$Date, "%d/%m/%Y")
germany$LeagueName <- c("Germany - Bundesliga 1")
germany$LeagueCode <- "germany"
germany <- germany %>% relocate(LeagueName, .before = Div)
germanyTeams <- c(unlist(germany$HomeTeam), unlist(germany$AwayTeam))
germanyTeams <- unique(germanyTeams) %>% sort()

#===== Ratings =====
germanyTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
germanyTeams <- germany$HomeTeam %>% as.data.frame()
names(germanyTeams) <- "germanyTeam"
germanyTeams <- rbind(germanyTeams, germany$AwayTeam) %>% unique() %>% arrange(germanyTeam)


getGoals <- function(x){
  germanyTeam <- x
  homeGoals <- germany %>% filter(germany$HomeTeam == germanyTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- germany %>% filter(germany$HomeTeam == germanyTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- germany %>% filter(germany$AwayTeam == germanyTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- germany %>% filter(germany$AwayTeam == germanyTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(germanyTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTablegermany <- sapply(germanyTeams$germanyTeam, getGoals, simplify = TRUE)
srsTablegermany <- t(srsTablegermany)

rownames(srsTablegermany) <- NULL
srsTablegermany <- srsTablegermany %>% as.data.frame()

srsTablegermanyRatingsSum <- srsTablegermany %>% select(teamGoalDiff)
srsTablegermanyRatingsSum <- as.double(srsTablegermanyRatingsSum$teamGoalDiff) %>% sum()

srsTablegermany$teamRating <- as.double(srsTablegermany$teamGoalDiff)+(1/10)*(srsTablegermanyRatingsSum-as.double(srsTablegermany$teamGoalDiff))
srsTablegermany$teamRating <- round(srsTablegermany$teamRating, 2)

srsTablegermany <- srsTablegermany %>% arrange(desc(teamRating))

#========


italy <- read_csv("https://www.football-data.co.uk/mmz4281/2122/I1.csv")
italy$Date <- as.Date(italy$Date, "%d/%m/%Y")
italy$LeagueName <- c("Italy - Serie A")
italy$LeagueCode <- "italy"
italy <- italy %>% relocate(LeagueName, .before = Div)
italyTeams <- c(unlist(italy$HomeTeam), unlist(italy$AwayTeam))
italyTeams <- unique(italyTeams) %>% sort()

#===== Ratings =====
italyTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
italyTeams <- italy$HomeTeam %>% as.data.frame()
names(italyTeams) <- "italyTeam"
italyTeams <- rbind(italyTeams, italy$AwayTeam) %>% unique() %>% arrange(italyTeam)


getGoals <- function(x){
  italyTeam <- x
  homeGoals <- italy %>% filter(italy$HomeTeam == italyTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- italy %>% filter(italy$HomeTeam == italyTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- italy %>% filter(italy$AwayTeam == italyTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- italy %>% filter(italy$AwayTeam == italyTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(italyTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTableitaly <- sapply(italyTeams$italyTeam, getGoals, simplify = TRUE)
srsTableitaly <- t(srsTableitaly)

rownames(srsTableitaly) <- NULL
srsTableitaly <- srsTableitaly %>% as.data.frame()

srsTableitalyRatingsSum <- srsTableitaly %>% select(teamGoalDiff)
srsTableitalyRatingsSum <- as.double(srsTableitalyRatingsSum$teamGoalDiff) %>% sum()

srsTableitaly$teamRating <- as.double(srsTableitaly$teamGoalDiff)+(1/10)*(srsTableitalyRatingsSum-as.double(srsTableitaly$teamGoalDiff))
srsTableitaly$teamRating <- round(srsTableitaly$teamRating, 2)

srsTableitaly <- srsTableitaly %>% arrange(desc(teamRating))

#========


spain <- read_csv("https://www.football-data.co.uk/mmz4281/2122/SP1.csv")
spain$Date <- as.Date(spain$Date, "%d/%m/%Y")
spain$LeagueName <- c("Spain - La Liga Primera")
spain$LeagueCode <- "spain"
spain <- spain %>% relocate(LeagueName, .before = Div)
spainTeams <- c(unlist(spain$HomeTeam), unlist(spain$AwayTeam))
spainTeams <- unique(spainTeams) %>% sort()

#===== Ratings =====
spainTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
spainTeams <- spain$HomeTeam %>% as.data.frame()
names(spainTeams) <- "spainTeam"
spainTeams <- rbind(spainTeams, spain$AwayTeam) %>% unique() %>% arrange(spainTeam)


getGoals <- function(x){
  spainTeam <- x
  homeGoals <- spain %>% filter(spain$HomeTeam == spainTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- spain %>% filter(spain$HomeTeam == spainTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- spain %>% filter(spain$AwayTeam == spainTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- spain %>% filter(spain$AwayTeam == spainTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(spainTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTablespain <- sapply(spainTeams$spainTeam, getGoals, simplify = TRUE)
srsTablespain <- t(srsTablespain)

rownames(srsTablespain) <- NULL
srsTablespain <- srsTablespain %>% as.data.frame()

srsTablespainRatingsSum <- srsTablespain %>% select(teamGoalDiff)
srsTablespainRatingsSum <- as.double(srsTablespainRatingsSum$teamGoalDiff) %>% sum()

srsTablespain$teamRating <- as.double(srsTablespain$teamGoalDiff)+(1/10)*(srsTablespainRatingsSum-as.double(srsTablespain$teamGoalDiff))
srsTablespain$teamRating <- round(srsTablespain$teamRating, 2)

srsTablespain <- srsTablespain %>% arrange(desc(teamRating))

#========

france <- read_csv("https://www.football-data.co.uk/mmz4281/2122/F1.csv")
france$Date <- as.Date(france$Date, "%d/%m/%Y")
france$LeagueName <- c("France - Le Championnat")
france$LeagueCode <- "france"
france <- france %>% relocate(LeagueName, .before = Div)
franceTeams <- c(unlist(france$HomeTeam), unlist(france$AwayTeam))
franceTeams <- unique(franceTeams) %>% sort()

#===== Ratings =====
franceTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
franceTeams <- france$HomeTeam %>% as.data.frame()
names(franceTeams) <- "franceTeam"
franceTeams <- rbind(franceTeams, france$AwayTeam) %>% unique() %>% arrange(franceTeam)


getGoals <- function(x){
  franceTeam <- x
  homeGoals <- france %>% filter(france$HomeTeam == franceTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- france %>% filter(france$HomeTeam == franceTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- france %>% filter(france$AwayTeam == franceTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- france %>% filter(france$AwayTeam == franceTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(franceTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTablefrance <- sapply(franceTeams$franceTeam, getGoals, simplify = TRUE)
srsTablefrance <- t(srsTablefrance)

rownames(srsTablefrance) <- NULL
srsTablefrance <- srsTablefrance %>% as.data.frame()

srsTablefranceRatingsSum <- srsTablefrance %>% select(teamGoalDiff)
srsTablefranceRatingsSum <- as.double(srsTablefranceRatingsSum$teamGoalDiff) %>% sum()

srsTablefrance$teamRating <- as.double(srsTablefrance$teamGoalDiff)+(1/10)*(srsTablefranceRatingsSum-as.double(srsTablefrance$teamGoalDiff))
srsTablefrance$teamRating <- round(srsTablefrance$teamRating, 2)

srsTablefrance <- srsTablefrance %>% arrange(desc(teamRating))

#========

neth <- read_csv("https://www.football-data.co.uk/mmz4281/2122/N1.csv")
neth$Date <- as.Date(neth$Date, "%d/%m/%Y")
neth$LeagueName <- c("Netherlands - Eredivisie")
neth$LeagueCode <- "neth"
neth <- neth %>% relocate(LeagueName, .before = Div)
nethTeams <- c(unlist(neth$HomeTeam), unlist(neth$AwayTeam))
nethTeams <- unique(nethTeams) %>% sort()

#===== Ratings =====
nethTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
nethTeams <- neth$HomeTeam %>% as.data.frame()
names(nethTeams) <- "nethTeam"
nethTeams <- rbind(nethTeams, neth$AwayTeam) %>% unique() %>% arrange(nethTeam)


getGoals <- function(x){
  nethTeam <- x
  homeGoals <- neth %>% filter(neth$HomeTeam == nethTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- neth %>% filter(neth$HomeTeam == nethTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- neth %>% filter(neth$AwayTeam == nethTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- neth %>% filter(neth$AwayTeam == nethTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(nethTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTableneth <- sapply(nethTeams$nethTeam, getGoals, simplify = TRUE)
srsTableneth <- t(srsTableneth)

rownames(srsTableneth) <- NULL
srsTableneth <- srsTableneth %>% as.data.frame()

srsTablenethRatingsSum <- srsTableneth %>% select(teamGoalDiff)
srsTablenethRatingsSum <- as.double(srsTablenethRatingsSum$teamGoalDiff) %>% sum()

srsTableneth$teamRating <- as.double(srsTableneth$teamGoalDiff)+(1/10)*(srsTablenethRatingsSum-as.double(srsTableneth$teamGoalDiff))
srsTableneth$teamRating <- round(srsTableneth$teamRating, 2)

srsTableneth <- srsTableneth %>% arrange(desc(teamRating))

#========

belgium <- read_csv("https://www.football-data.co.uk/mmz4281/2122/B1.csv")
belgium$Date <- as.Date(belgium$Date, "%d/%m/%Y")
belgium$LeagueName <- c("Belgium - Jupiler League")
belgium$LeagueCode <- "belgium"
belgium <- belgium %>% relocate(LeagueName, .before = Div)
belgiumTeams <- c(unlist(belgium$HomeTeam), unlist(belgium$AwayTeam))
belgiumTeams <- unique(belgiumTeams) %>% sort()
#===== Ratings =====
belgiumTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
belgiumTeams <- belgium$HomeTeam %>% as.data.frame()
names(belgiumTeams) <- "belgiumTeam"
belgiumTeams <- rbind(belgiumTeams, belgium$AwayTeam) %>% unique() %>% arrange(belgiumTeam)


getGoals <- function(x){
  belgiumTeam <- x
  homeGoals <- belgium %>% filter(belgium$HomeTeam == belgiumTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- belgium %>% filter(belgium$HomeTeam == belgiumTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- belgium %>% filter(belgium$AwayTeam == belgiumTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- belgium %>% filter(belgium$AwayTeam == belgiumTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(belgiumTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTablebelgium <- sapply(belgiumTeams$belgiumTeam, getGoals, simplify = TRUE)
srsTablebelgium <- t(srsTablebelgium)

rownames(srsTablebelgium) <- NULL
srsTablebelgium <- srsTablebelgium %>% as.data.frame()

srsTablebelgiumRatingsSum <- srsTablebelgium %>% select(teamGoalDiff)
srsTablebelgiumRatingsSum <- as.double(srsTablebelgiumRatingsSum$teamGoalDiff) %>% sum()

srsTablebelgium$teamRating <- as.double(srsTablebelgium$teamGoalDiff)+(1/10)*(srsTablebelgiumRatingsSum-as.double(srsTablebelgium$teamGoalDiff))
srsTablebelgium$teamRating <- round(srsTablebelgium$teamRating, 2)

srsTablebelgium <- srsTablebelgium %>% arrange(desc(teamRating))

#========


portugal <- read_csv("https://www.football-data.co.uk/mmz4281/2122/P1.csv")
portugal$Date <- as.Date(portugal$Date, "%d/%m/%Y")
portugal$LeagueName <- c("Portugal - Liga I")
portugal$LeagueCode <- "portugal"
portugal <- portugal %>% relocate(LeagueName, .before = Div)
portugalTeams <- c(unlist(portugal$HomeTeam), unlist(portugal$AwayTeam))
portugalTeams <- unique(portugalTeams) %>% sort()

#===== Ratings =====
portugalTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
portugalTeams <- portugal$HomeTeam %>% as.data.frame()
names(portugalTeams) <- "portugalTeam"
portugalTeams <- rbind(portugalTeams, portugal$AwayTeam) %>% unique() %>% arrange(portugalTeam)


getGoals <- function(x){
  portugalTeam <- x
  homeGoals <- portugal %>% filter(portugal$HomeTeam == portugalTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- portugal %>% filter(portugal$HomeTeam == portugalTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- portugal %>% filter(portugal$AwayTeam == portugalTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- portugal %>% filter(portugal$AwayTeam == portugalTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(portugalTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTableportugal <- sapply(portugalTeams$portugalTeam, getGoals, simplify = TRUE)
srsTableportugal <- t(srsTableportugal)

rownames(srsTableportugal) <- NULL
srsTableportugal <- srsTableportugal %>% as.data.frame()

srsTableportugalRatingsSum <- srsTableportugal %>% select(teamGoalDiff)
srsTableportugalRatingsSum <- as.double(srsTableportugalRatingsSum$teamGoalDiff) %>% sum()

srsTableportugal$teamRating <- as.double(srsTableportugal$teamGoalDiff)+(1/10)*(srsTableportugalRatingsSum-as.double(srsTableportugal$teamGoalDiff))
srsTableportugal$teamRating <- round(srsTableportugal$teamRating, 2)

srsTableportugal <- srsTableportugal %>% arrange(desc(teamRating))

#========

turkey <- read_csv("https://www.football-data.co.uk/mmz4281/2122/T1.csv")
turkey$Date <- as.Date(turkey$Date, "%d/%m/%Y")
turkey$LeagueName <- c("Turkey - Futbol Ligi 1")
turkey$LeagueCode <- "turkey"
turkey <- turkey %>% relocate(LeagueName, .before = Div)
turkeyTeams <- c(unlist(turkey$HomeTeam), unlist(turkey$AwayTeam))
turkeyTeams <- unique(turkeyTeams) %>% sort()
#===== Ratings =====
turkeyTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
turkeyTeams <- turkey$HomeTeam %>% as.data.frame()
names(turkeyTeams) <- "turkeyTeam"
turkeyTeams <- rbind(turkeyTeams, turkey$AwayTeam) %>% unique() %>% arrange(turkeyTeam)


getGoals <- function(x){
  turkeyTeam <- x
  homeGoals <- turkey %>% filter(turkey$HomeTeam == turkeyTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- turkey %>% filter(turkey$HomeTeam == turkeyTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- turkey %>% filter(turkey$AwayTeam == turkeyTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- turkey %>% filter(turkey$AwayTeam == turkeyTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(turkeyTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTableturkey <- sapply(turkeyTeams$turkeyTeam, getGoals, simplify = TRUE)
srsTableturkey <- t(srsTableturkey)

rownames(srsTableturkey) <- NULL
srsTableturkey <- srsTableturkey %>% as.data.frame()

srsTableturkeyRatingsSum <- srsTableturkey %>% select(teamGoalDiff)
srsTableturkeyRatingsSum <- as.double(srsTableturkeyRatingsSum$teamGoalDiff) %>% sum()

srsTableturkey$teamRating <- as.double(srsTableturkey$teamGoalDiff)+(1/10)*(srsTableturkeyRatingsSum-as.double(srsTableturkey$teamGoalDiff))
srsTableturkey$teamRating <- round(srsTableturkey$teamRating, 2)

srsTableturkey <- srsTableturkey %>% arrange(desc(teamRating))

#========

greece <- read_csv("https://www.football-data.co.uk/mmz4281/2122/G1.csv")
greece$Date <- as.Date(greece$Date, "%d/%m/%Y")
greece$LeagueName <- c("Greece - Ethniki Katigoria")
greece$LeagueCode <- "greece"
greece <- greece %>% relocate(LeagueName, .before = Div)
greeceTeams <- c(unlist(greece$HomeTeam), unlist(greece$AwayTeam))
greeceTeams <- unique(greeceTeams) %>% sort()


#===== Ratings =====
greeceTeams <- data.frame(matrix(data = NA, ncol = 1), stringsAsFactors = FALSE)
greeceTeams <- greece$HomeTeam %>% as.data.frame()
names(greeceTeams) <- "greeceTeam"
greeceTeams <- rbind(greeceTeams, greece$AwayTeam) %>% unique() %>% arrange(greeceTeam)


getGoals <- function(x){
  greeceTeam <- x
  homeGoals <- greece %>% filter(greece$HomeTeam == greeceTeam) %>% select(FTHG) %>% sum()
  homeGDiff <- greece %>% filter(greece$HomeTeam == greeceTeam)
  homeGDiff <- homeGDiff$FTHG-homeGDiff$FTAG
  homeGDiff <- homeGDiff %>% mean()
  awayGDiff <- greece %>% filter(greece$AwayTeam == greeceTeam)
  awayGDiff <- awayGDiff$FTHG-awayGDiff$FTAG
  awayGDiff <- awayGDiff %>% mean()
  teamGoalDiff <- round(mean(homeGDiff,awayGDiff), 2)
  awayGoals <- greece %>% filter(greece$AwayTeam == greeceTeam) %>% select(FTAG) %>% sum()
  totalGoals <- homeGoals+awayGoals
  teamData <- data.frame(greeceTeam, totalGoals, teamGoalDiff, stringsAsFactors = FALSE)
  return(teamData)
}

srsTablegreece <- sapply(greeceTeams$greeceTeam, getGoals, simplify = TRUE)
srsTablegreece <- t(srsTablegreece)

rownames(srsTablegreece) <- NULL
srsTablegreece <- srsTablegreece %>% as.data.frame()

srsTablegreeceRatingsSum <- srsTablegreece %>% select(teamGoalDiff)
srsTablegreeceRatingsSum <- as.double(srsTablegreeceRatingsSum$teamGoalDiff) %>% sum()

srsTablegreece$teamRating <- as.double(srsTablegreece$teamGoalDiff)+(1/10)*(srsTablegreeceRatingsSum-as.double(srsTablegreece$teamGoalDiff))
srsTablegreece$teamRating <- round(srsTablegreece$teamRating, 2)

srsTablegreece <- srsTablegreece %>% arrange(desc(teamRating))

#========

mainLeagues <- c(engprem$LeagueName, engchamp$LeagueName, scotprem$LeagueName, germany$LeagueName, italy$LeagueName, spain$LeagueName, france$LeagueName, neth$LeagueName, belgium$LeagueName, portugal$LeagueName, turkey$LeagueName, greece$LeagueName) %>% unique()

rm(leagueOverviewAll)
leagueOverviewAll <- data.frame(matrix(data = NA, nrow = 1, ncol = 15))
names(leagueOverviewAll) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")


# Gather League data
# Belgium
leagueSelected <- belgium
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)




leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview) %>% na.omit()

# Eng Champ
leagueSelected <- engchamp
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)

leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview) 

# Eng Prem
leagueSelected <- engprem
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)

leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview) 

# France
leagueSelected <- france
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)

leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview) 

# Germany
leagueSelected <- germany
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)

leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview) 

# Greece
leagueSelected <- greece
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)

leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview) 

# Italy
leagueSelected <- italy
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)

leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview) 

# Netherlands
leagueSelected <- neth
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)

leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview) 

# Portugal
leagueSelected <- portugal
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)

leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview) 

# Scot Prem
leagueSelected <- scotprem
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)

leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview) 

# Spain
leagueSelected <- spain
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)

leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview) 

# Turkey
leagueSelected <- turkey
leagueSelectedName <- unique(leagueSelected$LeagueCode)

# Home
leagueHPlayed <- as.data.frame(leagueSelected) %>% nrow()
leagueHGoalsAve <- round(mean(leagueSelected$FTHG), 2)
leagueHGoalsConcAve <- round(mean(leagueSelected$FTAG), 2)
leagueHTHGoalsAve <- round(mean(leagueSelected$HTHG), 2)
leagueHTHGoalsConcAve <- round(mean(leagueSelected$HTAG), 2)
leagueSHHGoalsAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)
leagueSHHGoalsConcAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
# Away
leagueAPlayed <- leagueHPlayed
leagueAGoalsAve <- round(mean(leagueSelected$FTAG), 2)
leagueAGoalsConcAve <- round(mean(leagueSelected$FTHG), 2)
leagueHTAGoalsAve <- round(mean(leagueSelected$HTAG), 2)
leagueHTAGoalsConcAve <- round(mean(leagueSelected$HTHG), 2)
leagueSHAGoalsAve <- round(mean(leagueSelected$FTAG-leagueSelected$HTAG), 2)
leagueSHAGoalsConcAve <- round(mean(leagueSelected$FTHG-leagueSelected$HTHG), 2)

leagueOverview <- data.frame(matrix(data = c(leagueSelectedName, leagueHPlayed, leagueHGoalsAve,leagueHGoalsConcAve,leagueHTHGoalsAve,leagueHTHGoalsConcAve,leagueSHHGoalsAve,leagueSHHGoalsConcAve, leagueAPlayed, leagueAGoalsAve,leagueAGoalsConcAve,leagueHTAGoalsAve,leagueHTAGoalsConcAve, leagueSHAGoalsAve,leagueSHAGoalsConcAve), nrow = 1, ncol = 15))
names(leagueOverview) <- c("leagueSelectedName", "leagueHPlayed","leagueHGoalsAve","leagueHGoalsConcAve","leagueHTHGoalsAve","leagueHTHGoalsConcAve","leagueSHHGoalsAve","leagueSHHGoalsConcAve", "leagueAPlayed", "leagueAGoalsAve","leagueAGoalsConcAve","leagueHTAGoalsAve","leagueHTAGoalsConcAve", "leagueSHAGoalsAve","leagueSHAGoalsConcAve")

leagueOverviewAll <- bind_rows(leagueOverviewAll, leagueOverview)


leagueOverviewengprem <- leagueOverviewAll %>% filter(leagueSelectedName == "engprem")
leagueengpremTbl <- leagueOverviewengprem %>% select(leagueHPlayed, leagueHGoalsAve, leagueHGoalsConcAve, leagueHTHGoalsAve, leagueHTHGoalsConcAve, leagueSHHGoalsAve, leagueSHHGoalsConcAve, leagueAGoalsAve, leagueAGoalsConcAve, leagueHTAGoalsAve, leagueHTAGoalsConcAve, leagueSHAGoalsAve, leagueSHAGoalsConcAve)

leagueengpremTbl <- leagueengpremTbl %>% rename("Total League Games" = leagueHPlayed, "HG Ave" = leagueHGoalsAve, "H Conceded Goals" = leagueHGoalsConcAve, "HTHG Ave" = leagueHTHGoalsAve, "H Conceded HTG" = leagueHTHGoalsConcAve, "HSHG Ave" = leagueSHHGoalsAve, "HSHG Conceded" = leagueSHHGoalsConcAve, "AG Ave" = leagueAGoalsAve, "A Conceded Goals" = leagueAGoalsConcAve, "HTAG Ave" = leagueHTAGoalsAve, "A Conceded HTG" = leagueHTAGoalsConcAve, "ASHG Ave" = leagueSHAGoalsAve, "ASHG Conceded" = leagueSHAGoalsConcAve)

leagueengpremTblGT <<- leagueengpremTbl %>% gt() %>% tab_spanner(label = "Home Team League Data", columns = c("HG Ave", "H Conceded Goals","HTHG Ave", "H Conceded HTG","HSHG Ave", "HSHG Conceded"))  %>% tab_spanner(label = "Away Team League Data", columns = c("AG Ave", "A Conceded Goals","HTAG Ave", "A Conceded HTG","ASHG Ave", "ASHG Conceded")) %>% tab_style(style = cell_fill(color = "#BFD6FA"), locations = cells_body(columns = c("AG Ave", "A Conceded Goals","HTAG Ave", "A Conceded HTG","ASHG Ave", "ASHG Conceded"))) %>% tab_options(table.width = "95%") %>% cols_align(align = "center", columns = everything())

# Other Leagues

argentina <- read_csv("https://www.football-data.co.uk/new/ARG.csv")
argentina$Date <- as.Date(argentina$Date, "%d/%m/%Y")
austria <- read_csv("https://www.football-data.co.uk/new/AUT.csv")
brazil <- read_csv("https://www.football-data.co.uk/new/BRA.csv")
china <- read_csv("https://www.football-data.co.uk/new/CHN.csv")
denmark <- read_csv("https://www.football-data.co.uk/new/DNK.csv")
finland <- read_csv("https://www.football-data.co.uk/new/FIN.csv")
ireland <- read_csv("https://www.football-data.co.uk/new/IRL.csv")
japan <- read_csv("https://www.football-data.co.uk/new/JPN.csv")
norway <- read_csv("https://www.football-data.co.uk/new/NOR.csv")
russia <- read_csv("https://www.football-data.co.uk/new/RUS.csv")
sweden <- read_csv("https://www.football-data.co.uk/new/SWE.csv")
switzerland <- read_csv("https://www.football-data.co.uk/new/SWZ.csv")
usa <- read_csv("https://www.football-data.co.uk/new/USA.csv")
