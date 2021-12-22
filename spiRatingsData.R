library(tidyverse)
library(gt)
spiRatings <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv")
spiRatings <- spiRatings %>% select(name, league, off, def, spi)

spiengprem <- spiRatings %>% filter(league == "Barclays Premier League")
spiengpremGT <- spiengprem %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("England Premier League", subtitle = "SPI Ratings") %>% opt_row_striping()  %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))

spiengchamp <- spiRatings %>% filter(league == 'English League Championship')
spiengchampGT <- spiengchamp %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("England Championship League", subtitle = "SPI Ratings") %>% opt_row_striping() %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))


spigermany <- spiRatings %>% filter(league == 'German Bundesliga')
spigermanyGT <- spigermany %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("German Bundesliga", subtitle = "SPI Ratings") %>% opt_row_striping() %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))


spibelgium <- spiRatings %>% filter(league == 'Belgian Jupiler League')
spibelgiumGT <- spibelgium %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("Belgian Jupiler League", subtitle = "SPI Ratings") %>% opt_row_striping() %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))


spifrance <- spiRatings %>% filter(league == 'French Ligue 1')
spifranceGT <- spifrance %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("French Ligue 1", subtitle = "SPI Ratings") %>% opt_row_striping() %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))


spigreece <- spiRatings %>% filter(league == 'Greek Super League')
spigreeceGT <- spigreece %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("Greek Super League", subtitle = "SPI Ratings") %>% opt_row_striping() %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))


spiitaly <- spiRatings %>% filter(league == 'Italy Serie A')
spiitalyGT <- spiitaly %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("Italy Serie A", subtitle = "SPI Ratings") %>% opt_row_striping() %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))


spineth <- spiRatings %>% filter(league == 'Dutch Eredivisie')
spinethGT <- spineth %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("Dutch Eredivisie", subtitle = "SPI Ratings") %>% opt_row_striping() %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))


spiportugal <- spiRatings %>% filter(league == 'Portuguese Liga')
spiportugalGT <- spiportugal %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("Portuguese Liga", subtitle = "SPI Ratings") %>% opt_row_striping() %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))


spiscotprem <- spiRatings %>% filter(league == 'Scottish Premiership')
spiscotpremGT <- spiscotprem %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("Scottish Premiership", subtitle = "SPI Ratings") %>% opt_row_striping() %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))


spispain <- spiRatings %>% filter(league == 'Spanish Primera Division')
spispainGT <- spispain %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("Spanish La Liga", subtitle = "SPI Ratings") %>% opt_row_striping() %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))


spiturkey <- spiRatings %>% filter(league == 'Turkish Turkcell Super Lig')
spiturkeyGT <- spiturkey %>% select(name, off, def, spi) %>% rename("Team" = name, "Offense" = off, "Defense" = def, "SPI"=spi) %>% gt() %>% tab_options(table.width = "65%") %>% tab_header("Turkish Super League", subtitle = "SPI Ratings") %>% opt_row_striping() %>% tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(columns = everything()))

