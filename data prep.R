library(tidyverse)
library(tidyjson)

players = readr::read_csv("Downloads/pff/players.csv") 
meta = readr::read_csv("Downloads/pff/metadata.csv") %>%
  mutate(awayTeam = gsub("'", '"', awayTeam), #replace single quotes with double so R will parse the json
         homeTeam = gsub("'",'"',homeTeam),
         stadium = gsub("'",'"', stadium)) %>%
  rowwise() %>%
  mutate(awayTeamId = rjson::fromJSON(awayTeam)$id, #extract relevant fields
         awayTeamName = rjson::fromJSON(awayTeam)$name,
         awayTeamShort = rjson::fromJSON(awayTeam)$shortName,
         homeTeamId = rjson::fromJSON(homeTeam)$id,
         homeTeamName = rjson::fromJSON(homeTeam)$name,
         homeTeamShort = rjson::fromJSON(homeTeam)$shortName,
         stadiumId = rjson::fromJSON(stadium)$id,
         stadiumName = rjson::fromJSON(stadium)$name,
         pitchLength = rjson::fromJSON(stadium)$pitchLength,
         pitchWidth = rjson::fromJSON(stadium)$pitchWidth) %>%
  select(-c("awayTeamKit","competition","homeTeamKit","season","awayTeam","homeTeam","stadium")) %>%
  filter(homeTeamName %in% c("France", "Argentina") | 
           awayTeamName %in% c("Argentina", "France") & 
           date < "2022-12-15") #filter to relevant teams before the specified date

rosters = readr::read_csv("Downloads/pff/rosters.csv") %>%
  filter(game_id %in% unique(meta$id)) %>% #get players in these games
  mutate(quotes = str_count(player, pattern = "'"), #watch out for people with single quotes in name
         player = gsub('[(\\)]',"",player)) %>% #get rid of extra escapes
  rowwise() %>%
  mutate(playerId = ifelse(quotes == 8, #if they don't have single quotes in their names
                           rjson::fromJSON(gsub("'", '"',player))$id, #pull the id as json
                           gsub("[^0-9.-]", "",str_split_1(player,",")[[1]])), #otherwise get creative
         playerName = ifelse(quotes == 8, #if they don't have single quotes in their names
                             rjson::fromJSON(gsub("'", '"',player))$nickname, #pull the name as json
                             substr(str_split_1(player,",")[[2]],15,nchar(str_split_1(player,",")[[2]])-2)),
         teamId = rjson::fromJSON(gsub("'", '"',team))$id,
         teamName = rjson::fromJSON(gsub("'", '"',team))$name) %>%
  select(-c("player", "team", "quotes")) %>%
  ungroup() %>%
  filter(teamName %in% c("France", "Argentina"))
#team france
france = left_join(rosters %>%
                     filter(teamName == "France") %>%
                     select(shirtNumber, playerName, playerId) %>% distinct(),
                   players %>% select(id, dob, height) %>% mutate(id = as.character(id)) %>% distinct(),
                   by = c("playerId" = "id"))
#france games
franceGames = meta %>% filter(homeTeamName == "France" | awayTeamName == "France") %>% 
  mutate(home = ifelse(homeTeamName == "France", TRUE, FALSE)) %>% #identify if home or away for pulling coordinates
  select(id, home, date)
france$team = "France"

#team argentina
argentina = left_join(rosters %>%
                        filter(teamName == "Argentina") %>%
                        select(shirtNumber, playerName, playerId) %>% distinct(),
                      players %>% select(id, dob, height) %>% mutate(id = as.character(id)) %>% distinct(),
                      by = c("playerId" = "id"))
argentina$team = "Argentina"
#argentina games
argentinaGames = meta %>% filter(homeTeamName == "Argentina" | awayTeamName == "Argentina") %>% 
  mutate(home = ifelse(homeTeamName == "Argentina", TRUE, FALSE)) %>% 
  select(id, home, date)
rm(list = c("players", "rosters")) #clean-up
players = rbind(france, argentina)

#i don't recommend trying this - had i figured out jq --stream for handling the large files,
#this would have been unnecessary
#these functions extract home/away player coordinates from the massive tracking files
homeLocations = function(chunkStart, chunkStop){
  file[chunkStart: chunkStop] %>% 
    spread_all() %>% 
    enter_object("homePlayers") %>% 
    gather_array() %>% 
    spread_all() %>%
    select(period, 
           periodElapsedTime, 
           periodGameClockTime, 
           game_event_id, 
           jerseyNum, x, y) %>% as.data.frame
}

awayLocations = function(chunkStart, chunkStop){
  file[chunkStart: chunkStop] %>% 
    spread_all() %>% 
    enter_object("awayPlayers") %>% 
    gather_array() %>% 
    spread_all() %>%
    select(period, 
           periodElapsedTime, 
           periodGameClockTime, 
           game_event_id, 
           jerseyNum, x, y) %>% as.data.frame
}

#loop through the relevant games (i didn't do this, my computer would have exploded)
for(i in 1:nrow(meta)){
  locations = data.frame(matrix(nrow = 0, ncol = 7)) #create an empty df
  fileLoc = paste("Downloads/pff/",meta$id[i],".jsonl.bz2", sep="") #find the tracking file
  file = readLines(bzfile(fileLoc)) #read the whole thing (really, don't)
  x = 0 #blank counter for chunking the file
  if(meta$id[[i]] %in% unique(franceGames$id)){ #check if it's team france
    if(franceGames[franceGames$id==meta$id[[i]],]$home){ #check if it's home/away
      while(x < length(file)){ #iterate through the file
        locations = rbind(locations, homeLocations(x+1, x+15000)) #pull locations 15,000 lines at a time
        x = x+15000
      }
    } else { #same thing, but for away games
      while(x < length(file)){
        locations = rbind(locations, awayLocations(x+1, x+15000))
        x = x+15000
      }
    }
  }
  if(meta$id[[i]] %in% unique(argentinaGames$id)){ #not sure why i separated games by team
    if(argentinaGames[argentinaGames$id==meta$id[[i]],]$home){
      while(x < length(file)){
        locations = rbind(locations, homeLocations(x+1, x+15000))
        x = x+15000
      }
      rm(file)
      locations$gameId = meta$id[[i]]
    } else {
      while(x < length(file)){
        locations = rbind(locations, awayLocations(x+1, x+15000))
        x = x+15000
      }
    }
  }
  rm(file) #get rid of it before the computer dies
  locations$gameId = meta$id[[i]] #write the gameId
  readr::write_csv(locations, paste("Downloads/pff/",meta$id[i],".csv",sep="")) #save the csv
}
## only managed to get through 4 games using R code
distances = data.frame(matrix(nrow = 0, ncol = 4))
# "locations" data is still stored for game 3849
locations = readr::read_csv("Downloads/pff/3849.csv")  %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "France")
distances = rbind(distances, locations)
#read game 3834
locations = readr::read_csv("Downloads/pff/3834.csv")  %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "France")
distances = rbind(distances, locations)
#read game 3816
locations = readr::read_csv("Downloads/pff/3816.csv")  %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "Argentina")
distances = rbind(distances, locations)
#read game 3819
locations = readr::read_csv("Downloads/pff/3819.csv")  %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "France")
distances = rbind(distances, locations)
# csv files below produced using CLI / jq - so much faster to produce
##read game 3835 
locations = readr::read_csv("Downloads/pff/3835.csv", 
                            col_names = c("period", "periodElapsedTime","periodGameClockTime","jerseyNum",
                                          "x","y")) %>%
  mutate(gameId = 3835) %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "Argentina")
distances = rbind(distances, locations)
#read game 3850
locations = readr::read_csv("Downloads/pff/3850.csv", 
                            col_names = c("period", "periodElapsedTime","periodGameClockTime","jerseyNum",
                                          "x","y")) %>%
  mutate(gameId = 3850) %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "Argentina")
distances = rbind(distances, locations)
#read game 10503
locations = readr::read_csv("Downloads/pff/10503.csv", 
                            col_names = c("period", "periodElapsedTime","periodGameClockTime","jerseyNum",
                                          "x","y")) %>%
  mutate(gameId = 10503) %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "Argentina")
distances = rbind(distances, locations)
#read game 10504
locations = readr::read_csv("Downloads/pff/10504.csv", 
                            col_names = c("period", "periodElapsedTime","periodGameClockTime","jerseyNum",
                                          "x","y")) %>%
  mutate(gameId = 10504) %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "France")
distances = rbind(distances, locations)
#read game 10511
locations = readr::read_csv("Downloads/pff/10511.csv", 
                            col_names = c("period", "periodElapsedTime","periodGameClockTime","jerseyNum",
                                          "x","y")) %>%
  mutate(gameId = 10511) %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "Argentina")
distances = rbind(distances, locations)
#read game 10513
locations = readr::read_csv("Downloads/pff/10513.csv", 
                            col_names = c("period", "periodElapsedTime","periodGameClockTime","jerseyNum",
                                          "x","y")) %>%
  mutate(gameId = 10513) %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "France")
distances = rbind(distances, locations)
#read game 10514
locations = readr::read_csv("Downloads/pff/10514.csv", 
                            col_names = c("period", "periodElapsedTime","periodGameClockTime","jerseyNum",
                                          "x","y")) %>%
  mutate(gameId = 10514) %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "Argentina")
distances = rbind(distances, locations)
#read game 10515
locations = readr::read_csv("Downloads/pff/10515.csv", 
                            col_names = c("period", "periodElapsedTime","periodGameClockTime","jerseyNum",
                                          "x","y")) %>%
  mutate(gameId = 10515) %>%
  group_by(jerseyNum, period) %>%
  mutate(xdif = (x - lag(x))^2,
         ydif = (y - lag(y))^2,
         distance = sqrt(xdif+ydif)) %>%
  ungroup() %>%
  group_by(jerseyNum) %>%
  summarize(distance = sum(distance, na.rm = T),
            gameId = first(gameId)) %>%
  ungroup() %>%
  mutate(team = "France")
distances = rbind(distances, locations)
distances = left_join(distances, meta %>% select(id, week, date), by = c("gameId" = "id"))
playerDistance = left_join(distances, players , by = c("jerseyNum" = "shirtNumber", "team" = "team")) %>%
  mutate(age = year(as.period(interval(start=dob, end="2022-12-14"))))
readr::write_csv(playerDistance,"Downloads/pff/final.csv")
