library(plotly)
library(htmltools)
library(jsonlite)
library(httr)
library(tidyverse)
library(shiny)

library(googledrive)
library(googlesheets4)

gs4_deauth()

#currently much of the data cleaning is being done in a google sheet that was previously used and brought into a data sheet using the code below.
# link to blank version of the google sheet https://docs.google.com/spreadsheets/d/1XV8zApE440LB6JbveOC7fTlbIizpyhZns3B6XPeu8Cw/edit?usp=sharing

#import and normalize a google sheet containing data for full season
scoutData <- data.frame(range_read("link to a google sheet containing data for full season"))
scoutData <- scoutData %>% filter(!is.na(robot))
rangeAM  <- 3
rangeANS <- max(scoutData[3])
rangeTNS <- max(scoutData[4])
rangeCST <- 3
rangeDS  <- 5
rangeC   <- 5
selection <- c(2:7)
ranges <- c(rangeAM, rangeANS, rangeTNS,  rangeCST, rangeDS, rangeC)

for (i in 1:length(ranges)){
  x <- selection[i]
  scoutData[x] <- scoutData[x] / ranges[i]
}

#import and normalize a google sheet containing data for Bayou Regional
scoutDataB <- data.frame(range_read("link to a google sheet containing data for Bayou Regional"))
scoutDataB <- scoutDataB %>% filter(!is.na(robot))
rangeAM  <- 3
rangeANS <- max(scoutDataB[3])
rangeTNS <- max(scoutDataB[4])
rangeCST <- 3
rangeDS  <- 5
rangeC   <- 5
selection <- c(2:7)
ranges <- c(rangeAM, rangeANS, rangeTNS,  rangeCST, rangeDS, rangeC)

for (i in 1:length(ranges)){
  x <- selection[i]
  scoutDataB[x] <- scoutDataB[x] / ranges[i]
}

#import and normalize a google sheet containing data for Magnolia Regional
scoutDataM <- data.frame(range_read("link to a google sheet containing data for Magnolia Regional"))
scoutDataM <- scoutDataM %>% filter(!is.na(robot))
rangeAM  <- 3
rangeANS <- max(scoutDataM[3])
rangeTNS <- max(scoutDataM[4])
rangeCST <- 3
rangeDS  <- 5
rangeC   <- 5
selection <- c(2:7)
ranges <- c(rangeAM, rangeANS, rangeTNS,  rangeCST, rangeDS, rangeC)

for (i in 1:length(ranges)){
  x <- selection[i]
  scoutDataM[x] <- scoutDataM[x] / ranges[i]
}

#imports comments from google sheet
comData <- data.frame(range_read("link to a google sheet containing data for Full Season"))
comData <- comData[order(as.numeric(comData$robot)),]

comDataM <- data.frame(range_read("link to a google sheet containing data for Magnolia Regional"))
comDataM <- comDataM[order(as.numeric(comDataM$robot)),]

comDataB <- data.frame(range_read("link to a google sheet containing data for Bayou Regional"))
comDataB <- comDataB[order(as.numeric(comDataB$robot)),]


#pulls opr data from the Blue Alliance api
oprDataM <- getEventOprs3("2023mslr")

oprDataM <- oprDataM[order(as.numeric(oprDataM$name)),]
oprDataM[2] <- oprDataM[2]/max(oprDataM[2])


oprDataB <- getEventOprs3("2023lake")

oprDataB <- oprDataB[order(as.numeric(oprDataB$name)),]
oprDataB[2] <- oprDataB[2]/max(oprDataB[2])

oprData <- rbind(oprDataM, oprDataB)
oprData[2] <- oprData[2]/max(oprData[2])
oprData <- data.frame("name"=c(oprData$name),
                      "value"=c(oprData$value))
oprData <- oprData %>% group_by(name) %>% summarize(value=mean(value))
oprData <- oprData[order(as.numeric(oprData$name)),]

#pulls opponent info from the Blue Alliance api
oppDataM<- as.numeric(gsub('frc','',getEventTeamsKeys(2023,"mslr")))
oppDataM<- oppDataM[order(oppDataM)]

oppDataB<- as.numeric(gsub('frc','',getEventTeamsKeys(2023,"lake")))
oppDataB<- oppDataB[order(oppDataB)]

oppData<- getTeamOpponents(2023,8044)




oppName<- as_tibble(as.character(oppDataM))
oppName$name = NA
for(i in 1:length(oppName$value)){
  key <- getTeamData(oppName$value[i])
  oppName$name[i] <- key$nickname
}
oppName2<- as_tibble(as.character(oppDataB))
oppName2$name = NA
for(i in 1:length(oppName2$value)){
  key <- getTeamData(oppName2$value[i])
  oppName2$name[i] <- key$nickname
}
oppName3<- c(oppDataM,oppDataB)
oppName3<- oppName3[order(oppName3)]
oppName3<- as_tibble(as.character(oppName3))
oppName3$name = NA
for(i in 1:length(oppName3$value)){
  key <- getTeamData(oppName3$value[i])
  oppName3$name[i] <- key$nickname
}
oppName3<- distinct(oppName3)


################################################################################
getFRCData <- function(url) {
  req <- httr::GET(url, httr::add_headers("X-TBA-Auth-Key" = "your own api auth key"))
  json <- httr::content(req, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(json)
  return(data)
} #Pulls dataset from Blue Alliance api.

################################################################################
getEvents <- function(year) {
  url <- paste(c("https://www.thebluealliance.com/api/v3/events/",
                 year),
               collapse = "")
  return(getFRCData(url))
} #Lists all events for given year

################################################################################
getEventMatches <- function(year, event_code) {
  url <- paste(c("https://www.thebluealliance.com/api/v3/event/",
                 year,
                 event_code,
                 "/matches"),
               collapse = "")
  return(getFRCData(url))
} #Lists all Matches for a given event.
getEventOprs <- function(year, event_code) {
  url <- paste(c("https://www.thebluealliance.com/api/v3/event/",
                 year,
                 event_code,
                 "/oprs"),
               collapse = "")
  data1 <- as_tibble(getFRCData(url), rownames = "teams")
  data2 <- c(data1$oprs) 
  return(data2)
} #Lists each teams OPR for a given event.
getEventOprs2 <- function(event_code) {
  url <- paste(c("https://www.thebluealliance.com/api/v3/event/",
                 event_code,
                 "/oprs"),
               collapse = "")
  data1 <- as_tibble(getFRCData(url), rownames = "teams")
  data2 <- c(data1$oprs) 
  return(data2)
} #GetEventOprs with a different parameter format.
getEventOprs3 <- function(event_code) {
  url <- paste(c("https://www.thebluealliance.com/api/v3/event/",
                 event_code,
                 "/oprs"),
               collapse = "")
  data1 <- as_tibble(getFRCData(url), rownames = "teams")
  data2 <- data.frame(data1$oprs) 
  data3 <- data2 %>% pivot_longer(cols = everything())
  data3[[1]] <- gsub('frc','',data3[[1]])
  return(data3)
} #GetEventOprs with a different parameter format.
################################################################################
getEventTeamsKeys <- function(year, event_code) {
  url <- paste(c("https://www.thebluealliance.com/api/v3/event/",
                 year,
                 event_code,
                 "/teams/keys"),
               collapse = "")
  return(getFRCData(url))
} #Lists team Key for a given event.

################################################################################
getTeamsPage <- function(page) {
  url <- paste(c("https://www.thebluealliance.com/api/v3/teams/",
                 page),
               collapse = "")
  print(url)
  return(getFRCData(url))
} #Requests data used for getTeams.
getTeams <- function() {
  page = 0
  teams = tibble()
  while(TRUE) {
    new_teams <- getTeamsPage(page)
    if(length(new_teams) == 0) {
      break
    }
    teams <- bind_rows(teams, new_teams)
    page <- page + 1
  }
  return(teams)
} #Generates a table with all FRC teams.

################################################################################
getTeamData <- function(team) {
  url <- paste(c("https://www.thebluealliance.com/api/v3/team/frc",
                 team),
               collapse = "")
  return(getFRCData(url))
} #Gives Blue Allaince profile information on requested team.

################################################################################
getTeamMatches <- function(year, team) {
  url <- paste(c("https://www.thebluealliance.com/api/v3/team/frc",
                 team,"/matches/",year),
               collapse = "")
  return(getFRCData(url))
} #Lists full match information for team in a given year.
getTeamMatchesSimple <- function(year, team) {
  url <- paste(c("https://www.thebluealliance.com/api/v3/team/frc",
                 team,"/matches/",year,"/simple"),
               collapse = "")
  return(getFRCData(url))
} #Lists abridged important match information for a team in a given year.
getTeamEvents <- function(year, team){
  url <- paste(c("https://www.thebluealliance.com/api/v3/team/frc",
                 team,"/events/",year),
               collapse = "")
  return(getFRCData(url))
} #Lists full event information for team in a given year.
getTeamEventForOpr <- function(year, team){
  data1 <- distinct(getTeamMatchesSimple(year, team) %>%
                      select("event_key"))
  data2 <- getEvents(year) %>%
    select(event_type_string, key, name)
  data3 <- data1 %>%
    left_join(data2, by = c("event_key" = "key"))
  return(data3)
  
} #Lists events given team participated in, formated for getTeamOpr.
getTeamOpr <- function(year, team){
  key <- paste(c("frc",team),collapse = "")
  data1 <- getTeamEventForOpr(year,team)
  opr <- vector("character",1)
  for (i in 1:length(data1$event_key)){
    eventKey <- data1$event_key[i]
    opr[[i]] <- getEventOprs2(eventKey)[[key]]
  }
  data2 <- data1 %>% mutate("OPR" = opr)
  return(data2)
} #lists given teams opr at each event during a given year.
getTeamEventOpr <- function(year, team, event_code){
  key1 <- paste(c("frc",team),collapse = "")
  key2 <- paste(c(year, event_code),collapse = "")
  data1 <- getTeamEventForOpr(year,team)
  opr <- getEventOprs(year, event_code)[[key1]]
  data2 <- data1 %>% mutate("OPR" = opr)
  data3 <- data2 %>%
    filter(event_key == key2)
  return(data3)
} #Returns a given teams opr for a given event.
getTeamEventOpr2 <- function(year, team, event_code){
  key1 <- paste(c("frc",team),collapse = "")
  key2 <- paste(c(year, event_code),collapse = "")
  data1 <- getTeamEventForOpr(year,team)
  opr <- getEventOprs(year, event_code)[[key1]]
  data2 <- data1 %>% mutate("OPR" = opr)
  data3 <- data2 %>%
    filter(event_key == key2)
  return(opr)
} #Returns a given teams opr for a given event.
################################################################################
teamTest <- function(vec,team) {
  flag <- FALSE
  for (i in 1:length(vec)){
    if(vec[i] == paste(c("frc",team),collapse = "")){
      flag <- TRUE
    }
  }
  return(flag)
} #formatting for GetTeamMatchesCustom
teamColor <- function(data,row,team){
  color <- "fail"
  if(teamTest(data$alliances$blue$team_keys[[row]],team) == TRUE){
    color <- "blue"
  }
  if(teamTest(data$alliances$red$team_keys[[row]],team) == TRUE){
    color <- "red"
  }
  return(color)
} #formatting for GetTeamMatchesCustom
addWin <- function(data,team){
  data2 <- data %>% mutate("win" = as.logical(data[[1]]))
  for (i in 1:length(data2[[1]])){
    if(teamColor(data,i,team) == data$winning_alliance[[i]]){
      data2$win[[i]] <- TRUE
    }else {
      data2$win[[i]] <- FALSE
    }
  }
  return(data2)
} #formatting for GetTeamMatchesCustom
addScore <- function(data,year,team){
  data2 <- data %>% mutate("Alliance_Score" = data[[1]], "Opponent_Score" = data[[1]])
  scoreData <- getTeamMatches(year, team)
  scoreBlue <- c(scoreData$alliances$blue$score)
  scoreRed <- c(scoreData$alliances$red$score)
  for (i in 1:length(data2[[1]])){
    if (data2$win[[i]] == TRUE){
      data2$Alliance_Score[[i]] <- max(scoreBlue[[i]], scoreRed[[i]])
      data2$Opponent_Score[[i]] <- min(scoreBlue[[i]], scoreRed[[i]])
    }else{
      data2$Alliance_Score[[i]] <- min(scoreBlue[[i]], scoreRed[[i]])
      data2$Opponent_Score[[i]] <- max(scoreBlue[[i]], scoreRed[[i]])
    }
  }
  return(data2)
} #formatting for GetTeamMatchesCustom
getTeamMatchesWon <- function(year, team) {
  data <- getTeamMatchesSimple(year, team)
  data2 <- addWin(data,team)
  data3 <- data %>% filter(data2$win == 1)
  return(data3)
} #Returns matches won by a given team in a given year.
getTeamMatchesLost <- function(year, team) {
  data <- getTeamMatchesSimple(year, team)
  data2 <- addWin(data,team)
  data3 <- data %>% filter(data2$win == 0)
  return(data3)
} #Returns matches lost by a given team in a given year.
getTeamMatchesCustom <- function(year,team){
  data <- getTeamMatchesSimple(year, team)
  data2 <- addWin(data,team)
  data3 <- addScore(data2,year, team)
  data4 <- getEvents(year) %>%
    select(event_type_string, key, name)
  data5 <- data3 %>%
    left_join(data4, by = c("event_key" = "key"))
  for (i in 1:length(data5$win)){
    if (data5$win[[i]] == TRUE){
      data5$win[[i]] <- "Won"
    }else {
      data5$win[[i]] <- "Lost"
    }
  }
  return(data5 %>%
           filter(event_type_string != "Offseason"))
} #Generates custom match information for a given team in a given year.

################################################################################
getTeamOpponents <- function(year, team){
  data1 <- getTeamEvents(year, team) %>%
    select(event_code)
  data3 <- NULL
  for (i in 1:length(data1[[1]])){
    data2 <- getEventTeamsKeys(year, data1[[1]][[i]])
    data3 <- c(data3,data2)
  }
  data4 <- as.numeric(gsub('frc','',data3))
  data5 <- data4[order(data4)]
  data6 <- distinct(as_tibble(data5))
  return(data6)
}
getTeamOpponentsEvents <- function(year,team){
  data1 <- getTeamOpponents(year,team) 
  data1$events = NA
  
  for(i in 1:length(data1[[1]])){
    data2 <- getTeamEvents(year, data1[[1]][[i]]) %>%
      select(name, week, end_date)
    data2 <- arrange(data2, week)
    data1$events[i] <- data2 %>% select(name)
    
  }
  data2 <- separate(data1, events, c("event1","event2","event3","event4","event5", "event6"), sep = ",")
  for (i in 1:7){
    data2[[i]] <- gsub('c\\(\\"','', data2[[i]]) 
    data2[[i]] <- gsub('\\"','', data2[[i]])
    data2[[i]] <- gsub('\\)','', data2[[i]]) 
  }
  
  return(data2)
}

################################################################################
plotScores <- function(year, team){
  winData <- getTeamMatchesCustom(year,team)
  qplot(Alliance_Score, Opponent_Score, data = winData, color = factor(win),shape = factor(str_trunc(name, 20)), main = team) + xlim(0,150) + ylim(0,150) + labs (shape = "Event") + labs (color = "Results")
} #Generates a graph plotting a teams performance in a given year.
plotScoresSimple <- function(year, team){
  winData <- getTeamMatchesCustom(year,team)
  qplot(Alliance_Score, Opponent_Score, data = winData, color = factor(win),) + xlim(0,150) + ylim(0,150)
} #Simplified version of plotScores if team has too many matches.

################################################################################
