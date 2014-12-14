library(dplyr)
library(tidyr)
library(XML)
library(ggplot2)

GetScores <- function(yearvec,homeadjust = T,fbs = T){
  scores <- data.frame()
  for(year in yearvec){
    if(year == 2014){
      rawscores <- readHTMLTable(paste0("http://www.sports-reference.com/cfb/years/",year,"-schedule.html"),header = F,stringsAsFactors = F,which = 1,)
      rawscores <- rawscores %>% select(-V11,-V13)
      names(rawscores) <- c("GameNum","Week","Date","Time","Day","Winner","Winner_Points","Home_Away","Loser","Loser_Points","Notes")
      rawscores <- rawscores %>% select(-Time)
    }
    if(year == 2013){
      rawscores <- readHTMLTable(paste0("http://www.sports-reference.com/cfb/years/",year,"-schedule.html"),header = F,stringsAsFactors = F,which = 1)
      names(rawscores) <- c("GameNum","Week","Date","Time","Day","Winner","Winner_Points","Home_Away","Loser","Loser_Points","Notes")
      rawscores <- rawscores %>% select(-Time)
    }
    if(year < 2013){
      rawscores <- readHTMLTable(paste0("http://www.sports-reference.com/cfb/years/",year,"-schedule.html"),header = F,stringsAsFactors = F,which = 1)
      names(rawscores) <- c("GameNum","Week","Date","Day","Winner","Winner_Points","Home_Away","Loser","Loser_Points","Notes")
    }
    rawscores$Year <- year
    scores <- rbind(scores,rawscores)
  }
  scores <- scores %>% filter(GameNum != "Rk",Winner_Points != "")
  scores$Winner <- sapply(scores$Winner,function(x) ifelse(substr(x,1,1) == "(",substr(x,regexpr(")",x)[[1]][1] + 2,nchar(x)),x))
  scores$Loser <- sapply(scores$Loser,function(x) ifelse(substr(x,1,1) == "(",substr(x,regexpr(")",x)[[1]][1] + 2,nchar(x)),x))
  scores$Location <- with(scores,ifelse(Home_Away == "@","Away",ifelse(Notes == "","Home","Neutral")))
  scores$Winner_Points <- as.numeric(scores$Winner_Points)
  scores$Loser_Points <- as.numeric(scores$Loser_Points)
  if(homeadjust){
    scores$Winner_Points <- with(scores,ifelse(Location == "Neutral",Winner_Points,ifelse(Location == "Home",Winner_Points - 1.5,Winner_Points + 1.5)))
    scores$Loser_Points <- with(scores,ifelse(Location == "Neutral",Loser_Points,ifelse(Location == "Home",Loser_Points + 1.5,Loser_Points - 1.5)))
  }
  if(fbs){
    winnergames <- scores %>% group_by(Year,Winner) %>% summarize(n = n()) %>% mutate(Team = Winner) %>% select(-Winner)
    losergames <- scores %>% group_by(Year,Loser) %>% summarize(n = n()) %>% mutate(Team = Loser) %>% select(-Loser)
    totalgames <- rbind(winnergames,losergames) %>% group_by(Year,Team) %>% summarize(Games = sum(n))
    scores <- merge(scores,totalgames,by.x = c("Winner","Year"),by.y = c("Team","Year"),all.x = T)
    scores <- merge(scores,totalgames,by.x = c("Loser","Year"),by.y = c("Team","Year"),all.x = T)
    scores <- filter(scores,Games.x > 2,Games.y > 2)
  }
  scores$Week <- as.numeric(scores$Week)
  scores$GameNum <- as.numeric(scores$GameNum)
  return(scores) 
}

CTMC <- function(scores,best = "big"){
  winnerscores <- scores %>% select(Winner,Loser,Winner_Points)
  names(winnerscores) <- c("Team","Opponent","Points")
  loserscores <- scores %>% select(Loser,Winner,Loser_Points)
  names(loserscores) <- c("Team","Opponent","Points")
  teamscores <- rbind(winnerscores,loserscores) %>% group_by(Team,Opponent) %>% summarize(Points = sum(Points))
  if(best == "big"){
    teamscores <- teamscores %>% spread(Team,Points)
    teams <- teamscores$Opponent
    teamscores[is.na(teamscores)] <- 0
    Tmatrix <- data.matrix(select(teamscores,-Opponent))
  }
  else{
    teamscores <- teamscores %>% spread(Opponent,Points)
    teams <- teamscores$Team
    teamscores[is.na(teamscores)] <- 0
    Tmatrix <- data.matrix(select(teamscores,-Team))
  }
  rownames(Tmatrix) <- teams
  leavingrate <- rowSums(Tmatrix)
  Ematrix <- t(Tmatrix)
  for(i in 1:nrow(Ematrix)){
    for(j in 1:ncol(Ematrix)){
      if(i == j) Ematrix[i,j] <- -leavingrate[i]
    }
  }
  Ematrix[1,] <- 1
  yearratings <- solve(Ematrix,c(1,rep(0,nrow(Ematrix) - 1)))
  return(yearratings)
}

Massey <- function(scores){
  winnerscores <- scores %>% select(Winner,Loser,Winner_Points)
  names(winnerscores) <- c("Team","Opponent","Points")
  loserscores <- scores %>% select(Loser,Winner,Loser_Points)
  names(loserscores) <- c("Team","Opponent","Points")
  teamscores <- rbind(winnerscores,loserscores) %>% group_by(Team,Opponent) %>% summarize(Points = sum(Points))
  
  Smatrix <- with(teamscores,table(Team,Opponent))
  totalgames <- rowSums(Smatrix)
  Smatrix <- -Smatrix
  for(i in 1:nrow(Smatrix)){
    for(j in 1:ncol(Smatrix)){
      if(i == j) Smatrix[i,j] <- totalgames[i]
    }
  }
  pointsfor <- teamscores %>% group_by(Team) %>% summarize(Points = sum(Points))
  pointsag <- teamscores %>% group_by(Opponent) %>% summarize(Points = sum(Points))
  pointsvec <- pointsfor$Points - pointsag$Points
  Smatrix[1,] <- 1
  yearratings <- solve(Smatrix,c(0,pointsvec[2:length(pointsvec)]))
  return(yearratings)
}

RatingsPredict <- function(scores,yearvec,method = "CTMC",best = "small",startweek = 5){
  results <- data.frame()
  for(year in yearvec){
    maxweek <- max(scores[scores$Year == year,"Week"])
    yearresults <- data.frame()
    for(week in seq(startweek,maxweek - 1)){
      if(method == "CTMC") weekratings <- CTMC(filter(scores,Year == year,Week < week))
      else weekratings <- Massey(filter(scores,Year == year,Week < week))
      teamratings <- data.frame(Rating = weekratings,Team = names(weekratings))
      weekscores <- merge(filter(scores,Year == year,Week == week),teamratings,by.x = "Winner",by.y = "Team")
      weekscores <- merge(weekscores,teamratings,by.x = "Loser",by.y = "Team")
      if(best == "small") weekscores$Prediction <- with(weekscores,ifelse(Rating.x <= Rating.y,1,0))
      else weekscores$Prediction <- with(weekscores,ifelse(Rating.x >= Rating.y,1,0))
      weeklyresults <- data.frame(Week = week,Accuracy = mean(weekscores$Prediction),n = nrow(weekscores))
      yearresults <- rbind(yearresults,weeklyresults)
    }
    yearresults$Year <- year
    results <- rbind(results,yearresults)
  }
  results$Method <- method
  return(results)
}

Wrapper <- function(years){
  scores <- GetScores(years,homeadjust = T,fbs = T)
  ctmcratings <- RatingsPredict(scores,years,method = "CTMC",best = "large",startweek = 5)
  masseyratings <- RatingsPredict(scores,years,method = "Massey",best = "large",startweek = 5)
  plotdata = rbind(ctmcratings,masseyratings)
  weightmean <- plotdata %>% group_by(Year,Method) %>% summarize(m = weighted.mean(Accuracy,n))
  return(list(Games = scores,ctmc = ctmcratings,mass = masseyratings,plotdata = plotdata,weightmean = weightmean))
}
