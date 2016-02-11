games=read.csv("http://statweb.stanford.edu/~jgorham/games.csv", as.is=TRUE)
teams=read.csv("http://statweb.stanford.edu/~jgorham/teams.csv", as.is=TRUE)

# Head gives the first few parts of a matrix/data frame etc
head(games)
head(teams)

#apparently similar to heads we can do tails
tail(games)
tail(teams)

all.teams=sort(unique(c(teams$team,games$home,games$away)))

ii = names(games) %in% c('home','homeScore')
head(games)[,ii]
# %in% = operator
3 %in% 1:5
c(3,5,10) %in% 1:5
#applies operator for each element of vector and returns true/false

## Functions

##Functin to compute teams total margin of victory
total.margin = function(team){
with(games,
     sum(homeScore[home==team])+
     sum(awayScore[away==team])-
     sum(homeScore[away==team])-
     sum(awayScore[home==team]))  
}


#Function to compute the humber of games a team played
number.games=function(team){
  with(games,
       sum(home==team)+sum(away==team))
}

#Computer total margin and number of game for each team
margins=sapply(teams$team,total.margin)
number.games=sapply(teams$team, number.games)

margin.per.game=margins/number.games

rank.table=cbind("Margin(Avg)" = margin.per.game,
                  "Margin Rank" = rank(-margin.per.game,ties="min"),
                  "AP Rank" = teams$apRank,
                  "USAT Rank" =teams$usaTodayRank)

margin.top25=order(margin.per.game, decreasing="TRUE")[1:25]
rank.table[margin.top25,]

y= with(games, homeScore-awayScore)
head(y)

X0= as.data.frame(matrix(0,nrow(games),length(all.teams)))
names(X0)=all.teams
X0[1:5,1:10]
