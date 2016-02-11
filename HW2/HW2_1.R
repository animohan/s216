games=read.csv("http://statweb.stanford.edu/~jgorham/games.csv", as.is=TRUE)
teams=read.csv("http://statweb.stanford.edu/~jgorham/teams.csv", as.is=TRUE)

all.teams=sort(unique(c(teams$team,games$home,games$away)))