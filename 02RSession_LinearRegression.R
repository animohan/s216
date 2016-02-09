games=read.csv("http://statweb.stanford.edu/~jgorham/games.csv", as.is=TRUE)
teams=read.csv("http://statweb.stanford.edu/~jgorham/teams.csv", as.is=TRUE)

# Head gives the first few parts of a matrix/data frame etc
head(games)
head(teams)

#apparently similar to heads we can do tails
tail(games)
tail(teams)

all.teams=sort(unique(c(teams$team,games$home,games$away)))

