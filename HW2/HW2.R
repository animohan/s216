library(ISLR)
names(Weekly)
attach(Weekly)


glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(glm.fit)

#Lag2 seems to be statistical significant result as P value is <0.05

glm.probs = predict(glm.fit, type="response")
glm.pred=rep("Down", length(glm.probs))
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Weekly$Direction)


train=(Year<2009)
Weekly.2008=Weekly[train,]
dim(Weekly.2008)

Weekly.2010=Weekly[!train,]

Direction.2008=Direction[train]
Direction.2010=Direction[!train]

glm.fit2=glm(Direction~Lag1+Lag2+Lag3,data=Weekly.2008,family=binomial)

glm.probs2 = predict(glm.fit2, Weekly.2010,type="response")
glm.pred2=rep("Down", length(glm.probs2))
glm.pred2[glm.probs2>0.5]="Up"
table(glm.pred2,Weekly.2010$Direction)


#d
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2+Lag3, data=Weekly.2008)
summary(lda.fit)
lda.pred=predict(lda.fit,Weekly.2010)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class, Direction.2010)


#e
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(2016)
knn.pred=knn(train.X, test.X,train.Direction,k=1)
table(knn.pred,Direction.2010)


#Problem 6
games=read.csv("http://statweb.stanford.edu/~jgorham/games.csv", as.is=TRUE)
teams=read.csv("http://statweb.stanford.edu/~jgorham/teams.csv", as.is=TRUE)


all.teams=sort(unique(c(teams$team,games$home,games$away)))

ii = names(games) %in% c('home','homeScore')
head(games)[,ii]

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


X0 = as.data.frame(matrix(0,nrow(games),length(all.teams)))
names(X0)=all.teams

for(tm in all.teams){
  X0[[tm]]=1*(games$home==tm)-1*(games$away==tm)
  
}

X=X0[,names(X0) !="stanford-cardinal"]
reg.season.games=which(games$gameType=="REG")
lm.fit=lm(y~0+.,data=X,subset=reg.season.games)

homeAdv=1-games$neutralLocation
Xh=cbind(homeAdv=homeAdv,X)

lm.fit.homeAdv=lm(y~0+.,data=Xh, subset=reg.season.games)
#head(coef(summary(lm.fit.homeAdv)),1)
#lmrank=coef(summary(lm.fit.homeAdv))[,1]
#rank.table.lm=cbind("Linear Reg Estimate" = lmrank,
#                   "Linear Reg Rank" = rank(-lmrank,ties="min"))
#lm.top25=order(lmrank, decreasing="TRUE")[1:25]
#rank.table.lm[lm.top25,]



y.win=with(games, homeScore-awayScore>0)
y.win+0;
glm.fit.ncaa=glm(y.win~0+.,data=Xh, subset=reg.season.games, family=binomial)
head(coef(summary(glm.fit.ncaa)))
coef(summary(glm.fit.ncaa))
#saint mary sainty mary has coeff of 14.13 with p value 0.9
#saint- thomas has 13.27 pvalue .9


#b

X0play = as.data.frame(matrix(NA,1,length(all.teams)))
names(X0play)=all.teams
i=1
for(tm in all.teams){
 X0play[i]=sum(games$home==tm)+sum(games$away==tm) 
 i=i+1
}

X0play.5=X0play[which(X0play[]>5)]


X05 = as.data.frame(matrix(0,nrow(games),ncol(X0play.5)))
names(X05)=names(X0play.5)

for(tm in names(X0play.5)){
  X05[[tm]]=1*(games$home==tm)-1*(games$away==tm)
  
}

X5=X05[,names(X05) !="stanford-cardinal"]
reg.season.games=which(games$gameType=="REG")
homeAdv=1-games$neutralLocation
Xh5=cbind(homeAdv=homeAdv,X5)

lm.fit.ncaa5=glm(y~0+.,data=Xh5, subset=reg.season.games)
lmrank=coef(summary(lm.fit.ncaa5))[,1]
rank.table.lm=cbind("Linear Reg Estimate" = lmrank,
                     "Linear Reg Rank" = rank(-lmrank,ties="min"),
                     "AP Rank" = teams$apRank,
                     "USAT Rank" =teams$usaTodayRank)

lm.top25=order(lmrank, decreasing="TRUE")[1:25]
rank.table.lm[lm.top25,]



glm.fit.ncaa5=glm(y.win~0+.,data=Xh5, subset=reg.season.games, family=binomial)

head(coef(summary(glm.fit.ncaa5)))
glmrank=coef(summary(glm.fit.ncaa5))[,1]



rank.table.glm=cbind("Log Reg Estimate" = glmrank,
                     "Log Reg Rank" = rank(-glmrank,ties="min"),
                     "AP Rank" = teams$apRank,
                     "USAT Rank" =teams$usaTodayRank)

glm.top25=order(glmrank, decreasing="TRUE")[1:25]
rank.table.glm[glm.top25,]

#c
u=which(coef(summary(lm.fit))[,4]<0.05)
coef(summary(glm.fit.ncaa))[u,]
nrow(coef(summary(glm.fit.ncaa))[u,])

k=which(coef(summary(glm.fit.ncaa))[,4]<0.05)
coef(summary(glm.fit.ncaa))[k,]
nrow(coef(summary(glm.fit.ncaa))[k,])


#d
library(boot)
set.seed(1)
cv.error10.lm=rep(0,5)
Xhywin=cbind(y.win=y.win,Xh)
lm.fit.homeAdv=glm(y.win~.,data=Xhywin, subset=reg.season.games, family=binomial)
cv.error10.lm=cv.glm(Xhywin,lm.fit.homeAdv,K=1000)$delta[1]



set.seed(1)
Xhywin=cbind(y.win=y.win,Xh)

glm.fit.homeAdv=glm(y.win~.,data=Xhywin, subset=reg.season.games, family=binomial)
cv.error=cv.glm(Xhywin,lm.fit.homeAdv)


