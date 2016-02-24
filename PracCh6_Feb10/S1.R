bodyR=load("body.RData")
par(mfrow=c(2,3)) ##par(mfrow=c(2,3), ask=TRUE) ##ASK just asks before loading graph

vnames=colnames(X)

## for each name in X, plot the histogram
for(i in vnames){
  hist(X[,i], main=i, xlab="")
}

vnames=colnames(Y)
## for each name in Y, plot the histogram
for(i in vnames){
  hist(Y[,i], main=i, xlab="")
}

summary(Y$Gender)

par(mfrow=c(1,1))
#plot(Y$Weight,Y$Height, main="How many dimensions ?", asp=1)
with(Y, plot(Weight,Height,main="How many dimensions?", asp=1))
