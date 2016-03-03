require(ISLR)
require(tree)
attach(Carseats)
hist(Sales)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats, High)
tree.Carseats=tree(High~.-Sales, data=Carseats)
summary(tree.Carseats)
plot(tree.Carseats)
text(tree.Carseats, pretty=0)

tree.Carseats

set.seed(1)
train = sample(1: nrow(Carseats),250)
tree.Carseats=tree(High~.-Sales, Carseats, subset=train)
plot(tree.Carseats)
text(tree.Carseats, pretty=0)

tree.pred=predict(tree.Carseats, Carseats[-train, ],type="class")

with(Carseats[-train,], table(tree.pred, High))


cv.carseats=cv.tree(tree.Carseats, FUN=prune.misclass)
cv.carseats
plot(cv.carseats)

prune.carseats=prune.misclass(tree.Carseats, best=12)


#Random Forests and Boosting

plot(prune.carseats); 
text(prune.carseats, pretty=0)

tree.pred=predict(tree.Carseats, Carseats[-train,],type="class")
with(Carseats[-train,], table(tree.pred, High) )


