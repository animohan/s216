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
