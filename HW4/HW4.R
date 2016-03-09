#HW4 R Scrapbook

#1


#3
#should try this with two different types of clusterings

#clustering type 1 (threeh clusters not in an axis)
set.seed(1)
x=matrix(rnorm(20*3*50),ncol=50)
x[1:20,1]=x[1:20,1]+3
x[1:20,2]= x[1:20,2]+3
x[21:40,1]=x[21:40,1]-4
x[21:40,2]=x[21:40,2]-4
x[41:60,2]=x[41:60,2]+3
x[41:60,1]=x[41:60,1]-4
plot(x)

y=rep(NA,60)
y[1:20]=1
y[21:40]=2
y[41:60]=3

#3b perform PCA
#plot(x[1:20,],col="blue", xlim=c(-6,6), ylim=c(-6,6),xlab="x1", ylab="x2", type="p",lwd=6)
#par(new=T)
#plot(x[21:40,],col="green", xlim=c(-6,6), ylim=c(-6,6),xlab="x1", ylab="x2", type="p",lwd=6)
#par(new=T)
#plot(x[41:60,],col="red", xlim=c(-6,6), ylim=c(-6,6),xlab="x1", ylab="x2", type="p",lwd=6)
#par(new=F)


pr.out=prcomp(x)
plot(pr.out$x[,1:2],col=1:3, xlab="z1", ylab="z2", pch=20)
c=c(0,pr.out$rotation[1,1])
d=c(0,pr.out$rotation[2,1])
lines(5*c,5*d,col="purple")

c=c(0,pr.out$rotation[1,2])
d=c(0,pr.out$rotation[2,2])
lines(5*c,5*d,col="orange")


##
#Cols=function(vec){
#  cols=rainbow(length(unique(vec)))
#  return(cols[as.numeric(as.factor(vec))])
#}

#par(mfrow=c(1,2))
#plot(pr.out$x[,1:2],Cols(x),pch=19,xlab="Z1",ylab="Z2")

#lines(pr.out$rotation[,1],scale="2")
#lines(pr.out$rotation[,2])

#3c
set.seed(1)
km.out=kmeans(x,3,nstart=20)
table(y,km.out$cluster)

#3d
set.seed(1)
km.out=kmeans(x,2,nstart=20)
table(y,km.out$cluster)


#3e
set.seed(1)
km.out=kmeans(x,4,nstart=20)
table(y,km.out$cluster)

#3f
set.seed(1)
km.out=kmeans(pr.out$x[,1:2],3,nstart=20)
table(y,km.out$cluster)

#3g

set.seed(1)
km.out=kmeans(scale(x),3,nstart=20)
table(y,km.out$cluster)

#2a
X1op=c(3,2,4,1,2,4,4)
X2op=c(4,2,4,4,1,3,1)
Yop=c("Red","Red", "Red", "Red", "Blue", "Blue","Blue")

plot(X1op[5:7],X2op[5:7],col="green", xlim=c(0,5), ylim=c(0,5),xlab="X1", ylab="X2", type="p",lwd=6)
par(new=T)
plot(X1op[1:4],X2op[1:4],col="red", xlim=c(0,5), ylim=c(0,5),xlab="",ylab="",type="p", lwd=6)
par(new=F)

#2b
abline(-0.5,1)
#Equation of the hyperplane is 2*X1-2*X2-1=0

#2c
#Eqn is 2*X1-2*X2-1=0, so subbing values 
#1. Green (2,1)= 2(2)-2(1)-1= 4-3=1
#2. Red (2,2)=2(2)-2(2)-1=4-4-1=-1

#=>2*X1-2*X2-1>0== Green
#=>2*X1-2*X2-1<0== Red

#2d
#margin is the perpendicular distance from a point to line
#e.g red point at (2,2) intersects the line at (2.25, 1.75)
#distance between them is about 0.3

#2eP
#need to calculate th points.
#2f: movement will not impact unless it moves in a fashion such that
#it crosses theboundard.

#2g
#abline(-0.25,1) can divide the pane as well.

#2h
#add a red moing at 3,1

Y=c(1,1,1,1,0,0,0)
dat=data.frame(Y=as.factor(Y),X2op,X1op)
library(e1071)
svmfit=svm(Y~.,data=dat, kernel="linear", cost=10, Scale=F)
svmfit$index
summary(svmfit)
plot(svmfit,dat)

plot(X1op[5:7],X2op[5:7],col="green", xlim=c(0,5), ylim=c(0,5),xlab="X1", ylab="X2", type="p",lwd=6)
par(new=T)
plot(X1op[1:4],X2op[1:4],col="red", xlim=c(0,5), ylim=c(0,5),xlab="",ylab="",type="p", lwd=6)
line()
par(new=F)



#4
require(tree)
bodyR=load("body.RData")
plot(Y$Gender,Y$Weight)
set.seed(1)
train=sample(507,307)
test=-train
X.train=X[train,]
X.test=X[test,]
Y.test=Y[test,"Weight"]
Y.train=Y[train,"Weight"]

bag.body=randomForest(Y.train~.,data=X.train,mtry=21,ntree=50)
yhat.bag=predict(bag.body, newdata=X[-train,])
mean((yhat.bag-Y$Weight[-train])^2)


rf.body=randomForest(Y$Weight~.,data=X,subset=train,mtry=7,ntree=50)
yhat.rf=predict(rf.body, newdata=X[-train,])
mean((yhat.rf-Y$Weight[-train])^2)

plot(c(0,50),c(5,30), type="n", xlab= "# of Trees", ylab="MSE")
lines(rf.body$mse, col="blue", lwd=2.5)
lines(bag.body$mse, col="red", lwd=2.5)
legend(30,30,c("Bagging","Random Forest"))
lwd=c(2.5,2,5)
col=c("blue","red")
legend(30,30,c("Bagging","Random Forest"),lty=c(1,1),lwd=c(2.5,2.5),col=c("red","blue"))

#4b
varImpPlot(rf.body)
varImpPlot(bag.body)
#top3 for randomFores: Waistgirth, shoulder girth, chest girth
#top 3 for bagging: waist, shoulder chest

#4c
rf.body=randomForest(Y$Weight~.,data=X,subset=train,mtry=7,ntree=700)
yhat.rf=predict(rf.body, newdata=X[-train,])
mean((yhat.rf-Y$Weight[-train])^2)

#In the HW3 Solution, the PLS model had a test error of 8.65, PCR of 9.27
# forward stepwise 8.63. The error here is a 10.58, that is bit higher
# than other methods.

#4d
#The idea of using a smaller subet of 7 from 21 variables is so
#that we use trees from different variables and that they are 
#uncorrelated, thus helping us to reduce the variance of the averaged
#trees. Theorectically if there are 21C7 ~116280 ways to select 7 variables
# from 21 variables. So theoretically adding more trees should
# give a better estimate.

#Another practical way is to plot the test data error as a function
# of number of trees and see if the error improves.

