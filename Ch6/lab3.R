library(ISLR)

fix(Hitters)
names(Hitters)
dim(Hitters)

#No of missing entries in the hitters
sum(is.na(Hitters$Salary)) 

#Removes the rows that have an entry that is is.na ie not avaialble.
Hitters=na.omit(Hitters)
dim(Hitters)

# check that is.na==0
sum(is.na(Hitters))

library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type ="MSEP")


x=model.matrix(Salary~., Hitters)[,-1]
y=Hitters$Salary
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=-train
y.test=y[test]


set.seed(1)
pcr.fit=pcr(Salary~.,data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred=predict(pcr.fit,x[test,], ncomp=7)
mean((pcr.pred-y.test)^2)


#Partial Least Squares Fit
set.seed(1)
pls.fit=plsr(Salary~.,data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)

pls.pred=predict(pls.fit,x[test,], ncomp=2)
mean((pls.pred-y.test)^2)

pls.fit=plsr(Salary~.,data=Hitters, Scale=TRUE, ncomp=2)
summary(pls.fit)
