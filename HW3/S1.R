#2
library(ISLR)
library(MASS)
attach(Boston)

#a
  #limits of the distances
dislims=range(dis)

  #create grid with range of distance values
dis.grid=seq(from=dislims[1],to=dislims[2])

  #fit the cubic polynomial
poly.fit=lm(nox~poly(dis,3),data=Boston)

  #fit output
summary(poly.fit)

  #prediction and plotting
poly.pred=predict(poly.fit,newdata=list(dis=dis.grid),se=T)
se.bands=cbind(poly.pred$fit+2*poly.pred$se,poly.pred$fit-2*poly.pred$se)

plot(dis,nox,xlim=dislims,cex=0.5, col="darkgrey")
lines(dis.grid,poly.pred$fit,lwd=2, col="blue")
lines(dis.grid,se.bands,lwd=1, col="blue",lty=3)

#b
par(mfrow=c(3,4))
rss=rep(NA,10)

for(i in 1:10){
  poly.fit=lm(nox~poly(dis,i),data=Boston) 
  rss[i]=sum(poly.fit$residuals^2)
  poly.pred=predict(poly.fit,newdata=list(dis=dis.grid),se=T)
  plot(dis,nox,xlim=dislims,cex=0.5, col="darkgrey")
  lines(dis.grid,poly.pred$fit,lwd=2, col="blue")
}

plot(rss)

#c
poly.fit.1=lm(nox~poly(dis,1),data=Boston) 
poly.fit.2=lm(nox~poly(dis,2),data=Boston) 
poly.fit.3=lm(nox~poly(dis,3),data=Boston) 
poly.fit.4=lm(nox~poly(dis,4),data=Boston) 
poly.fit.5=lm(nox~poly(dis,5),data=Boston) 
poly.fit.6=lm(nox~poly(dis,6),data=Boston) 
poly.fit.7=lm(nox~poly(dis,7),data=Boston) 
poly.fit.8=lm(nox~poly(dis,8),data=Boston) 
poly.fit.9=lm(nox~poly(dis,9),data=Boston) 
poly.fit.10=lm(nox~poly(dis,10),data=Boston) 
anova(poly.fit.1,poly.fit.2,poly.fit.3,poly.fit.4,poly.fit.5,poly.fit.6,poly.fit.7,poly.fit.8,poly.fit.9,poly.fit.10)

#d
par(mfrow=c(1,1))
library(splines)
sp.fit=lm(nox~bs(dis,df=4),data=Boston)
sp.pred=predict(sp.fit,newdata=list(dis=dis.grid),se=T)
plot(dis,nox,col="blue",main="Regression spline fit")
lines(dis.grid,sp.pred$fit,lwd=2)

#e
rss=rep(NA,8)
par(mfrow=c(3,4))
for(i in 3:10){
  bs.fit=lm(nox~bs(dis,df=i),data=Boston) 
  rss[i]=sum(bs.fit$residuals^2)
  bs.pred=predict(bs.fit,newdata=list(dis=dis.grid),se=T)
  plot(dis,nox,xlim=dislims,cex=0.5, col="darkgrey")
  lines(dis.grid,bs.pred$fit,lwd=2, col="blue")
}

par(mfrow=c(3,4))
plot(rss)

#f
bs.fit.3=lm(nox~bs(dis,df=3),data=Boston) 
bs.fit.4=lm(nox~bs(dis,df=4),data=Boston) 
bs.fit.5=lm(nox~bs(dis,df=5),data=Boston) 
bs.fit.6=lm(nox~bs(dis,df=6),data=Boston) 
bs.fit.7=lm(nox~bs(dis,df=7),data=Boston) 
bs.fit.8=lm(nox~bs(dis,df=8),data=Boston) 
bs.fit.9=lm(nox~bs(dis,df=9),data=Boston) 
bs.fit.10=lm(nox~bs(dis,df=10),data=Boston) 
anova(bs.fit.3,bs.fit.4,bs.fit.5,bs.fit.6,bs.fit.7,bs.fit.8,bs.fit.9,bs.fit.10)


#3
library(ISLR)
library(pls)
bodyR=load("body.RData")

#3a.
plot(Y$Gender,Y$Height)
plot(Y$Gender,Y$Weight)

#3b
set.seed(1)
train=sample(507,307)
test=-train
X.train=X[train,]
X.test=X[test,]
Y.test=Y[test,"Weight"]
Y.train=Y[train,"Weight"]


set.seed(1)
#PCR Fit
pcr.fit=pcr(Y.train~.,data=X.train, scale=TRUE, validation="CV")
#PLS Fit
set.seed(1)
pls.fit=plsr(Y.train~.,data=X.train, scale=TRUE, validation="CV")

#3c
summary(pcr.fit)
summary(pls.fit)

#3d

#number of components can be chosen by lowest CV error
#also the one that show maximum variance covered

pcr.pred=predict(pcr.fit,X[test,], ncomp=5)
mean((pcr.pred-Y.test)^2)

pls.pred=predict(pls.fit,X[test,], ncomp=5)
mean((pls.pred-Y.test)^2)


#3e
# We show that you can do some variable selection with Lasso and more variable selection with Forward Selection and
# and get comparable error rate to PCR and PLS with a better model interpretability

#Running Lasso
library(glmnet)
body=data.frame(X,Y)
xl=model.matrix(Weight~.,body)[,-1]
yl=body$Weight

grid=10^seq(10,-2, length=100)
lasso.mod=glmnet(xl[train,], yl[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(xl[train,],yl[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

lasso.pred=predict(lasso.mod,s=bestlam, newx=xl[test,])
mean((lasso.pred-yl[test])^2)
lasso.coef=predict(cv.out,type="coefficients",s=bestlam)


library(leaps)
regfit.fwd=regsubsets(Y.train~., data=X.train, method="forward", nv=20)
summary(regfit.fwd)


#apply linear regression with variables selected from forward selection
lm.fit=lm(Y.train~Forearm.Girth+Waist.Girth+Hip.Girth+Knee.Girth+Ankle.Diam+Shoulder.Girth+Chest.Depth, data=X.train)
lm.pred=predict(lm.fit,X.train)
mean((Y.train-lm.pred)^2)

lm.testpred=predict(lm.fit,newdata=X.test)
mean((Y.test-lm.testpred)^2)

summary(lm.fit)


