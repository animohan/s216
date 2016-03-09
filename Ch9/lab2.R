#Ch 9 SVM : Non linear SVM

load(url("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/ESL.mixture.rda"))
names(ESL.mixture)
rm(x,y)
attach(ESL.mixture)
plot(x,col=y+1)
dat=data.frame(y=factor(y),x)
fit=svm(factor(y)~.,data=dat, scale=FALSE, kernel="radial", cost=5)
xgrid=expand.grid(X1=px1,X2=px2)
ygrid=predict(fit,xgrid)
plot(xgrid, col=as.numeric(ygrid),pch=20, cex=.2)
points(x,col=y+1, pch=19)

func=predict(fit,xgrid, decision.value=TRUE)
func=attributes(func)$decision
xgrid=expand.grid(X1=px1,X2=px2)
ygrid=predict(fit,xgrid)
points(X,col=y+1, pch=19)

#true decision boundary
contour(px1,px2,matrix(func,69,99),level=0, add=TRUE)
contour(px1,px2,matrix(prob,69,99), level=.5, add=TRUE, col="blue")
