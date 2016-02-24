library(ISLR)
attach(Wage)
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))

fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se)

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
#lines(preds$fit) Does not have xcoordinate set, hence we should
#privde age.grid

lines(age.grid,preds$fit,lwd=2, col="blue")
se.bands
matlines(age.grid,se.bands,lwd=1, col="blue",lty=3)

#doing ANOVA
fit.1=lm(wage~age, data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)

anova(fit.1,fit.2,fit.3,fit.4,fit.5)


#Another example with ANOVA
fit.1=lm(wage~education+age, data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)


#Predicting if an individual earns more than 250K based on age.
fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)

preds=predict(fit, newdata=list(age=age.grid),type="response",se=T)

preds=predict(fit, newdata=list(age=age.grid),se=T)

pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))

#plotting
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,0.2))
points(jitter(age),I((wage>250)/5),cex=0.5,pch="1",col="darkgrey")
lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

#Splines
library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="grey")

lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

#fitting natural splines
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2, newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)


#smoothing splines
plot(age,wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Smoothing splines")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age, wage, cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2, col="blue",lwd=2)

#local regression
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Local Regression")
fit=loess(wage~age, span=0.2, data=Wage)
fit2=loess(wage~age, span=0.5, data=Wage)
lines(age.grid, predict(fit,data.frame(age=age.grid)),col="red", lwd=2)
lines(age.grid, predict(fit2,data.frame(age=age.grid)),col="blue", lwd=2)
