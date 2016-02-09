#Simple things
#1. Assign
x=5
y=x*X

#6Repitition
x=rep(5,20)

#7 Sequences
x=seq(1,5,1)
x=seq(1,5,2)

#8Accessing elements for vector
x=seq(2,32,2)
x[1]
x[1:5]
x[-1] # return everything except first element
x[-2]
x[c(1,2,4)] # return 1st 2nd and 4th element
x[1]=2002
x

#9 Plotting
x=1:100
y=x^2
plot(x,y) #plot (independent and dependent variable)

#10 documentation
?plot

#11 color the plot
plot(x,y, col='red')

#12 Run a complete script
#inc command window type: source('~01RSession.R, echo=True')

#13 Data.frame
data=data.frame(x,y)
dim(data)
plot(data)

#14 Remove
#use remove


#15 Class Handout
set.seed(2016)
X=rnorm(100, mean=20, sd=3)
hist(X)
beta0=25
beta1=2
eps=rnorm(100, mean=0, sd=15)
Y=beta0+beta1*X+eps
plot(X,Y)

abline(beta0, beta1, col='red')
abline(lm.fit, col='blue')

#linear model fitting
lm.fit=lm(Y~X)
summary(lm.fit)

#Class handout: Simulation to check aspects for regression

# Theoretical noise
eps #noise
actual.sd.eps=sd(eps) #standard dev of noise.

# we generally don't have access to noise data; but calculated from output Y
estimate.sd.eps=sqrt((sum((Y-mean(Y))^2))/(100-2))

#Variability of Beta1= sd(eps)*sd(eps)/sum((X-mean(X))^2)
true.stderror.beta1=(sd(eps)*sd(eps))/sum((X-mean(X))^2)
estimate.stderror.beta1= (estimate.sd.eps^2)/sum((X-mean(X))^2)


X.var=rep(0,1000)

for(i in 1:1000)
  {
    eps=rnorm(100, mean=0, sd=15)
    Y=beta0+beta1*X+eps
    lm.fit2=lm(Y~X)
    X.var[i]=lm.fit2$coefficients[2]
}

mean(X.var)
sd(X.var)


#Running simulations by randomly generating X and noise
X.variability=rep(0,1000)

for(i in 1:1000)
{
  X=rnorm(100, mean=20, sd=3)
  eps=rnorm(100, mean=0, sd=15)
  Y=beta0+beta1*X+eps
  lm.fit3=lm(Y~X)
  X.variability[i]=lm.fit3$coefficients[2]
}

mean(X.variability)
sd(X.variability) #std error of Coef of X i.e Beta1

#Result: The std error of Beta1 here also is in line with lm.fit
# This proves that if the underlying mode is same with same parameters
#(e.g in this case rnorm with mean 20 and sd 3), taking different samples
#doesn't change the out put much.

# Class Handout: 5 Multiple Regression
set.seed(109)
X1=rnorm(100, mean=20, sd=3)
X2=rnorm(100, mean=20, sd=3)
b0=25
b1=2
b2=1
eps=rnorm(100, mean=0, sd=15)
Y=b0+b1*X1+b2*X2+eps

## fitting linear model
lm.fit4=lm(Y~ X1)
mlm.fit=lm(Y ~ X1+X2)
summary(lm.fit4)
summary(mlm.fit)
##Note: The standard errors for both X1 and X2 are similar.
## Remember both X1 and X2 are from the distribution with same parameters
## Check if there is high correlation between X1 and X2

cor(X1,X2)
##Value was a very low -0.02, hence low correlation.
## Might be because these are random data points.

##generating correlated vectors with 0.6 correation
Z=X2
X2=0.7*X1+Z
plot(X1,X2)
cor(X1,X2)

Y=b0+b1*X1+b2*X2+eps
lm.fit5=lm(Y~X1)
summary(lm.fit5)
mlm.fit2=lm(Y ~ X1+X2)
summary(mlm.fit2)

#related vectors with 0.8 correation
## The formula given in the notes does not work, unless i am doing something obviously worng

set.seed(109)
X11=rnorm(100,20,3)
X22=X11+rnorm(100,20,3)
plot(X11,X22)
cor(X11,X22)

Y=b0+b1*X11+b2*X22+eps
lm.fit6=lm(Y~X11)
summary(lm.fit6)
mlm.fit3=lm(Y ~ X11+X22)
summary(mlm.fit3)
