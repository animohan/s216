library(ISLR)
dim(Auto)
names(Auto)
plot(Auto$cylinders, Auto$mpg)
Auto$cylinders=as.factor(cylinders)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="green", varwidth=T)
plot(cylinders, mpg, col="blue", varwidth=T, horizontal=T)
plot(cylinders, mpg, col="yellow", varwidth=T, horizontal=T, xlab="cylinders", ylab="MPG")
hist(mpg)
hist(mpg, col=2) #changes color to red
hist(mpg, col=3,breaks=15)

pairs(Auto)
pairs(~mpg+displacement+horsepower+weight+acceleration, Auto)

plot(horsepower, mpg)
identify(horsepower,mpg, name)


library(MASS)
summary(Boston)
dim(Boston)
pairs(Boston)

pairs(~crim+dis+black+lstat+medv)
pairs(~crim+dis, Boston)
pairs(~crim+medv, Boston)
pairs(~crim+lstat, Boston)
pairs(~crim+nox, Boston)
pairs(~crim+age, Boston)

