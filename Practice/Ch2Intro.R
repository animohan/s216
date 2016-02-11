x=c(1,2,3,5)
x
y=c(2,3,4,6)
plot(x,y)

length(x)
length(y)
ls()

a=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
a

b=matrix(data=c(1,2,3,4),2,2,byrow=TRUE)
b

sqrt(b)

u=rnorm(50)
v=x+rnorm(50, mean=50, sd=1)
plot(u,v)

set.seed(3)
y=rnorm(100)
mean(y)
var(y)

x=rnorm(100)
y=rnorm(100)
plot(x,y)


x= seq(1,10)
x
x=seq(-pi, pi, length=50)

y=x
f=outer(x,y, function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
plot(x,f[1:50]) # lenght of f is 50
fa=(f-t(f))/2
contour(x,y,fa, nlevels=15)


contour(x,y,f,nlevels=45, add=T)

mean(x)
var(x)
sqrt(var(x))

image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)

A = matrix(1:16,4,4)
A[2,3]
A
