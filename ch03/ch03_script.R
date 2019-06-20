setwd("D:/Workplace/Statistical_Analysis_R/ch03")


install.packages("prob")
library(prob)
tosscoin(1)
rolldie(1)
urnsamples(1:3,size=2)
urnsamples(1:3,size=2,replace=T)
urnsamples(c(rep("R",3),rep("B",2)),size=2)
tosscoin(2,makespace=T)

x <- 0:2
px <- c(0.25,0.5,0.25)
(EX <- sum(x*px))
x*2
x*(1:4)
x*(1:6)
(VX <- sum(x^2*px)-EX^2)

n <- 6
p <- 1/3
x <- 0:n
(dbinom(2,size=n,prob=p))
(dbinom(4,size=n,prob=p))
(px <- dbinom(x,size=n,prob=p))
plot(x,px,type="s",xlab="성공 횟수(x)",ylab="확률(P[X=x])",
     main="B(6,1/3)")
pbinom(2,size=n,prob=p)
pbinom(4,size=n,prob=p)
pbinom(4,size=n,prob=p) - pbinom(2,size=n,prob=p)
qbinom(0.1,size=n,prob=p)
qbinom(0.5,size=n,prob=p)
rbinom(10,size=n,prob=p)
px <- dbinom(x,size=n,prob=p)
(ex <- sum(x*px))
(ex2 <- sum(x^2*px))
(varx <- ex2-ex^2)

options(digits=3)
mu <- 170
sigma <- 6
ll <- mu - 3*sigma
ul <- mu + 3*sigma
x <- seq(ll,ul,by=0.01)
nd <- dnorm(x,mean=mu,sd=sigma)
plot(x,nd,type="l",xlab="x",ylab="P(X=x)",lwd=2,col="red")
pnorm(mu,mean=mu,sd=sigma)
pnorm(158,mean=mu,sd=sigma)
pnorm(180,mean=mu,sd=sigma) - pnorm(160,mean=mu,sd=sigma)
qnorm(0.25,mean=mu,sd=sigma)
qnorm(0.5,mean=mu,sd=sigma)
qnorm(0.75,mean=mu,sd=sigma)

options(digits=5)
set.seed(5)
smp <- rnorm(400,mean=mu,sd=sigma)
c(mean(smp),sd(smp))
hist(smp,prob=T,main="N(170,6^2)으로부터 추출한 표본의 분포(n=400)",
     xlab="",ylab="",col="white",border="black")
lines(x,nd,lty=2)

options(digits=4)
mu <- 0
sigma <- 1
(p0.05 <- qnorm(0.05,mean=mu,sd=sigma))
(p0.025 <- qnorm(0.025,mean=mu,sd=sigma))
pnorm(1.645) - pnorm(-1.645)
pnorm(1.96) - pnorm(-1.96)
