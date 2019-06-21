setwd("D:/Workplace/Statistical_Analysis_R/ch04")

# Ex 4-1

m10 <- rep(NA,1000)
m40 <- rep(NA,1000)
set.seed(9)
for (i in 1:1000) {
  m10[i] <- mean(rnorm(10))
  m40[i] <- mean(rnorm(40))
}

options(digits=4)
c(mean(m10),sd(m10))
c(mean(m40),sd(m40))

par(mfrow=c(1,2))
hist(m10,xlim=c(-1.5,1.5),main="n=10",xlab="",ylab="",
     col="cyan",border="blue")
hist(m40,xlim=c(-1.5,1.5),main="n=40",xlab="",ylab="",
     col="cyan",border="red")
par(mfrow=c(1,1))


# Ex 4-2

set.seed(9)
n <- 1000
r.1.mean <- rep(NA,n)
r.2.mean <- rep(NA,n)
for (i in 1:n) {
  r.1.mean[i] <- mean(rnorm(4,3,1))
  r.2.mean[i] <- mean(rnorm(4,170,6))
}

options(digits=4)
c(mean(r.1.mean),sd(r.1.mean))
c(mean(r.2.mean),sd(r.2.mean))

hist(r.1.mean,prob=T,xlab="표본평균 N(3,1^2)",ylab="밀도",
     main="",col="orange",border="red")
x1 <- seq(min(r.1.mean),max(r.1.mean),length=1000)
y1 <- dnorm(x1,3,1/sqrt(4))
lines(x1,y1,lty=2,lwd=2,col="blue")

hist(r.2.mean,prob=T,xlab="표본평균 N(170,6^2)",ylab="밀도",
     main="",col="orange",border="red")
x2 <- seq(min(r.2.mean),max(r.2.mean),length=1000)
y2 <- dnorm(x2,170,6/sqrt(4))
lines(x2,y2,lty=2,lwd=2,col="blue")


# Ex 4-3

set.seed(9)
t <- 10
p <- 0.1
x <- 0:10
n <- 1000
b.2.mean <- rep(NA,n)
b.4.mean <- rep(NA,n)
b.32.mean <- rep(NA,n)

for (i in 1:n) {
  b.2.mean[i] <- mean(rbinom(2,t,p))
  b.4.mean[i] <- mean(rbinom(4,t,p))
  b.32.mean[i] <- mean(rbinom(32,t,p))
}

options(digits=4)
c(mean(b.2.mean),sd(b.2.mean))
c(mean(b.4.mean),sd(b.4.mean))
c(mean(b.32.mean),sd(b.32.mean))

par(mfrow=c(1,3))
hist(b.2.mean,prob=T,xlim=c(0,4),main="표본크기 : 2",
     col="orange",border="red")
x1 <- seq(min(b.2.mean),max(b.2.mean),length=1000)
y1 <- dnorm(x1,mean=1,sd=sqrt(0.9)/sqrt(2))
lines(x1,y1,lty=2,lwd=2,col="blue")
hist(b.4.mean,prob=T,xlim=c(0,4),main="표본크기 : 4",
     col="orange",border="red")
x2 <- seq(min(b.4.mean),max(b.4.mean),length=1000)
y2 <- dnorm(x2,mean=1,sd=sqrt(0.9)/sqrt(4))
lines(x2,y2,lty=2,lwd=2,col="blue")
hist(b.32.mean,prob=T,xlim=c(0,4),main="표본크기 : 32",
     col="orange",border="red")
x3 <- seq(min(b.32.mean),max(b.32.mean),length=1000)
y3 <- dnorm(x3,mean=1,sd=sqrt(0.9)/sqrt(32))
lines(x3,y3,lty=2,lwd=2,col="blue")
par(mfrow=c(1,1))
  
  

