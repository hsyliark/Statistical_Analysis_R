setwd("D:/Workplace/Statistical_Analysis_R/ch06")

data <- read.csv("2016.6th.csv",header=T)
str(data)

tmp <- subset(data,data$나이==7)
height.p <- tmp$X104.키

set.seed(9)
height <- height.p[sample(length(height.p),15)]
height

mean(height)
sd(height)
t.test(height,mu=1220)
t.test(height,mu=1220,alternative="greater")


# Ex 6-1

data <- read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt",
                   header=F)
str(data)
names(data) <- c("time","gender","weight","minutes")
tmp <- subset(data,gender==1)
weight <- tmp[[3]]

barx <- mean(weight)
s <- sd(weight)
n <- length(weight)
h0 <- 2800
( t.t <- (barx-h0)/(s/sqrt(n)) )

alpha <- 0.05
( c.u <- qt(1-alpha,df=n-1) )
( p.value <- 1-pt(t.t,df=n-1) )

t.test(weight,mu=2800,alternative="greater")


# 그림 6-8

par(mar=c(0,1,1,1))
x <- seq(-3,3,by=0.001)
y <- dt(x,df=n-1)
plot(x,y,type="l",axes=F,ylim=c(-0.02,0.38),
     main="",xlab="t",ylab="")
abline(h=0)

polygon(c(c.u, x[x>c.u], 3), c(0, y[x>c.u], 0), col=2)
text(c.u, -0.02, expression(t[0.05]==1.74))
text(1.8, 0.2, expression(alpha==0.05), cex=0.8)
arrows(1.8,0.18,1.8,0.09,length=0.05)

polygon(c(t.t, x[x>t.t], 3), c(0, y[x>t.t], 0), density=20, angle=45)
text(t.t, -0.02, paste("t=", round(t.t,3)), pos=4)
text(2.65, 0.1, expression(plain(P)(T>2.233) == 0.0196), cex=0.8)
arrows(2.7,0.08,2.5,0.03,length=0.05)


# Ex 6-2

tmp <- read.table("data/restitution.txt",header=T)
rel <- ifelse(tmp$rst < 0.4134 | tmp$rst > 0.4374, 1, 0)

n <- length(rel)
nos <- sum(rel)
sp <- nos/n
hp <- 0.1
(z <- (sp-hp)/sqrt((hp*(1-hp))/n))

alpha <- 0.05
( c.u <- qnorm(1-alpha) )
( p.value <- 1-pnorm(z) )

prop.test(nos,n,p=0.1,alternative="greater",correct=F)


# 그림 6-9

par(mar=c(0,1,1,1))
x <- seq(-3,3,by=0.001)
y <- dnorm(x)
plot(x,y,type="l",axes=F,ylim=c(-0.02,0.4),
     main="",xlab="z",ylab="")
abline(h=0)

polygon(c(c.u, x[x>c.u], 3), c(0, y[x>c.u], 0), col=2)
text(c.u, -0.02, expression(z[0.05]==1.645))

polygon(c(z, x[x>z], 3), c(0, y[x>z], 0), density=20, angle=45)
text(c.u, -0.02, paste("z=", round(z, 3)))
text(1.2, 0.3, paste("P(Z>z)=", round(p.value, 3)), cex=0.8)

