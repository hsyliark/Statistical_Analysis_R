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
( c.u <- gt(1-alpha,df=n-1) )
( p.value <- 1-pt(t.t,df=n-1) )

t.test(weight,mu=2800,alternative="greater")



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

