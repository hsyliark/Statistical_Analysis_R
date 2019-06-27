setwd("D:/Workplace/Statistical_Analysis_R/ch07")

data <- read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt",
                   header=F)
nrow(data)
ncol(data)
str(data)

names(data) <- c("time.24Hrs","gender","weight","minutes")

g1 <- data$gender
str(g1)
g2 <- data[,2]
str(g2)
g3 <- data["gender"]
str(g3)
g4 <- data[[2]]
str(g4)
g5 <- data[["gender"]]
str(g5)

gg1 <- data[,c(2,4)]
str(gg1)
gg2 <- data[c("gender","minutes")]
str(gg2)

str(data[data$gender==2,])
str(subset(data,gender==2))

male.m <- mean(data$weight)
str(data[data$gender==2 & data$weight > male.m,])
str(subset(data, (gender==2) & (data$weight > male.m)))
str(data[data$gender==2 & data$weight > male.m, c(2, 4)])
str(subset(data, (gender==2) & (weight > male.m), select=c(2, 4)))

chapter7 <- data[, c(2,3)]
write.table(chapter7, "data/chapter7.txt")
write.table(chapter7, "data/chapter7.txt", row.names=F)

ad <- read.csv("data/age.data.csv",header=T)
str(ad)
summary(ad)

ad$score <- ifelse(ad$score==99, NA, ad$score)
summary(ad)

ad2 <- read.csv("data/age.data.csv",header=T,na.strings=c("99"))
summary(ad2)
mean(ad$score,na.rm=T)

nonna.sum <- sum(ad$score[!is.na(ad$score)])
nonna.length <- length(ad$score[!is.na(ad$score)]) 
nonna.sum/nonna.length

ad$scale <- factor(ad$scale)
ad$sex <- factor(ad$sex)
str(ad)
summary(ad)

length(ad$age[ad$scale=="1"])
mean(ad$age[ad$scale=="1"])
sd(ad$age[ad$scale=="1"])

install.packages("doBy")
library(doBy)
summaryBy(age~scale,data=ad,FUN=c(length))
summaryBy(age~scale,data=ad,FUN=c(mean,sd),na.rm=T)


# Ex 7-1

data <- read.table("data/chapter7.txt",header=T)
boy <- subset(data,gender==1)
girl <- subset(data,gender==2)
shapiro.test(boy$weight)
shapiro.test(girl$weight)
qqnorm(boy$weight) ; qqline(boy$weight)
qqnorm(girl$weight) ; qqline(girl$weight)

kruskal.test(weight~gender,data) # Kruskal-Wallis test

# Assume normality
var.test(data$weight~data$gender)
t.test(data$weight~data$gender,mu=0,alternative="less",
       var.equal=T)


# Ex 7-2

install.packages("PairedData")
library(PairedData)
install.packages("psych")
library(psych)
anorexia <- read.csv("data/01.anorexia.csv",header=T)
summary(anorexia)
describe(anorexia)
attach(anorexia)
shapiro.test(Prior-Post)
n <- length(Prior-Post)
m <- mean(Prior-Post)
s <- sd(Prior-Post)
( t.t <- m/(s/sqrt(n)) )
t.test(Prior,Post,paired=T,alternative="less")


# Ex 7-3

ad <- read.csv("data/age.data.csv",header=T)
ad$scale <- as.factor(ad$scale)

y1 <- ad$age[ad$scale=="1"]
y2 <- ad$age[ad$scale=="2"]
y3 <- ad$age[ad$scale=="3"]

y1.mean <- mean(y1)
y2.mean <- mean(y2)
y3.mean <- mean(y3)

sse.1 <- sum((y1-y1.mean)^2)
sse.2 <- sum((y2-y2.mean)^2)
sse.3 <- sum((y3-y3.mean)^2)

( sse <- sse.1+sse.2+sse.3 )
( dfe <- (length(y1)-1)+(length(y2)-1)+(length(y3)-1) )

y <- mean(ad$age)

sst.1 <- length(y1)*sum((y1.mean-y)^2)
sst.2 <- length(y2)*sum((y2.mean-y)^2)
sst.3 <- length(y3)*sum((y3.mean-y)^2)

( sst <- sst.1+sst.2+sst.3 )
( dft <- length(levels(ad$scale))-1 )

( tsq <- sum((ad$age-y)^2) )
( ss <- sst+sse )

mst <- sst/dft
mse <- sse/dfe
(f.t <- mst/mse)

alpha <- 0.05
(tol <- qf(1-alpha,2,147))

(p.value <- 1-pf(f.t,2,147))

ow <- lm(age~scale,data=ad)
anova(ow)
oneway.test(age~scale,data=ad,var.equal=T)

out <- aov(ow)
summary(out)
(ph <- TukeyHSD(out))
plot(ph)

pairwise.t.test(ad$age,ad$scale,p.adj="none")

install.packages("multcomp")
library(multcomp)
install.packages("agricolae") # Duncan, Scheffe test
library(agricolae)

duncan.test(out,"scale",alpha=0.05,group=T,console=T)
install.packages("laercio")
library(laercio) 
LDuncan(out,"scale")

scheffe.test(out,"scale",alpha=0.05,group=T,console=T)
