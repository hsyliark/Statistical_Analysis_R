setwd("D:/Workplace/Statistical_Analysis_R/ch02")


# 2. parameter & statistics

ranicafe <- read.csv("cafedata.csv",header=T,stringsAsFactors=F)
str(ranicafe)
head(ranicafe)
summary(ranicafe)
dim(ranicafe)

ranicafe$Coffees <- as.numeric(ranicafe$Coffees)
sort(ranicafe$Coffees)
sort(ranicafe$Coffees,decreasing=T)
min(ranicafe$Coffees,na.rm=T) 
max(ranicafe$Coffees,na.rm=T)
stem(ranicafe$Coffees)
hist(ranicafe$Coffees)
mode(ranicafe$Coffees)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(ranicafe$Coffees)

rc <- ranicafe$Coffees
weight <- 1/length(rc)
sum(rc*weight,na.rm=T)
mean(rc,na.rm=T)
which(rc==21 | rc==22)

rc[rc==max(rc,na.rm=T)] <- 480
mean(rc,na.rm=T)

rc <- ranicafe$Coffees
median.idx <- (length(rc)+1)/2
rc.srt <- sort(rc)
rc.srt[median.idx]
median(rc,na.rm=T)

rc[rc==max(rc,na.rm=T)] <- 480
median(rc,na.rm=T)

height <- c(164,166,168,170,172,174,176)
(height.m <- mean(height))
(height.dev <- height-height.m)
sum(height.dev)
(height.dev2 <- height.dev^2)
sum(height.dev2)
mean(height.dev2)
sqrt(mean(height.dev2))
var(height)
sd(height)

rc <- ranicafe$Coffees
rc.m <- mean(rc,na.rm=T)
rc.sd <- sd(rc,na.rm=T)
cat("커피 판매량",round(rc.m,1),"+-",round(rc.sd,2),"잔")

ranicafe$Juices <- as.numeric(ranicafe$Juices)
str(ranicafe)
rc <- ranicafe$Coffees
rj <- ranicafe$Juices
(rc.cv <- round(sd(rc,na.rm=T)/mean(rc,na.rm=T),3))
(rj.cv <- round(sd(rj,na.rm=T)/mean(rj,na.rm=T),3))

(qs <- quantile(rc,na.rm=T))
(qs[4]-qs[2])
IQR(rc,na.rm=T)
boxplot(rc,main="커피 판매량에 대한 상자도표")

(Q <- quantile(cars$dist))
(ll <- Q[2]-1.5*IQR(cars$dist))
(ul <- Q[4]+1.5*IQR(cars$dist))
cars$dist[cars$dist < ll]
cars$dist[cars$dist > ul]
boxplot(cars$dist,main="Boxplot of Distance")
