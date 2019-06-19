setwd("D:/Workplace/Statistical_Analysis_R/ch02")

data <- read.csv("2010년 인구사항.csv",header=F,na.strings=c("."))
str(data)
data$V1 <- factor(data$V1,levels=c(1,2),labels=c("남자","여자"))
data$V3 <- factor(data$V3,levels=1:14,labels=c("가구주","가구주의 배우자","자녀",
                                               "자녀의 배우자","가구주의 부모",
                                               "배우자의 부모","손자녀, 그 배우자",
                                               "증손자녀, 그 배우자","조부모",
                                               "형제자매, 그 배우자",
                                               "형제자매의 자녀, 그 배우자",
                                               "부모의 형제자매, 그 배우자","기타 친인척",
                                               "그외같이사는사람"))
data$V4 <- factor(data$V4,levels=1:8,
                  labels=c("안 받았음","초등학교","중학교",
                           "고등학교","대학-4년제 미만","대학-4년제 이상",
                           "석사과정","박사과정"))
str(data)
save.image("data.rda")

# 1. graph

library(ggplot2)

cars
par(mfrow=c(1,2))
plot(cars$speed,cars$dist,
     main="속도와 제동거리",xlab="속도(mph)",ylab="제동거리(ft)",
     pch=1,col="red")
plot(jitter(cars$speed),jitter(cars$dist),
     main="속도와 제동거리",xlab="속도(mph)",ylab="제동거리(ft)",
     pch=1,col="blue")
par(mfrow=c(1,1))

Nile
par(mfrow=c(1,2))
plot(Nile,main="Nile강의 연도별 유량 변화",xlab="연도",ylab="유량")
plot(Nile,type="p",main="Nile강의 연도별 유량 변화",xlab="연도",ylab="유량")
par(mfrow=c(1,1))
plot.ts(Nile)
year <- as.data.frame(1871:1970)
df_Nile <- as.data.frame(Nile)
df_Nile <- cbind.data.frame(year,df_Nile)
df_Nile <- data.frame(year=time(Nile),Nile=as.matrix(Nile))
colnames(df_Nile) <- c("year","Nile")
str(df_Nile)
ggplot(df_Nile,aes(x=year,y=Nile)) +
  geom_line(color="green",size=2) +
  ggtitle("Nile강의 연도별 유량 변화") +
  theme(plot.title=element_text(color="darkblue",size=16,
                                face="bold",hjust=0.5))

load("data.rda")
tableV5 <- table(data$V5)
tableV5
barplot(tableV5,main="출생아(남자)별 빈도",xlab="출생아수",ylab="빈도")
df_tableV5 <- as.data.frame(tableV5)
num <- 1:13
df_tableV5 <- cbind.data.frame(df_tableV5,num)
colnames(df_tableV5) <- c("born","freq","num")
df_tableV5$born <- factor(df_tableV5$born, levels = df_tableV5$born[order(df_tableV5$num)])
df_tableV5 <- data.frame(born=born,freq=df_tableV5$Freq)
ggplot(df_tableV5,aes(x=born,y=freq,fill=born)) +
  geom_bar(stat="identity") +
  xlab("출생아수") + ylab("빈도") +
  ggtitle("출생아(남자)별 빈도") +
  theme(plot.title=element_text(color="darkblue",size=16,
                                face="bold",hjust=0.5))
  

tableV1.V4 <- table(data$V1,data$V4)
tableV1.V4
barplot(tableV1.V4,legend.text=T,col=c("orange","green"),
        main="학력에 따른 성별 인원수",xlab="학력",ylab="빈도")
df_tableV1.V4 <- as.data.frame(tableV1.V4)
df_tableV1.V4$Var1 <- factor(df_tableV1.V4$Var1, levels = df_tableV1.V4$Var1[order(1:2)])
df_tableV1.V4$Var2 <- factor(df_tableV1.V4$Var2, levels = df_tableV1.V4$Var2[order(1:8)])
colnames(df_tableV1.V4) <- c("성별","학력","인원수")
options(scipen=6)
ggplot(df_tableV1.V4,aes(x=학력,y=인원수)) +
  geom_bar(aes(fill=성별),stat="identity") +
  xlab("학력") + ylab("인원수") +
  ggtitle("학력에 따른 성별 인원수") +
  theme(plot.title=element_text(color="darkblue",size=16,
                                face="bold",hjust=0.5))
  
hist(data$V2,main="연령별 분포",xlab="연령",ylab="빈도")
hist(data$V2,breaks=c(seq(0,90,10)),right=F,
     main="연령별 분포",xlab="연령",ylab="빈도")
hist(data$V2,probability=T,
     main="연령별 분포",xlab="연령",ylab="밀도")
age <- as.data.frame(data$V2)
colnames(age) <- "age"
ggplot(age,aes(age,fill=I("green"),col=I("red"))) +
  geom_histogram(binwidth=5) +
  xlab("연령") + ylab("빈도") +
  ggtitle("연령별 분포") +
  theme(plot.title=element_text(color="blue",size=16,
                                face="bold",hjust=0.5))
  
pie(table(data$V4),main="학력수준별 비중",cex=0.8)
table(data$V4)
edu <- as.data.frame(table(data$V4))
edu$Var1 <- factor(edu$Var1, levels = edu$Var1[order(1:8)])
library(dplyr)
edu <- edu %>%
  arrange(desc(Var1)) %>%
  mutate(edu.ylab=0.5*Freq) %>%
  mutate(edu.ylab2=cumsum(Freq)-0.5*Freq) %>%
  mutate(pct=paste(round(Freq/sum(Freq)*100,2),"%"))
colnames(edu) <- c("학력","빈도","lab1","lab2","pct")
ggplot(edu,aes(x="",y=빈도,fill=학력)) +
  geom_bar(width=1,stat="identity",color="white") +
  coord_polar("y",start=0) +
  geom_text(aes(y=lab2,label=pct),color="black") +
  theme_void() +
  ggtitle("학력수준별 비중") +
  theme(plot.title=element_text(color="navy",size=16,
                                face="bold",hjust=0.5))
ggplot(edu,aes(x=학력,y=빈도,fill=학력)) +
  geom_bar(width=1,stat="identity",color="white") +
  coord_polar() +
  geom_text(aes(y=lab1,label=pct),color="black") +
  theme_void() +
  ggtitle("학력수준별 비중") +
  theme(plot.title=element_text(color="navy",size=16,
                                face="bold.italic",hjust=0.5))
