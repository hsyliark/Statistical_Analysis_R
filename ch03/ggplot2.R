setwd("D:/Workplace/Statistical_Analysis_R/ch03")

library(ggplot2)

### Binomial

## Q2
x2 <- 0:10
y2 <- dbinom(x2,size=10,prob=0.8)
lab2 <- c(rep(1,7),2,rep(1,3))
(Q2 <- data.frame(x=x2,y=y2,lab=lab2))
ggplot(Q2,aes(x=x2,y=y2,fill=factor(lab2))) +
  geom_bar(stat='identity') +
  geom_text(aes(label=lab2),color="white",vjust=2) +
  scale_fill_manual(breaks=levels(Q2$lab2),values=c('black','red')) +
  xlab("try") + ylab("probability") +
  ggtitle("Question2") +
  theme(plot.title=element_text(
    color="black",size=20,face="bold.italic",hjust=0.5),
    axis.title.x=element_text(color="blue",size=15,face="bold"),
    axis.title.y=element_text(color="red",size=15,face="bold"))

## Q3
x3 <- 0:20
y3 <- dbinom(x3,size=20,prob=0.05)
lab3 <- c(rep(2,3),rep(1,18))
(Q3 <- data.frame(x=x3,y=y3,lab=lab3))
ggplot(Q3,aes(x=x3,y=y3,fill=factor(lab3))) +
  geom_bar(stat='identity') +
  geom_text(aes(label=lab3),color="white",vjust=2) +
  scale_fill_manual(breaks=levels(Q3$lab3),values=c('black','red')) +
  xlab("count") + ylab("probability") +
  ggtitle("Question3") +
  theme(plot.title=element_text(
    color="black",size=20,face="bold.italic",hjust=0.5),
    axis.title.x=element_text(color="blue",size=15,face="bold"),
    axis.title.y=element_text(color="red",size=15,face="bold"))

## Q4
x4 <- 0:20
y4 <- dbinom(x4,size=20,prob=0.2)
lab4 <- c(rep(1,2),rep(2,19))
(Q4 <- data.frame(x=x4,y=y4,lab=lab4))
ggplot(Q4,aes(x=x4,y=y4,fill=factor(lab4))) +
  geom_bar(stat='identity') +
  geom_text(aes(label=lab4),color="white",vjust=2) +
  scale_fill_manual(breaks=levels(Q4$lab4),values=c('black','red')) +
  xlab("recover") + ylab("probability") +
  ggtitle("Question4") +
  theme(plot.title=element_text(
    color="black",size=20,face="bold.italic",hjust=0.5),
    axis.title.x=element_text(color="blue",size=15,face="bold"),
    axis.title.y=element_text(color="red",size=15,face="bold"))

### Normal

## Q1
x1 <- seq(600,1000,by=0.01)
y1 <- dnorm(x1,mean=800,sd=40)
(Q1 <- data.frame(x=x1,y=y1))
ggplot(Q1,aes(x=x,y=y)) +
  geom_line(size=1) +
  geom_area(mapping=aes(x=ifelse(x<=750,x,0)),fill="seagreen") +
  geom_vline(xintercept=750,colour="darkturquoise",size=1) +
  geom_text(aes(x=750,y=0.0025,label="750"),colour="darksalmon",
            angle=90,vjust=1,text=element_text(size=10)) +
  xlim(600,1000) + ylim(0,0.012) +
  xlab("lifetime") + ylab("probability") +
  ggtitle("Question1") +
  theme(plot.title=element_text(
    color="black",size=20,face="bold.italic",hjust=0.5),
    axis.title.x=element_text(color="blue",size=15,face="bold"),
    axis.title.y=element_text(color="red",size=15,face="bold"))

## Q2
x2 <- seq(-10,31,by=0.01)
y2 <- dnorm(x2,mean=11,sd=4)
(Q2 <- data.frame(x=x2,y=y2))
ggplot(Q2,aes(x=x,y=y)) +
  geom_line(size=1) +
  geom_area(mapping=aes(x=ifelse(x>=20,x,0)),fill="seagreen") +
  geom_vline(xintercept=20,colour="darkturquoise",size=1) +
  geom_text(aes(x=20,y=0.02,label="20"),colour="darksalmon",
            angle=90,vjust=1,text=element_text(size=10)) +
  geom_vline(xintercept=qnorm(0.9,mean=11,sd=4),
             colour="darkcyan",size=1) +
  geom_text(aes(x=qnorm(0.9,mean=11,sd=4),y=0.02,
                label="약 16년 이상"),colour="darkslateblue",
            angle=90,vjust=1,text=element_text(size=10)) +
  xlim(-10,31) + ylim(0,0.125) +
  xlab("period") + ylab("probability") +
  ggtitle("Question2") +
  theme(plot.title=element_text(
    color="black",size=20,face="bold.italic",hjust=0.5),
    axis.title.x=element_text(color="blue",size=15,face="bold"),
    axis.title.y=element_text(color="red",size=15,face="bold"))

## Q3
x3 <- seq(40,100,by=0.01)
y3 <- dnorm(x3,mean=70,sd=8)
(Q3 <- data.frame(x=x3,y=y3))
ggplot(Q3,aes(x=x,y=y)) +
  geom_line(size=1) +
  geom_area(mapping=aes(x=ifelse(x>=80 & x<=90,x,0)),fill="seagreen") +
  geom_vline(xintercept=80,colour="darkturquoise",size=1) +
  geom_text(aes(x=80,y=0.03,label="80"),colour="darksalmon",
            angle=90,vjust=1,text=element_text(size=10)) +
  geom_vline(xintercept=90,colour="darkcyan",size=1) +
  geom_text(aes(x=90,y=0.03,label="90"),
            colour="darkslateblue",
            angle=90,vjust=1,text=element_text(size=10)) +
  xlim(40,100) + ylim(0,0.06) +
  xlab("grade") + ylab("probability") +
  ggtitle("Question3") +
  theme(plot.title=element_text(
    color="black",size=20,face="bold.italic",hjust=0.5),
    axis.title.x=element_text(color="blue",size=15,face="bold"),
    axis.title.y=element_text(color="red",size=15,face="bold"))

## Q4
x4 <- seq(-3.5,6.5,by=0.01)
y4 <- dnorm(x4,mean=1.5,sd=2)
(Q4 <- data.frame(x=x4,y=y4))
ggplot(Q4,aes(x=x,y=y)) +
  geom_line(size=1) +
  geom_area(mapping=aes(x=ifelse(x>=0 & x<=1,x,0)),fill="seagreen") +
  geom_area(mapping=aes(x=ifelse(x>=2 & x<=3,x,0)),fill="wheat") +
  geom_vline(xintercept=0,colour="darkturquoise",size=1) +
  geom_text(aes(x=0,y=0,label="0"),colour="darksalmon",
            angle=90,vjust=1,text=element_text(size=10)) +
  geom_vline(xintercept=1,colour="darkcyan",size=1) +
  geom_text(aes(x=1,y=0,label="1"),colour="darkslateblue",
            angle=90,vjust=1,text=element_text(size=10)) +
  geom_vline(xintercept=2,colour="firebrick",size=1) +
  geom_text(aes(x=2,y=0,label="2"),colour="darkgoldenrod",
            angle=90,vjust=1,text=element_text(size=10)) +
  geom_vline(xintercept=3,colour="thistle",size=1) +
  geom_text(aes(x=3,y=0,label="3"),colour="darkkhaki",
            angle=90,vjust=1,text=element_text(size=10)) +
  xlim(-0.5,3.5) + ylim(0,0.225) +
  xlab("X") + ylab("probability") +
  ggtitle("Question4") +
  theme(plot.title=element_text(
    color="black",size=20,face="bold.italic",hjust=0.5),
    axis.title.x=element_text(color="blue",size=15,face="bold"),
    axis.title.y=element_text(color="red",size=15,face="bold"))

  