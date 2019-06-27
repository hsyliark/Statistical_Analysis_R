setwd("D:/Workplace/Statistical_Analysis_R/ch08")


# Ex 8-1

x <- seq(0,15,by=0.01)
dc <- dchisq(x,df=3)
alpha <- 0.05
tol <- qchisq(0.95,df=3)
plot(x,dc,type="l",axes=F,ylim=c(-0.03,0.25),xlab="",ylab="")
abline(h=0)
tol.g <- round(tol,2)
polygon(c(tol.g,x[x>tol.g],15),c(0,dc[x>tol.g],0),col="red")
text(0,-0.03,"0",cex=0.8)
text(tol,-0.03,expression(chi[0.05]^2==2.14),cex=0.8)


mendel <- c(315,101,108,32)
(men.test <- chisq.test(mendel,p=c(9,3,3,1)/16))

abline(v=men.test$statistic)
cri <- round(men.test$statistic,2)
polygon(c(cri,x[x>cri],15),c(0,dc[x>cri],0),angle=45,lwd=1,
        density=20,col="blue")
text(cri+1,-0.03,expression(chi[0.9254]^2==0.47),cex=0.8)


# Ex 8-2

sns.c <- read.csv("data/snsbyage.csv",header=T,stringsAsFactors=F)
str(sns.c)

sns.c <- transform(sns.c,age.c=factor(age,levels=c(1,2,3),labels=c("20대","30대","40대")))
sns.c <- transform(sns.c,service.c=factor(service,levels=c("F","T","K","C","E"),ordered=T))

(c.tab <- table(sns.c$age.c,sns.c$service.c))
(a.n <- margin.table(c.tab,margin=1))
(s.n <- margin.table(c.tab,margin=2))
(s.p <- s.n/margin.table(c.tab))
(expected <- a.n%*%t(s.p))

(o.e <- c.tab-expected)
(t.t <- sum((o.e)^2/expected))

qchisq(0.95,df=8)

result <- chisq.test(c.tab)
names(result)
result$observed
result$expected
addmargins(result$expected)


# Ex 8-3

(ucba.tab <- apply(UCBAdmissions,c(1,2),sum))
round(prop.table(ucba.tab,margin=2)*100,1)

(a.n <- margin.table(ucba.tab,margin=1))
(g.n <- margin.table(ucba.tab,margin=2))

(a.p <- a.n/margin.table(ucba.tab))
(g.p <- g.n/margin.table(ucba.tab))

(expected <- margin.table(ucba.tab)*(a.p%*%t(g.p)))
addmargins(expected)

(o.e <- (ucba.tab-expected)^2/expected)
addmargins(o.e)

(chisq.t <- sum(o.e))
qchisq(0.95,df=1)
1-pchisq(chisq.t,df=1)

(o.e2 <- (abs(ucba.tab-expected)-0.5)^2/expected)
sum(o.e2)

chisq.test(ucba.tab)
chisq.test(ucba.tab,correct=F)
