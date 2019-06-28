setwd("D:/Workplace/Statistical_Analysis_R/ch09")

hf <- read.table("http://www.randomservices.org/random/data/Galton.txt",
                 header=T,stringsAsFactors=F)
str(hf)
hf$Gender <- factor(hf$Gender,levels=c("M","F"))
str(hf$Gender)
hf.son <- subset(hf,Gender=="M")
hf.son <- hf.son[c("Father","Height")]
str(hf.son)
plot(hf.son$Father,hf.son$Height,xlab="아버지의 키",
     ylab="아들의 키",main="아버지와 아들의 키")
abline(v=mean(hf.son$Father),col=2,lty=2)
abline(h=mean(hf.son$Height),col=2,lty=2)


# Ex 9-1

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)
cov.num <- sum((hf.son$Father-f.mean)*(hf.son$Height-s.mean))
(cov.xy <- cov.num/(nrow(hf.son)-1))
cov(hf.son$Father,hf.son$Height)

(r.xy <- cov.xy/(sd(hf.son$Father)*sd(hf.son$Height)))
cor(hf.son$Father,hf.son$Height)


# Ex 9-2

mean.x <- mean(hf.son$Father)
mean.y <- mean(hf.son$Height)

sxy <- sum((hf.son$Father-mean.x)*(hf.son$Height-mean.y))
sxx <- sum((hf.son$Father-mean.x)^2)

(b1 <- sxy/sxx)
(b0 <- mean.y-b1*mean.x)

(res <- lm(Height~Father,data=hf.son))
summary(res)
anova(res)
str(res)
