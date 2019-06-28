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
names(res)

par(mfrow=c(2,2))
plot(res)
par(mfrow=c(1,1))

qqnorm(res$residuals) ; qqline(res$residuals)
shapiro.test(res$residuals)

plot(hf.son$Father,res$residuals,
     ylab="Residuals",xlab="Father's height",main="Residual analysis") 
abline(0,0)

install.packages("lmtest")
library(lmtest)
(res2 <- dwtest(res))
names(res2)


# Ex 9-3

women <- women
str(women)
attach(women)
plot(height,weight,type="p",col="blue",lwd=2,main="Women data")

(fit <- lm(weight~height,data=women))
summary(fit)
cor.test(weight,height)
plot(weight~height,data=women)
abline(fit,col="red")
title(expression(italic(weight==3.45%*%height-87.52)))

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

shapiro.test(fit$residuals) # 정규성
install.packages("gvlma")
library(gvlma)
(gvmodel <- gvlma(fit)) # 선형성
summary(gvmodel)

(fit2 <- lm(weight~height+I(height^2),data=women))
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))
plot(weight~height,data=women)
lines(height,fitted(fit2),col="green")
title(expression(italic(weight==-7.348%*%height+0.083%*%height^2+261.878)))

(newfit <- lm(weight~ height + I(height^2), data=women[-c(13,15),]))
summary(newfit)

AIC(fit,fit2)

# 참고 : https://rstudio-pubs-static.s3.amazonaws.com/190997_40fa09db8e344b19b14a687ea5de914b.html


## Multiple

state.x77 <- state.x77
states <- as.data.frame(state.x77[,c("Murder","Population",
                                     "Illiteracy","Income","Frost")])
(fit1 <- lm(Murder~.,data=states))
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)
par(mfrow=c(1,1))

summary(gvlma(fit1))
shapiro.test(fit1$residuals)

install.packages("car")
library(car)
vif(fit1) # VIF
sqrt(vif(fit1))>2

(fit2 <- lm(Murder~Population+Illiteracy,data=states))
summary(fit2)

AIC(fit1,fit2)

step(fit1,direction="backward") # backward elimination
fit3 <- lm(Murder~1,data=states)
step(fit3,direction="forward",
     scope=~Population+Illiteracy+Income+Frost) # forward selection
step(fit3,direction="forward",
     scope=list(upper=fit1,lower=fit3))
step(fit1,direction="both") # stepwise regression

install.packages("leaps")
library(leaps)
subsets1 <- regsubsets(Murder~.,data=states,
                      method='seqrep',nbest=4) # all possible regression
summary(subsets1)
plot(subsets1)
subsets2 <- regsubsets(Murder~.,data=states,
                      method='exhaustive',nbest=4)
summary(subsets2)
plot(subsets2)

require(car)
subsets(subsets1,statistic="cp",main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="red")
subsets(subsets2,statistic="cp",main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="blue")


## Logistic

data <- read.csv("http://stats.idre.ucla.edu/stat/data/binary.csv")
str(data)
head(data)

data$rank <- as.factor(data$rank)
str(data)

train <- data[1:200,]
test <- data[201:400,]
model1 <- glm(admit~.,data=data,family="binomial")
summary(model1)
model2 <- glm(admit~gpa+rank,data=data,family="binomial")
summary(model2)

AIC(model1,model2)

par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))
