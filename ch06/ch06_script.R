setwd("D:/Workplace/Statistical_Analysis_R/ch06")

data <- read.csv("2016.6th.csv",header=T)
str(data)

tmp <- subset(data,data$나이==7)
height.p <- tmp$X104.키

set.seed(9)
height <- height.p[sample(length(height.p),15)]
height

mean(height)
t.test(height,mu=1220,alternative="greater")


# Ex 6-1



