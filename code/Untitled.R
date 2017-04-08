setwd('~/Desktop/qmss/spring17/thesis/code/')

install.packages("readstata13")
library(readstata13)
library(foreign)
library(arm)

data <- read.dta13('../data/spon_coop_for_prosocials.dta')
names(data)
head(data$svoangle)

hist(data$con_all)
plot(data$svoangle, data$con_all)

fit <- lm(data$con_all~data$svoangle) 
summary(fit)
coef(fit)
