setwd('~/Desktop/qmss/spring17/thesis/code/')

install.packages("readstata13")
library(readstata13)
library(foreign)
library(arm)

data <- read.dta13('../data/thirdparty/spon_coop_for_prosocials.dta')
names(data)
head(data$svoangle)



plot(data$svoangle, data$con_all)

write.csv(data, file = "~/Desktop/qmss/spring17/thesis/data/thirdparty/spon_coop_prosocials.csv")

fit <- lm(data$con_all~data$svoangle) 
summary(fit)
coef(fit)


