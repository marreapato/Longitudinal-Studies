library(tidyverse)
library(lmtest)
library(lme4)
library(nlme)
library(MASS)
#install.packages("lmtest")
dados <- read.table("dogs.txt",header=T)

#a)

#install.packages("glmtoolbox")
library(glmtoolbox)

fit = glm(score ~ week+lateral+typsurg+age, data = dados,
            family=binomial("logit"))
summary(fit)
exp(-0.8)
set.seed(5)
envelope(fit)
colnames(dados)
#score seria 0
predict(fit,data.frame(lateral="b",typsurg=2,age=2,
                       week=1), type = "response")#previsao
################33
#b)

mod = glmer(score ~ week+lateral+typsurg+age + (1 | dog), data = dados,
            family=binomial("logit"))
summary(mod)

summary(mod)$coefficients[,1]

tau2 = as.numeric(data.frame(VarCorr(mod))["vcov"])

tau2/(tau2+(pi^2)/3)#icc
###########3
#c) modelo 2

#####
#d)

