#install.packages("geepack")
library(tidyverse)
library(geepack)

#dogs
dados <- read.table("insulin(3).dat",header = F)
colnames(dados) <- c("id","tempo","nas","grupo")
#1)
##a)
#ar1

gee1 = geeglm(nas ~ tempo + grupo, data = dados, id = id, family = gaussian, corstr ="ar1")
summary(gee1)
#permutavel

gee1.Ex = update(gee1, corstr = "exchangeable")
summary(gee1.Ex)
#n estruturada

gee1.UN = update(gee1, corstr = "unstructured")
summary(gee1.UN)

## B)

sapply(list(gee1, gee1.Ex, gee1.UN), QIC)

QIC(gee1.Ex)#modelo 2


##c)modelo escolhido
se=summary(gee1.Ex)$coefficients["Std.err"]
lower = summary(gee1.Ex)$coefficients["Estimate"] + (qnorm(.025)*se)
upper = summary(gee1.Ex)$coefficients["Estimate"] + (qnorm(.975)*se)
cbind(summary(gee1.Ex)$coefficients["Estimate"] , lower, upper)





## c)ajuste do
#logistico

dados <- read.table("dogs(1).txt",header=T)

gee2 = geeglm(score~week+lateral+typsurg+age, data = dados, id = dog, family = binomial, corstr = "ar1")
summary(gee2)

se=summary(gee2)$coefficients["Std.err"]
lower = summary(gee2)$coefficients["Estimate"] + (qnorm(.025)*se)
upper = summary(gee2)$coefficients["Estimate"] + (qnorm(.975)*se)
cbind(summary(gee2)$coefficients["Estimate"] , lower, upper)


cbind(exp(summary(gee2)$coefficients["Estimate"]) , exp(lower), exp(upper))
