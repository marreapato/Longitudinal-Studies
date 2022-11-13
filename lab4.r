library(tidyverse)
library(lmtest)
#install.packages("lmtest")

dados <- read.table("insulin.dat",header = F)
colnames(dados) <- c("id","tempo","nas","grupo")


#a)
ggplot(dados,aes(x=dados$tempo, y=dados$nas)) +geom_jitter() + 
  geom_smooth(method='lm', formula= y~x)+labs(x="Tempo",y="Nível de Açucar no Sangue")


fit <- lm(dados$nas~dados$tempo)
plot(fit)
bptest(fit)#ssem heterodasticide
shapiro.test(fit$residuals)#com normalidade

anova(fit)

#b)

