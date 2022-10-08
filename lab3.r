library(tidyverse)

dados1 <- read.table("LAB2Q1.txt")

dados <- spread(dados1,day,y)

arv.mod = lm(cbind(dados$`152`, dados$`174`, dados$`201`, dados$`227`, dados$`258`) ~ 1, data=dados)

idata = data.frame(tempo=ordered(1:5))

library(car)
ajuste=Anova(arv.mod, idata=idata, idesign=~tempo, type='III')

ajuste

ajuste2=Anova(arv.mod, idata=idata, idesign=~tempo, type='III',test.statistic = "Wilks")

ajuste2

ajuste3=Anova(arv.mod, idata=idata, idesign=~tempo, type='III',test.statistic = "Roy")

ajuste3

ajuste4=Anova(arv.mod, idata=idata, idesign=~tempo, type='III',test.statistic = "Hotelling-Lawley")

ajuste4

sumario_manova=summary(Anova(arv.mod, idata=idata, idesign=~tempo, type="III"))

sumario_manova$multivariate.tests$tempo

sumario_manova$sphericity.tests

sumario_manova

ajuste$P





####################################################

#2)

dados2 <- read.csv("fosfato.csv")

dados <- dados2


arv.mod = lm(cbind(dados$X0, dados$X0.5, dados$X1, dados$X1.5, dados$X2, dados$X3, dados$X4, dados$X5) ~ Grupo, data=dados)

idata = data.frame(tempo=ordered(1:8))

library(car)
ajuste=Anova(arv.mod, idata=idata, idesign=~tempo, type='III')

ajuste

ajuste2=Anova(arv.mod, idata=idata, idesign=~tempo, type='III',test.statistic = "Wilks")

ajuste2

ajuste3=Anova(arv.mod, idata=idata, idesign=~tempo, type='III',test.statistic = "Roy")

ajuste3

ajuste4=Anova(arv.mod, idata=idata, idesign=~tempo, type='III',test.statistic = "Hotelling-Lawley")

ajuste4


summary(Anova(arv.mod, idata=idata, idesign=~tempo, type="III"))

ajuste$P
