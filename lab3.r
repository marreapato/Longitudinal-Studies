library(tidyverse)

dados1 <- read.table("LAB2Q1.txt")

dados <- spread(dados1,day,y)

arv.mod = lm(cbind(dados$`152`, dados$`174`, dados$`201`, dados$`227`, dados$`258`) ~ 1, data=dados)

idata = data.frame(tempo=ordered(1:5))

library(car)
ajuste=Anova(arv.mod, idata=idata, idesign=~tempo, type='III')

summary(Anova(arv.mod, idata=idata, idesign=~tempo, type="III"))

ajuste$P


####################################################

#2)

dados2 <- read.csv("fosfato.csv")

dados <- dados2


arv.mod = lm(cbind(dados$X0, dados$X0.5, dados$X1, dados$X1.5, dados$X2, dados$X3, dados$X4, dados$X5) ~ 1, data=dados)

idata = data.frame(tempo=ordered(1:8))

library(car)
ajuste=Anova(arv.mod, idata=idata, idesign=~tempo, type='III')

summary(Anova(arv.mod, idata=idata, idesign=~tempo, type="III"))

ajuste$P
