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

ajuste$P$tempo


  
dados <- read.table("LAB2Q1.txt")

medias_tempos=dados %>% group_by(day) %>% summarise(media=mean(y),desvios=sd(y))#medias e devios
medias_tempos

polinomio=ajuste$P$tempo#polinomi

ajuste$SSP$tempo

diag(ajuste$SSP$tempo)

df_spread <- spread(dados,day,y)
df_spread<- df_spread[,-c(1)]
m=length(unique(dados1$id)) #individuos


dfmat <- t(as.matrix(polinomio))%*%(t(as.matrix(df_spread))%*%as.matrix(df_spread)-m*as.matrix(medias_tempos[,2])%*%
  t(as.matrix(medias_tempos[,2])))%*%(as.matrix(polinomio))#equivale a

ssp_matrix=ajuste$SSP$tempo#sum of squares

ssr_matrix <- sumario_manova$SSPE$tempo#sum of square residuals

ssp_matrix[upper.tri(ssp_matrix)] <- NA
ssp_matrix

ssr_matrix[upper.tri(ssr_matrix)] <- NA
ssr_matrix

ssp_diag=diag(ssp_matrix)
qmrs=diag(ssr_matrix)/(m-1)

ssp_diag/qmrs
n_1=length(colnames(df_spread))-1

ssp_matrix
ssr_matrix

round(pf(ssp_diag/qmrs,1,m-1,lower.tail = F),4)

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
