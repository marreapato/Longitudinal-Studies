library(tidyverse)

dados <- read.table("LAB2Q1.txt")

dados %>% group_by(day) %>% summarise(media=mean(y),desvios=sd(y))#medias e devios

#matriz de cor

df_spread <- spread(dados,day,y)
df_spread<- df_spread[,-c(1)]
dados_cor <- cor(df_spread)
corrplot(dados_cor,method = "number")

#grafico dos perfis de crescimento
library(lattice)

xyplot(y~as.numeric(day), data = dados, type = 'l', group = id, xlab = 'Dia', col.line
       = 'gray20', ylab = 'Perfis de Crescimento')


#tabela anova

#soma dos quadrados totais

y_p_p=mean(dados$y)
SQT=sum((dados$y-y_p_p)^2)
n=nrow(dados)

#soma dos quadrados entre grupos

ids_media <- dados %>% group_by(id) %>% summarise(mean(y))
SQE <- sum((ids_media$`mean(y)`-y_p_p)^2)

#SOMA DOS QUADRADOS Dentro

SQD <- SQT-SQE
g=nrow(ids_media)

#QUADRADO MEDIO ENTRE

QME=SQE/(g-1)

#QUADRADO MEDIO DENTRO

QMD=SQD/(n-g)

#estatistica F
#F 53,216

F_SNED=QME/QMD
F_SNED
qf(0.05,df1 =53 ,df2 = 216)

#n rejeita h0

dados.aov = aov(y~day + Error(id), data = dados)

summary(dados.aov)
