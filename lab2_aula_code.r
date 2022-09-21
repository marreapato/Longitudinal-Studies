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

#soma dos quadrados dos grupos

ids_media <- dados %>% group_by(id) %>% summarise(mean(y))
SQE <- sum((ids_media$`mean(y)`-y_p_p)^2)

SQD <- SQT-SQE


