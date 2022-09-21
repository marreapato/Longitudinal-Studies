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
