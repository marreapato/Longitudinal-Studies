library(tidyverse)

dados <- read.table("LAB2Q1.txt")

medias_tempos=dados %>% group_by(day) %>% summarise(media=mean(y),desvios=sd(y))#medias e devios

#matriz de cor

df_spread <- spread(dados,day,y)
df_spread<- df_spread[,-c(1)]
dados_cor <- cor(df_spread)
corrplot(dados_cor,method = "number")

#grafico dos perfis de crescimento
library(lattice)

xyplot(y~as.numeric(day), data = dados, type = 'l', group = id, xlab = 'Dia', col.line
       = 'gray20', ylab = 'Perfis de Crescimento')


#tabela anova medidas repetidas


y_p_p=mean(dados$y)


#soma dos quadrados individuos
#4 pontos no tempo
n=nrow(medias_tempos)
ids_media <- dados %>% group_by(id) %>% summarise(mean(y))
m=nrow(ids_media)
SQi <- n*sum((ids_media$`mean(y)`-y_p_p)^2)



#SOMA DOS QUADRADOS tempos

SQt <- m*sum((medias_tempos$media-y_p_p)^2)

#QUADRADO MEDIO ENTRE

QMi=SQi/(m-1)

#QUADRADO MEDIO DENTRO

QMt=SQt/(n-1)
#modificar abaixo
#estatistica F
#F 53,216

F_SNED=QME/QMD
F_SNED
qf(0.05,df1 =53 ,df2 = 216)

#n rejeita h0

dados.aov = aov(y~factor(day) + Error(factor(id)), data = dados)

summary(dados.aov)


sum((dados$y-ids_media$`mean(y)`)^2)
