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

#soma quadrados dos residuos (verificar)
SQr <- sum((dados$y-ids_media$`mean(y)`-medias_tempos$media+y_p_p)^2)
#54 individuos
m
vetor_medias=NULL
for(i in 1:m){
vetor_medias <- append(rep(ids_media$`mean(y)`[i],5),x = vetor_medias)
}
vetor_medias

dados_double_sum <- dados %>% group_by(id,day) %>% cbind(media_tempos=rep(medias_tempos$media,54),ind_medias=vetor_medias)

SQr <- sum((dados_double_sum$y-dados_double_sum$ind_medias-dados_double_sum$media_tempos+y_p_p)^2)

#QUADRADO MEDIO ENTRE

QMi=SQi/(m-1)

#QUADRADO MEDIO DENTRO

QMt=SQt/(n-1)
#QUADRADO MEDIO RES

QMr=SQr/((m-1)*(n-1))
df_res=(m-1)*(n-1)
#total
(m*n)-1

#SQy

SQy <- sum((dados$y-y_p_p)^2)

#modificar abaixo
#estatistica F 53,53*4
#F INDIVIDUOS

F_SNED=QMi/QMr
F_SNED
qf(0.05,df1 =53 ,df2 = 53*4,lower.tail = F)

#tempos

F_SNED=QMt/QMr
F_SNED
qf(0.05,df1 =4 ,df2 = 53*4,lower.tail = F)
#n rejeita h0

dados.aov = aov(y~factor(day) + Error(factor(id)), data = dados)
dados.aov = aov(y~factor(day) + (factor(id)), data = dados)
summary(dados.aov)

#d)

y.v = df_spread

rfactor = factor(c("t1","t2","t3","t4","t5"))

C = contr.poly(5)

num.est=m*((t(colMeans(y.v))%*%C))*(t(colMeans(y.v))%*%(C))

den.est=QMr
F_sned=num.est/den.est

pcomp1 = 1-pf(F_sned[1], 1, df_res)
pcomp1

######

#2)

dados2 <- read.csv("fosfato.csv")

dados2_gathe <- gather(data = dados2,key=tempo,value=fosfato,X0,X0.5,X1,X1.5,X2,X3,X4,X5)



dados2_gathe %>% group_by(tempo) %>% summarise(media=mean(fosfato),desvio=sd(fosfato))

dados_cor <- cor(dados2[,-c(1,2)])
corrplot(dados_cor,method = "number")

#b


with(dados2_gathe, interaction.plot(tempo, Grupo, fosfato, ylim = c(2, 5), lty = c(1, 2), lwd = 3, ylab = "media de
                         fosfato", xlab = "tempo", trace.label = "Grupo"))
