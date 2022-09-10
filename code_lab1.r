library(tidyverse)
#install.packages("tidyverse")

dados <- read.table("LAB1Q1.txt",header = T)

summary(dados)


dados %>% group_by(Sexo) %>% summarise(mean(Tempo,na.rm=T))
dados %>% group_by(Treinamento) %>% summarise(mean(Tempo,na.rm=T))
dados %>% group_by(Treinamento,Sexo) %>% summarise(mean(Tempo,na.rm=T))

boxplot(dados$Tempo~dados$Sexo+dados$Treinamento)

t.test(dados$Tempo~dados$Treinamento)

df_spread <- spread(dados,Treinamento,Tempo)

df_spread$dif_tempo <- df_spread$Depois-df_spread$Antes

#dados2 = reshape(dados, 
 #                timevar = "Tempo",
  #               v.names = "Placa",
   #              idvar = c("Id", "Sexo"),
    #             direction = "wide")

m1 <- lm(df_spread$dif_tempo~1)
summary(m1)

7.1903-0.9169

m1 <- lm(df_spread$dif_tempo~df_spread$Sexo)
summary(m1)


m1 <- lm(Depois~Sexo+Antes,data = df_spread)
summary(m1)


m2 <- lm(dif_tempo~Sexo+Antes,data = df_spread)
summary(m2)


predict(m1,data.frame(Sexo="M",Antes=mean(df_spread$Antes, na.rm=TRUE)),se=F)

predict(m1,data.frame(Sexo="F",Antes=mean(df_spread$Antes, na.rm=TRUE)),se=F)

predict(m2,data.frame(Sexo="M",Antes=mean(df_spread$Antes, na.rm=TRUE)),se=F)

predict(m2,data.frame(Sexo="F",Antes=mean(df_spread$Antes, na.rm=TRUE)),se=F)


#####################33


dados <- read.table("insulin.dat",header = F)
colnames(dados) <- c("id","tempo","nas","grupo")

#dados$tempo <- as.factor(dados$tempo)

dados %>% group_by(tempo) %>% summarise(media=mean(nas),desvio=sd(nas))

#install.packages("corrplot")
library(corrplot)

df_spread <- spread(dados,tempo,nas)
df_spread<- df_spread[,-c(1,2)]

dados_cor <- cor(df_spread)
corrplot(dados_cor)

plot(nas~as.numeric(tempo),data = dados, pch = ".", xlab = "Tempo(Horas)", ylab="Nivel de a ̧c ́ucar
      no sangue")

library(lattice)

xyplot(nas~as.numeric(tempo), data = dados, type = 'l', group = id, xlab = 'Tempo(horas)', col.line
  = 'gray20', ylab = 'Nivel de açucar no sangue')



s = sample(unique (dados$id), size = 5)

plot(nas~as.numeric(tempo), data = dados, col = "gray50", xlab = "Tempo", ylab = "N ́ıvel de a ̧cuucar
      no sangue")

for (i in 1:5) {
  lines (dados$tempo[dados$id == s[i]],
         dados$nas[dados$id == s[i]], lwd = 1.5)
}

library(gplots)
#install.packages("gplots")
plotmeans(nas~tempo, data=dados,barwidth = 3)

## c


plot(nas~tempo, data = dados, col = 'gray50', pch = '.', xlab = 'Tempo(Horas)',ylab="açucar no sangue")
      #kernel
with (dados, {
  lines(ksmooth(tempo, nas, 'normal', bandwidth = 1), col = 2)
  lines(ksmooth(tempo, nas, 'normal', bandwidth = 0.25), col = 3)
  lines(ksmooth(tempo, nas, 'normal', bandwidth = 0.5), col = 4)
})

#lowess

plot(nas~tempo, data = dados, col = 'gray50', pch = '.', xlab = 'Tempo(Horas)',
      ylab='Nivel a ̧cucar no san')

with (dados, {
  lines (loess.smooth(tempo, nas, family = 'gaussian'), lty = 3, col = 3, lwd = 2)
  lines (loess.smooth(tempo, nas, family = 'gaussian', degree = 2), lty = 3, col = 2, lwd = 1)
})

#splines

plot (nas~tempo, data = dados, col ='gray50', pch = '.', xlab = 'Tempo(Horas)',
      ylab='Nivel a ̧cucar no sangu')
with (dados, {
  lines (smooth.spline (tempo, nas), lty = 4, col = 4, lwd = 2)
  lines (smooth.spline (tempo, nas, df=2), lty = 4, col = 2, lwd = 1)
})

#todos

plot (nas~tempo, data = dados, col ='gray50', pch = '.', xlab = 'Tempo(Horas)',
      ylab='Nivel a ̧cucar no sangu')
with (dados, {
  lines(ksmooth(tempo, nas, 'normal', bandwidth = 1), col = 2)
  lines(ksmooth(tempo, nas, 'normal', bandwidth = 0.25), col = 3)
  lines(ksmooth(tempo, nas, 'normal', bandwidth = 0.5), col = 4)
  lines (loess.smooth(tempo, nas, family = 'gaussian'), lty = 3, col = 3, lwd = 2)
  lines (loess.smooth(tempo, nas, family = 'gaussian', degree = 2), lty = 3, col = 2, lwd = 1)
  lines (smooth.spline (tempo, nas), lty = 4, col = 4, lwd = 2)
  lines (smooth.spline (tempo, nas, df=2), lty = 4, col = 2, lwd = 1)
})

#d)

library(lattice)
#dados$grupo <- as.factor(dados$grupo)
xyplot(nas~tempo+grupo, data = dados,type = 'l', group = id, xlab = 'Tempo(horas)',col.line
        = 'gray20', ylab='nivel de açucar',strip = strip.custom (var.name = 'grupo'))



group.subset1= subset(dados,dados$grupo== 1)
mean.group1=tapply(group.subset1$nas,group.subset1$tempo, mean)
mean.group1
group.subset2= subset(dados,dados$grupo== 2)

mean.group2=tapply(group.subset2$nas,group.subset2$tempo, mean)
mean.group2
group.subset3= subset(dados,dados$grupo== 3)
mean.group3=tapply(group.subset3$nas,group.subset3$tempo, mean)
mean.group3

plot(dados$tempo,dados$nas, type = 'p', col='white', ylab='nivel medio de acucar no sangue',
     xlab='Horas', main='Perfil m ́edio por grup')

lines(mean.group1, col='red', lwd=2, lty=1)
lines(mean.group2, col='black', lwd=2, lty=3)
lines(mean.group3, col='blue', lwd=3, lty=2)
legend(2,50,c('grupo 1', 'grupo 2', 'grupo 3'), lty=c(3,1), col=c( 'black', 'red', 'blue')
)

# e)

source('variog.txt')
mean.nas=tapply(dados$nas,dados$tempo, mean)
residuo=dados$nas- rep(mean.nas, 36)
vargm = variog(indv = dados$id, time = dados$tempo,Y = residuo)
