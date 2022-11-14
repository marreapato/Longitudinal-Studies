library(tidyverse)
library(lmtest)
library(lme4)
library(nlme)
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

mod2 = lmer(nas ~ tempo + (1|id), data = dados)
summary(mod2)
icc(mod2)
anova(mod2)

#c) checar




(teststat <- -2*(logLik(fit, REML=FALSE)-logLik(mod2, REML=FALSE)))
?logLik

(p.val <- pchisq(teststat, df = 1, lower.tail = FALSE))
#d)


pred.mod2 = fitted(mod2)

datapred.mod2 = unique(data.frame(cbind(pred.mod2 = pred.mod2, tempo = dados$tempo,
                                        id = dados$id)))

xyplot(pred.mod2 ~ tempo, data = datapred.mod2, groups = id, type = c("p", "l"), col =
         "blue")


#e)
mod3 = lmer(nas~tempo + (tempo|id), data = dados)
summary(mod3)


#f)


pred.mod2 = fitted(mod3)

datapred.mod2 = unique(data.frame(cbind(pred.mod2 = pred.mod2, tempo = dados$tempo,
                                        id = dados$id)))

xyplot(pred.mod2 ~ tempo, data = datapred.mod2, groups = id, type = c("p", "l"), col =
         "blue")


#g)

anova(mod2, mod3)

(teststat <- -2*(logLik(mod2, REML=FALSE)-logLik(mod3, REML=FALSE)))
?logLik

(p.val <- pchisq(teststat, df = 1, lower.tail = FALSE))


#h)

hist(resid(mod2))
hist(resid(mod3))

shapiro.test(resid(mod2))
shapiro.test(resid(mod3))

#i)


mod3 = lmer(nas~tempo + (tempo|id)+grupo+grupo*tempo, data = dados)
summary(mod3)

anova(mod3)
