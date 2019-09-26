### Análise Multivariada
### Programa de Pós graduação em Sociologia
## Prof. Jorge Alexandre Neves
## Monitoria: Jonatas Varella
#################################

#### AULA 5 - 27/09/2019

## Carregando os pacotes
library(haven)
library(ggplot2)
library(stargazer)
library(dplyr)
library(car)
library(xtable)

## Lendo o banco de dados
pnad96 = read_sav("https://github.com/jonatasvarella/analisemultivariada/blob/master/PNAD96_25a60_PPGS_2019%20(1).sav?raw=true")

## Investigando a variável idadecen

summary(pnad96$idadecen)

## Transformando idadecen em idade

pnad$idade = pnad$idadecen + (-min(pnad$idadecen)) + 25

## Calculando a média logaritmo da renda por idade

medias_renda = sapply(25:60, function(x) mean(pnad96$lnrenda[pnad96$idade == x]))

## Plotando o gráfico

ggplot(NULL, aes(y = medias_renda, x = 25:60)) +
  geom_line() + 
  ylab(label = "Média do logaritmo natural do rendimento do trabalho principal") +
  xlab(label = "Idade")

## Modelo I

## Criando a variável idade ao quadrado

pnad96$idade2 = pnad96$idade^2
summary(pnad96$idade2)

## Modelo de regressão polinomial
reg = lm(lnrenda ~ idade + idade2, data=pnad96)
summary(reg)
vif(reg)  #verifica colinearidade

## Modelo II - utilizando a idade centralizada

reg2 = lm(lnrenda~ idadecen + idadecen2 , data=pnad96)
summary(reg2)
vif(reg2) 

## Regressão Logística 

## Modelo III 
reg3 = glm(segmento ~ anosesco, data = pnad96, family = "binomial")

## Modelo IV
reg4 = glm(segmento ~ anosesco + iseipai, data = pnad96, family = "binomial")

## Modelo V
reg5 = glm(segmento ~ anosesco + iseipai + raçabin, data = pnad96, family = "binomial")

## Analisando os modelos

stargazer(reg3, reg4, reg5, type = "html", out = "reglog.html")
anova(reg3, reg4, reg5, test = "Chisq")

library(DescTools)
cbind(`Modelo III` = PseudoR2(reg3, c("CoxSnell", "Nagelkerke")),
      `Modelo IV`  = PseudoR2(reg4, c("CoxSnell", "Nagelkerke")),
      `Modelo V`   =  PseudoR2(reg5, c("CoxSnell", "Nagelkerke")))



