### Análise Multivariada
### Programa de Pós graduação em Sociologia
## Prof. Jorge Alexandre Neves
## Monitoria: Jonatas Varella
#################################

#### AULA 4 - 19/09/2019

## Carregando os pacotes
library(haven)
library(ggplot2)
library(stargazer)

## Lendo o banco de dados
pnad96 = read_sav("https://github.com/jonatasvarella/analisemultivariada/blob/master/PNAD96_25a60_PPGS_2019%20(1).sav?raw=true")

## Modelo I

reg = lm(lnrenda ~ anosesco + raçabin, data = pnad96)
summary(reg)
coef(reg)

#Plotando as curvas de regressão para cada categoria de raça
plot(pnad96$anosesco, pnad96$lnrenda)
abline(coef(reg)[1],coef(reg)[2], lwd=2, col="red")
abline(coef(reg)[1]+coef(reg)[3],coef(reg)[2], lwd=2, col="blue")


## Modelo II - Utilizando termo interativo

reg2 = lm(lnrenda ~ anosesco + raçabin + raçaesco, data = pnad96)
summary(reg2)

## Modelo III - Variável região

##Recodificando as labels. 

pnad96$região = as.factor(pnad96$região)
pnad96$região = factor(pnad96$região, levels = c("1", "2", "3", "4", "5"),
                       labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))

# Definindo a categoria de referência

pnad96$região <- relevel(pnad96$região, ref="Nordeste")

# Montando o modelo de regressão

reg3 = lm(lnrenda~anosesco + região, data=pnad96)



