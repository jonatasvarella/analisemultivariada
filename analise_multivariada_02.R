### Análise Multivariada
### Programa de Pós graduação em Sociologia
## Prof. Jorge Alexandre Neves
## Monitoria: Jonatas Varella
#################################

#### AULA 2 - 06/09/2019

# Carregando os pacotes necessários
library(haven)
library(ggplot2)

# Lê o banco de dados
#pnad = read_sav("PNAD2014_30a50_novo4.sav")

pnad = read_sav("https://github.com/jonatasvarella/analisemultivariada/blob/master/PNAD2014_30a50_novo4.sav?raw=true")


#Pedindo média de x e y

summary(pnad$isei88)      #média de y
summary(pnad$isei88pai)   #média de x

# Refazendo a regressão
reg = lm(isei88 ~ isei88pai, data=pnad)

#Analisando resultados e Anova
summary(reg)
anova(reg)


# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

#### Modelo II #####

#Pedindo média de x e y

summary(pnad$renda)      #média de y
summary(pnad$isei88)   #média de x

# Refazendo a regressão
reg2 = lm(renda ~ isei88, data=pnad)

#Analisando resultados e Anova
summary(reg2)
anova(reg2)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg2)
par(mfrow=c(1,1))

#### Modelo III #####

#Pedindo média de x e y

summary(pnad$lnrenda)      #média de y
summary(pnad$isei88)   #média de x

# Refazendo a regressão
reg3 = lm( lnrenda ~ isei88, data=pnad)

#Analisando resultados e Anova
summary(reg3)
anova(reg3)

coef(reg3)[1]        #Extraindo o primeiro coeficiente da reg
exp(coef(reg3)[1])   #calcula o exponencial do 1 coef

coef(reg3)[2]       #Extraindo o segundo coeficiente (da regressao)

# taxa simple = b1 * 100
coef(reg3)[2] * 100
#taxa composta = ( exp(b1)-1 ) * 100
(exp(coef(reg3)[2]) - 1) * 100


# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg3)
par(mfrow=c(1,1))

#### Modelo IV #####

## Criando o logaritmo da variável isei88

pnad$lnisei88 = log(pnad$isei88)

#Pedindo média de x e y

summary(pnad$lnrenda)      #média de y
summary(pnad$lnisei88)   #média de x

# Refazendo a regressão
reg3= lm(lnrenda ~ lnisei88, data=pnad)

#Analisando resultados e Anova
summary(reg3)
anova(reg3)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg3)
par(mfrow=c(1,1))

#### Modelo V #####

## Padronizando as variáveis

pnad_pad = data.frame(Y = pnad$isei88, X = pnad$anosesco)

pnad_pad$Y = (pnad_pad$Y - mean(pnad_pad$Y)) / sd(pnad_pad$Y)
pnad_pad$X = (pnad_pad$X - mean(pnad_pad$X)) / sd(pnad_pad$X)

## Fazendo a regressão

reg_pad = lm(Y ~ X, data = pnad_pad)
summary(reg_pad)
anova(reg_pad)

plot(reg_pad)

