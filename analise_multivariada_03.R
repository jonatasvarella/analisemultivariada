### Análise Multivariada
### Programa de Pós graduação em Sociologia
## Prof. Jorge Alexandre Neves
## Monitoria: Jonatas Varella
#################################

#### AULA 3 - 13/09/2019

# Carregando os pacotes necessários
library(haven)
library(ggplot2)
library(car)
library(stargazer)
# Lê o banco de dados
#pnad = read_sav("PNAD2014_30a50_novo4.sav")

pnad = read_sav("https://github.com/jonatasvarella/analisemultivariada/blob/master/PNAD2014_30a50_novo4.sav?raw=true")


# Modelo I
reg = lm(lnrenda ~ anosesco, data=pnad)

#Analisando resultados e Anova
summary(reg)
anova(reg)
vif(reg)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

# Modelo II
reg2 = lm(lnrenda ~ anosesco + isei88pai, data=pnad)

#Analisando resultados e Anova
summary(reg2)
anova(reg2)
vif(reg2)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg2)
par(mfrow=c(1,1))

# Modelo III
reg3 = lm(lnrenda ~ anosesco + isei88pai + isei88, data=pnad)

#Analisando resultados e Anova
summary(reg3)
anova(reg3)
vif(reg3)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg3)
par(mfrow=c(1,1))

## Modelo IV
## Padronizando as variáveis

pnad_pad = pnad

pnad_pad$lnrenda_pad = (pnad_pad$lnrenda - mean(pnad_pad$lnrenda)) / sd(pnad_pad$lnrenda)
pnad_pad$anosesco_pad = (pnad_pad$anosesco - mean(pnad_pad$anosesco)) / sd(pnad_pad$anosesco)
pnad_pad$isei88pai_pad = (pnad_pad$isei88pai - mean(pnad_pad$isei88pai)) / sd(pnad_pad$isei88pai)
pnad_pad$isei88_pad = (pnad_pad$isei88 - mean(pnad_pad$isei88)) / sd(pnad_pad$isei88)

## Modelo padronizado

reg4 = lm(lnrenda_pad ~ anosesco_pad + isei88pai_pad + isei88_pad, data = pnad_pad)

summary(reg4)
anova(reg4)
vif(reg4)

## Exibindo os modelos
stargazer(reg, reg2, reg3,reg4,  type="html", out="tabela.html")

## Guardando os resultados
resultados_reg = summary(reg)
resultados_reg2 = summary(reg2)
resultados_reg3 = summary(reg3)
resultados_reg4 = summary(reg4)

# Colocando os R2 lado a lado para comparar
cbind(resultados_reg$r.squared, resultados_reg2$r.squared, resultados_reg3$r.squared, resultados_reg4$r.squared)

#Exibindo os R2 de outro jeito
cat("R2 do modelo I: "); resultados_reg$r.squared
cat("R2 do modelo II: "); resultados_reg2$r.squared
cat("R2 do modelo III: "); resultados_reg3$r.squared
cat("R2 do modelo IV: "); resultados_reg4$r.squared


