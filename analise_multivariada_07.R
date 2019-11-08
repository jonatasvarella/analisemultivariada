### Análise Multivariada
### Programa de Pós graduação em Sociologia
## Prof. Jorge Alexandre Neves
## Monitoria: Jonatas Varella
#################################

#### AULA 7 - 17/10/2019

## Carregando os pacotes
library(haven)
library(ggplot2)
library(stargazer)
library(dplyr)
library(car)
library(xtable)

## EQUAÇÕES ESTRUTURAIS (SEM) ##

pnad = read_sav("https://github.com/jonatasvarella/analisemultivariada/blob/master/PNAD2014_30a50_novo4.sav?raw=true")
pnad = read_dta("https://github.com/jonatasvarella/analisemultivariada/blob/master/Pnad_2014.dta?raw=true")
## Pacotes
##install.packages("lavaan")
##install.packages("qgraph")
#devtools::install_github("rich-iannone/DiagrammeR")

library(lavaan)
library(qgraph)
library(stringr)
library(lavaan)
library(DiagrammeR)
library(dplyr)
library(semPlot)
library(psych)
## Modelo I

model <- '
  # measurement model
    ose =~ escmãe + escpai + iseopai
    dse =~ lnrenda + iseo 
  # regressions
    
    dse ~ ose + esco 
   esco ~ ose
  # residual correlations
    escmãe ~~ escpai
    escmãe ~~ iseopai
    escpai ~~ iseopai
'
fit <- sem(model, data=pnad)
summary(fit, standardized=TRUE)

# Plot

semPaths(fit, intercept = FALSE, whatLabel = "est",
         residuals = FALSE, exoCov = FALSE)

#pnad$dse = fa(pnad[2,6], nfactors = 1, rotate = "oblimin", covar = T, fm="minres")


