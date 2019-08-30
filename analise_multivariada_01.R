### Análise Multivariada
### Programa de Pós graduação em Sociologia
## Prof. Jorge Alexandre Neves
## Monitoria: Jonatas Varella
#################################

#### AULA 1 - 30/08/2019

# Montando um modelinho de regressão hipotético...
# Vamos ver se dá pra confiar na estimação

X = rnorm(100)      #gera dados aleatórios
e = rnorm(100)      #gera dados aleatórios

Y = 2 + 1.75*X + e  # Montamos o modelo com os coeficientes reais
hist(Y)             #plota um histograma para verificar a distribuição de Y

reg = lm(Y~X)       #monta o modelo
summary(reg)        #exibe os resultados do modelo
coef(reg)           #exibe só os coeficientes
confint(reg)        #exibe o intervalo de confiança

#### Funcionou?

# plotando a curva de regressão
plot(X,Y, pch = 19)  #plota o diagrama de dispersão
abline(reg = reg, lwd = 2, col = "red")  #plota a reta

########################################################


# Carregando os pacotes necessários
library(haven)
library(ggplot2)

# Lê o banco de dados
#pnad = read_sav("PNAD2014_30a50_novo4.sav")

pnad = read_sav("https://github.com/jonatasvarella/analisemultivariada/blob/master/PNAD2014_30a50_novo4.sav?raw=true")


names(pnad)  #nomes das variáveis
head(pnad)   #primeiros casos
str(pnad)    #estrutura do objeto
View(pnad)   #visualiza o banco de dados

# plotando a dispersão do log da renda
ggplot(pnad, aes(x=lnrenda))+geom_histogram()


############
# Regressão

# analisando a variável dependente
summary(pnad$anosesco)
sd(pnad$anosesco)



# Montando a regressão
reg = lm(anosesco~escmãe, data=pnad)

#Anova e resultados
anova(reg)
summary(reg)
confint(reg)   #intervalo de confiança a 95%
confint(reg, level = .99)  #intervalo de confiança a 99%

plot(pnad$escmãe, pnad$anosesco)
abline(reg=reg, lwd=2, col="red")

ggplot(pnad, aes(x=escmãe, y=anosesco))+
  geom_point()+stat_smooth(method = "lm")
mean(pnad$anosesco)

# gráficos de avaliação
par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))
