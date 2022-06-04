
rm(list=ls())


require(ggplot2)
require(ggthemes)
library(readr)
library(tidyverse)
library(TTR)
library(forecast)
library(lubridate)
library(janitor)
library(dplyr)
library(TTR)
library(forecast)
library(timeSeries)
require(Kendall)
library(fpp2)
require(tseries)
require(Hmisc)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)
#library(writexl) #exportar o banco de dados limpo


#Importação e visualização dos dados 

ETH_USD <- read_csv("Econometria/ETH-USD.csv")
#View(ETH_USD)
#View(head(ETH_USD))
summary(ETH_USD)

#limpeza dos dados

ETH_USD <- clean_names(ETH_USD)
summary(ETH_USD)

#remoção de linhas e colunas vazias

remove_empty(ETH_USD)

#verificando se há valores duplicados

get_dupes(ETH_USD)

#convertendo as colunas para o tipo certo de dados

ETH_USD$open = as.numeric(ETH_USD$open)
ETH_USD$high = as.numeric(ETH_USD$high)
ETH_USD$low = as.numeric(ETH_USD$low)
ETH_USD$close = as.numeric(ETH_USD$close)
ETH_USD$adj_close = as.numeric(ETH_USD$adj_close)
ETH_USD$volume = as.numeric(ETH_USD$volume)
ETH_USD$date = as.Date(ETH_USD$date)
summary(ETH_USD)

#remoção dos NA

ETH_USD = na.omit(ETH_USD)
summary(ETH_USD)

#analise exploratória 

head(ETH_USD)
str(ETH_USD)
glimpse(ETH_USD)
View(ETH_USD)

# valores de maximo e minimo que o Ethereum alcançou em um dia e volume de transações

ETH_USD %>% 
  summarise(
    high = max(high),
    low = min(low),
    max_volume = max(volume),
    min_volume = min(volume),
    adj_close= mean(adj_close)
  )

#transformação dos dados para visualização 

eth_meses = ETH_USD %>% 
  
  group_by(meses = floor_date(date,"month")) %>% 
             summarise(high = max(high),
                      low = min(low),
                      volume_max = max(volume),
                      volume_min = min(volume), 
                      adj_close= mean(adj_close))%>% 
  mutate(percentual_mudanca = (high - lag(high)) / lag(high) * 100)

glimpse(eth_meses)
summary(eth_meses)
View(eth_meses)

head(eth_meses)

#grafico

eth_meses %>% 
  ggplot()+
  geom_area(aes(x = meses, y = high), fill = "#be96b1", color = "#331228")+
  labs(x = "Anos", y = "Variações de preço")

#percentual de mudança por mês

eth_meses %>% 
  ggplot()+
  geom_col(aes(x = meses, y = percentual_mudanca, fill = percentual_mudanca), color = "black")+
  labs(x = "Meses", y = "Percentual de Variação")+
  scale_fill_gradient2(low = "#e53030", midpoint = 0, high = "#0aaf52")+
  theme(legend.position = "none")

#nesta analise vamos verificar se o preço de alta e baixa possui relação com o volume maximo de 
#transaçoes do Ethereum por mês


#variavel dependente: volume
#variavel independente: high e low

#mod <- lm(volume ~ low + high, ETH_USD)

#trabalhando com os dados mensais 

#mod1 <- lm(volume_max ~ low + high, eth_meses)

#analise gráfica

#par(mfrow=c(2,2))
#plot(mod1)
#par(mfrow=c(1,1))
#anova(mod1)

#aplicação dos testes

## Normalidade dos resíduos:
#shapiro.test(mod1$residuals)


## Outliers nos resíduos:
#summary(rstandard(mod1))


## Independência dos resíduos (Durbin-Watson):
#durbinWatsonTest(mod1)


## Homocedasticidade (Breusch-Pagan):
#bptest(mod1)


## Ausência de Multicolinearidade

#ETH_USDxx<-cbind(ETH_USD[3],ETH_USD[4],ETH_USD[7] ) 
#head(ETH_USDxx)
#pairs.panels(ETH_USDxx)

#eth_mesesxx<-cbind(eth_meses[3],eth_meses[4],eth_meses[2] ) 
#head(eth_mesesxx)
#pairs.panels(eth_mesesxx)

#pairs.panels(eth_meses)

### Multicolinearidade: r > 0.9 (ou 0.8)

#vif(mod1)

### Multicolinearidade: VIF > 10


###### Criação de um segundo modelo
## verificar se o volume de transações é influenciado pelo valor de fechamento mensal

mod2 <- lm( volume_max~adj_close,eth_meses)

par(mfrow=c(2,2))

plot(mod2)

par(mfrow=c(1,1))

anova(mod2)


###########################################

## Normalidade dos resíduos:
shapiro.test(mod2$residuals)

anova(mod2)

## Outliers nos resíduos:
summary(rstandard(mod2))


## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(mod2)


## Homocedasticidade (Breusch-Pagan):
bptest(mod2)


# Passo 4: Análise do modelo
summary(mod2)

ggplot(data = eth_meses, mapping = aes(x = adj_close, y = volume_max)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\",\")~~")),
                        label.x = 1500, label.y = 2800) +
  theme_classic()


##########################################


#para mod 2 


#transformando dos dados em serie temporal para verificar a variação do Ethereum ao 
#longo do tempo

#transformando os dados de fechamento em série temporal

ETH_USD2 <- ETH_USD[1609:2311, c(1,2,3,4,5,6,7)]
head(ETH_USD2)
eth_serie <- ts(ETH_USD2[,6], start=2020, freq=365)

#linha de tendência

N <- nrow(eth_serie)
Tempo <- 1:N
X.lm <- lm(eth_serie ~ Tempo)
X.pred <- predict(X.lm) ## Previsão da série observada
M <- seq(as.Date("2020/1/1"),as.Date("2021/12/3"),"days")
plot(M, eth_serie, type="l", col="darkblue", xlab = "Anos",ylab = "Valores", main = "Gráfico 7: Variação do Ethereum 
     ao longo dos anos")
lines(M, X.pred, col= "red", lwd=2,lty=2)

# aplicando modelo multiplicativo para decompor a serie

ETH_USD_serie2<- ts(ETH_USD[,6],freq=365) 
eth_adt <- decompose(ETH_USD_serie2, type = "mult")
plot(eth_adt, col= "darkblue")

# utilizando a Suavização Exponencial de Holt Winters
#pois a serie possui tendencia de sazonalidade aditiva

modelo_eth <- HoltWinters(ETH_USD_serie2)


#valores estimados da serie

modelo_eth$fitted

#gráfico 

plot(ETH_USD_serie2)
lines(modelo_eth$fitted[,1],pch = 18, col = "red", type = "l", 
      lty = 2, lwd = 1)

#########################################################################

#serie temporal por meses 

eth_meses_ts<- ts(eth_meses[,2], start = 2015, freq= 12)
plot(eth_meses_ts)

N <- nrow(eth_meses_ts)
Tempo <- 1:N
X.lm <- lm(eth_meses_ts ~ Tempo)
X.pred <- predict(X.lm) ## Previsão da série observada
M <- seq(as.Date("2015/8/1"),as.Date("2021/12/1"),"month")
plot(M, eth_meses_ts, type="l",lwd=1, col="darkblue", xlab = "Anos",ylab = "Valores")
lines(M, X.pred, col= "red", lwd=2,lty=1)
grid(nx = NULL, ny = NULL, 
     lty = 3, 
     lwd = 1, 
     col = "gray")

#decomposição da serie

eth_adt <- decompose(eth_meses_ts)
plot(eth_adt, col= "#422a60", lwd=1)
grid(nx = NULL, ny = NULL, 
     lty = 2, 
     lwd = 1, 
     col = "gray")

##############################################################################
# ver se a serie é estacionária

seasonplot(eth_meses_ts,col=rainbow(7),year.labels = TRUE,
           las=2)
grid(nx = NULL, ny = NULL, 
     lty = 2, 
     lwd = 1, 
     col = "gray")

#Teste de Mann-Kendall para verificar tendência na série temporal, sob a
#hipótese:

#H0:"As observações da série são independentes e identicamente distribuídas"
#H1:"As observações da série possuem tendência monotônica no tempo"

MannKendall(eth_meses_ts)

# teste de não estacionaridade

adf.test(eth_meses_ts)


#diferenciação 

ethdif = diff(eth_meses_ts)

plot(ethdif)

ndiffs(ethdif)

# analise grafica de autocorrelaçao

par(mfrow=c(2,2),mar=c(4,4,4,1)+.1)
plot(eth_meses_ts,ylab="Ethereum")
acf(eth_meses_ts,main="Série não-estacionária")
plot(ethdif,ylab="Ethereum")
acf(ethdif,main="Série Estacionária")

par(mfrow=c(1,1))


# decomposição 

autoplot(decompose(ethdif,"additive")) + labs(title = "Descomposição da série
temporal", subtitle = "(dados) (Tendência) (Sazonalidade) (Resíduo)")

autoplot(decompose(eth_meses_ts,"additive")) + labs(title = "Descomposição da série
temporal", subtitle = "(dados) (Tendência) (Sazonalidade) (Resíduo)")

# Teste para Sazonalidade 
fit1 <- ets(ethdif)
fit2 <- ets(ethdif,model="ANN")

deviance <- 2*c(logLik(fit1) - logLik(fit2));deviance
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df ;df
#P value
1-pchisq(deviance,df)
plot(forecast(fit1))

#H0:"Existe um componente Sazonal na Série"
#H1:"Não Existe um componente Sazonal na Série"

ggseasonplot(eth_meses_ts, polar = T)

modelo1 = auto.arima(eth_meses_ts, seasonal = FALSE, stationary = FALSE)

summary(modelo1)

checkresiduals(modelo1)





########################



Meses = c("J","F","M","A","M","J","J","A","S","O","N","D")

# Gráficos de Series de Tiempo
plot.ts(eth_meses_ts, xlab="Anos",ylab="Índice de valores",axes=FALSE)

axis(1,c(2015,2016,2017, 2018, 2019,2020,2021),las=3)
axis(2,c(0,500,1000,1500,2000,2500,3000,3500,4000, 4500, 5000))

points(eth_meses_ts,cex=0.8, font=4, col=1:4)

################################################################################

#media móvel por trimestre

eth_M3<- SMA(eth_meses_ts, n=3)

plot(eth_meses_ts, col= "#493267")
points(eth_M3, pch=20, col= "#4aa873")
grid(nx = NULL, ny = NULL, 
     lty = 3, 
     lwd = 1, 
     col = "gray")

#############################################################3

# utilizando a Suavização Exponencial de Holt Winters
#pois a serie possui tendencia de sazonalidade aditiva

modelo_ethM <- HoltWinters(eth_meses_ts)
a<- forecast(modelo_ethM, h=24)
plot(a)

#valores estimados da serie

head(modelo_ethM$fitted)

#gráfico 

plot(eth_meses_ts, col= "#351d74")
lines(modelo_ethM$fitted[,1],pch = 18, col = "red", type = "b", 
      lty = 2, lwd = 1)
grid(nx = 11, ny = 8, 
     lty = 2, 
     lwd = 1, 
     col = "gray")

#previsão
a<- forecast(modelo_ethM, h=12)
plot(a, col= "#520c2e")
grid(nx = 11, ny = 8, 
     lty = 2, 
     lwd = 1, 
     col = "gray")


###################################################################################
###################################################################################
#modelo de médias móveis 

modelo1 = auto.arima(eth_meses_ts, seasonal = FALSE, stationary = FALSE)

summary(modelo1)

checkresiduals(modelo1)

Meses = c("J","F","M","A","M","J","J","A","S","O","N","D")

# Gráficos de Series de Tiempo
plot.ts(eth_meses_ts, xlab="Anos",ylab="Índice de preços do consumidor",axes=FALSE)

axis(1,c(2015,2016,2017, 2018, 2019,2020,2021,2022),las=2)
axis(2,c(0,500,1000,2000,3000,4000,5000))

points(eth_meses_ts, pch=Meses, cex=0.8, font=4, col=1:4)

bb<- forecast(modelo1, h=14)
plot(bb)

###############################################################################
