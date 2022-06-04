
#Carregando os pacotes necess�rios para an�lise
library(fpp2)
require(forecast)
require(tseries)
require(Kendall)
library(lmtest)
library(readxl)
library(openxlsx)
library(timeSeries)
library(ggplot2)


################################################################
################################################################
### Indice de pre�os do Consumidor 1980-2021 ###################
################################################################
#O �ndice de Pre�os ao Consumidor (IPC) mede a varia��o de pre�os 
# de um conjunto fixo de bens e servi�os componentes de despesas 
# habituais de fam�lias com n�vel de renda situado entre 1 e 33 
# sal�rios m�nimos mensais.
################################################################




dados <-  read_excel("Econometria/STP_IPCA.xlsx")
#dados<- read_excel(path = dadosxlsx, sheet = 1)
#dados
IPCA <- as.data.frame(dados)
IPCA
head(IPCA,2)



#######################################################################
# Generaremos nuestros dados desde la primera observaci�n obtenida 
# hasta la �ltima, esto tambi�n lo haremos de manera mensal como 
# se observa a continuaci�n
######################################################################

summary(IPCA$Indice)

####################################################################
IPCA$Indice  = as.numeric(IPCA$Indice)

####################################################################

Ipca.ts <- ts(IPCA$Indice, start = c(1980, 1), end = c(2021, 12), 
                           frequency = 12)

class(Ipca.ts)
# Los valores de la serie quedar�n definidos en el objeto creado de nombre 
# Ipca.ts:
print(Ipca.ts)

ndiffs(Ipca.ts)


####################################################################
#Se podr� observar que la serie creada, tal y como se indic� en la 
# sintaxis de dicha funci�n, empieza en el primer mes de 1980 y 
# termina en el diciembre de 2021.
####################################################################
#########################################################
# Gr�ficos de Series de Tiempo
plot.ts(Ipca.ts, xlab="Anos",ylab="�ndice de pre�os do consumidor",axes=FALSE)

axis(1,c(1980,1984,1988,1992,1996,2000,2004,2008,2012,
         2016,2020,2022),las=2)
axis(2,c(-0.6,15,30,45,60))

#lines(lowess(time(Ipca.ts),Ipca.ts),lwd=1,col="black")

# Sazonalidade
ggmonthplot(Ipca.ts)


##############################################################################
# diagrama de caja y bigotes
boxplot(Ipca.ts ~ cycle(Ipca.ts), ylab="Indice de pre�os do consumidor",
        col="2",xlab="Meses",las=2)


# Adem�s, en este ejemplo, podr�as agregar puntos a cada 
# diagrama de caja de la siguiente manera

stripchart(Ipca.ts ~ cycle(Ipca.ts), vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, las=2,col = 1:length(levels(Ipca.ts)))


#################################################################
#Para modelo aditivo

modeloaditivo=decompose(Ipca.ts)
plot(modeloaditivo)

# para el modelo multiplicativo
modelomultiplicativo=decompose(Ipca.ts,type = "mult")
plot(modelomultiplicativo)

# Para estimar la tendencia
Tendencia=modelomultiplicativo$trend
print(Tendencia)

#Para estimar la estacionariedad
Estacionariedad=modelomultiplicativo$seasonal
print(Estacionariedad)


ts.plot(cbind(Tendencia,Tendencia*Estacionariedad),
        lty=1:2)
################################################################


# �Es estacionaria la serie de tiempo?


################################################################
seasonplot(Ipca.ts,col=rainbow(12),year.labels = TRUE,
           las=2)

###############################################################

#Teste de Mann-Kendall para verificar tend�ncia na s�rie temporal, sob a
#hip�tese:

#H0:"As observa��es da s�rie s�o independentes e identicamente distribu�das"
#H1:"As observa��es da s�rie possuem tend�ncia monot�nica no tempo"

MannKendall(Ipca.ts)

#Testes para n�o-estacionariedade: Dickey-Fuller

adf.test(Ipca.ts)
#Pelo teste de Dickey-Fuller, rejeita-se a hip�tese nula
#de n�o-estacionariedade, ou seja, a s�rie � estacion�ria.....

###############################################################
# Crear el primer diferencial de la serie original para que 
# as� puedamos trabajar con el pron�stico a futuro y 
# sin ningun problema de ra�z unitaria
###############################################################

# Usar diferenciaci�n (Primera Difencia del Neumon�a)

Ipcadif = diff(Ipca.ts)

plot(Ipcadif)

ndiffs(Ipcadif)




#################################################################
# Analisis visual de las gr�ficas de Autocorrelaci�n
par(mfrow=c(2,2),mar=c(4,4,4,1)+.1)
plot(Ipca.ts,ylab="Ipca")
acf(Ipca.ts,main="S�rie n�o-estacion�ria")
plot(Ipcadif,ylab="Ipca")
acf(Ipcadif,main="S�rie Estacion�ria")

################################################################


autoplot(Ipcadif, color = "darkgoldenrod4", size = 0.7) + 
    labs(title = "Primeira diferen�a do Ipca (1980 - 2021)", 
         subtitle = "Ipca", 
         caption = "Olinda, R. A. (2022)") +
    xlab("Anos") + ylab("Diferencial")


################################################################
# Observamos que la serie ahora s� se ha vuelto 
# estacionaria y estacional al generar su primer diferencial. 
# Debemos de comprobarlo a trav�s de las pruebas de Dickey-Fuller 
# aumentada y Phillips-Perron para comprobar que realmente lo sea.
#################################################################

# Revisi�n de estacionariedad

adf.test(Ipcadif, alternative = "stationary")

pp.test(Ipcadif)

##################################################################
# En ambos casos observamos que el valor p de Pearson es menor 
# que el valor cr�tico de 0.05, por lo que podemos concluir que 
# rechazamos la Hip�tesis nula. En otras palabras, la serie es 
# estacionaria y con estacionalidad y no existe una ra�z unitaria 
# que afecte a nuestro modelo. El tiempo no influye dentro de 
# nuestra serie de tiempo.
##################################################################

#La autocorrelaci�n e la variable y se almacenan en el vector 
#acf(y)$asf, con el rezago k localizado en acf(x)$acf[k+1]. 
#Por ejemplo, de acuerdo a la base de dados de la Neumin�a
# anteriormente, la autocorrelaci�n el rezago 1 para gnp.ts es:

autoplot(acf(Ipcadif), color = "deepskyblue", size = 1.5) + 
    labs(title = "Autocorrela��o",
         subtitle = "95% de confian�a") +
    xlab("Passos") +
    ylab("Autocorrela��o")


autoplot(pacf(Ipcadif), color = "deepskyblue", size = 1.5) +
    labs(title = "Autocorrela��o Parcial",
         subtitle = "95% de confian�a") +
    xlab("Passos") +
    ylab("Autocorrela��o Parcial")


# Descomposici�n de la serie de tiempo

################################################################
# Ahora descompondremos la serie para obtener sus componentes. 
# Como lo es: 
# La serie origninal: Tend�ncia * Sazonalidade * Res�duo
#################################################################

autoplot(decompose(Ipcadif,"additive")) + labs(title = "Descomposi��o da s�rie
temporal", subtitle = "(dados) (Tend�ncia) (Sazonalidade) (Res�duo)")



# Teste para Sazonalidade 
fit1 <- ets(Ipcadif)
fit2 <- ets(Ipcadif,model="ANN")

#"ANN" is simple exponential smoothing with additive errors

deviance <- 2*c(logLik(fit1) - logLik(fit2));deviance
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df ;df
#P value
1-pchisq(deviance,df)
plot(forecast(fit1))

#H0:"Existe um componente Sazonal na S�rie"
#H1:"N�o Existe um componente Sazonal na S�rie"
#################################################################

ggseasonplot(Ipca.ts, polar = T)




modelo1 = auto.arima(Ipca.ts, seasonal = FALSE, stationary = FALSE)

summary(modelo1)

checkresiduals(modelo1)

##################################################################

##################################################################

Meses = c("J","F","M","A","M","J","J","A","S","O","N","D")

# Gr�ficos de Series de Tiempo
plot.ts(Ipca.ts, xlab="Anos",ylab="�ndice de pre�os do consumidor",axes=FALSE)

axis(1,c(1980,1984,1988,1992,1996,2000,2004,2008,2012,
         2016,2020,2022),las=2)
axis(2,c(-0.6,15,30,45,60,85,90))

points(Ipca.ts, pch=Meses, cex=0.8, font=4, col=1:4)


#nsdiffs(Ipca.ts)

####################################################################
#ic="aicc",include.mean=TRUE


modelo2 = auto.arima(Ipca.ts,stepwise = FALSE, approximation = FALSE)

summary(modelo2)

res=resid(modelo2)
plot(res)

adf.test(res)

checkresiduals(modelo2)

#Otra opcion para hacer SARIMA

mod1_Ipca <- Arima(Ipca.ts,order=c(2,1,3),seasonal=list(order=c(1,0,0),period=12))
mod1_Ipca

mod2_Ipca <- Arima(Ipca.ts,order=c(2,1,2),seasonal=list(order=c(1,0,0),period=12))
mod2_Ipca

mod3_Ipca <- Arima(Ipca.ts,order=c(2,1,1),seasonal=list(order=c(1,0,0),period=12))
mod3_Ipca

mod4_Ipca <- Arima(Ipca.ts,order=c(1,1,2),seasonal=list(order=c(1,0,0),period=12))
mod4_Ipca

mod5_Ipca <- Arima(Ipca.ts,order=c(2,1,1),seasonal=list(order=c(2,0,0),period=12))
mod5_Ipca

names(mod1_Ipca)

AICc = data.frame(mod1_Ipca$aicc,mod2_Ipca$aicc,
                  mod3_Ipca$aicc,mod4_Ipca$aicc,
                  mod5_Ipca$aicc)

AICc


checkresiduals(mod3_Ipca)

# Existe ruido blanco

tsdiag(mod3_Ipca)
########################################################
# Pron�stico
# De igual forma, crearemos un pron�stico para el futuro
# a 12 meses en adelante:
########################################################

pronostico1 = forecast(mod3_Ipca, h = 12)

autoplot(pronostico1) +
    labs(title = "Previs�o: 12 meses para a primeira diferen�a",
         subtitle = "SARIMA (1,1,1), (1,0,0)") +
    xlab("Anos") + ylab("Primeira deferen�a")









##################################################################
## Revisar si hay efectos ARCH
## El primer paso es conocer el orden de nuestro modelo ARMA, de 
## esta forma crearemos nuestro mejor ARIMA y checar el orden de 
## nuestro modelo.
##################################################################

#Paso 1: estimar el modelo

#fit1 = auto.arima(Ipca.ts)

#summary(fit1)

#checkresiduals(fit1)
####################



library(quantmod)


Erros_quadrado = resid(mod3_Ipca)^2

##La varianza no es constante, existe Heterocedacistidad
chartSeries(Erros_quadrado)
##################################################################

acf(Erros_quadrado)

pacf(Erros_quadrado)




