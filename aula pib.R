install.packages("pwt8")
library(pwt8) # Carregando o pacote
data("pwt8.0") # Carregando os dados do pacote

br <-subset(pwt8.0, country=="Brazil",
            select=c(rgdpna, avh, xr))
View(br)
dim(br)
str(br)
#A partir desses dados podemos criar novas variáveis. 
#Que tal criarmos uma proxy, denida como PIB real dividido 
#pela média de horas trabalhadas, para produtividade.


br$prod <- br$rgdpna/br$avh

# salvando os dados

write.table(br, file='PIB_Horas.txt')


dados <- read.table('PIB_Horas.txt')
head(dados)


# Estruturando os dados num formato para séries temporais
dados <- ts(dados, start=1950, freq=1)

#Para finalizar podemos visualizar os dados através do 
#comando plot

plot(dados)

#Podemos aprimorar um pouco este gráco 
#através das opções do comando plot, como
#pode ser visto abaixo.

plot(dados,
     col='blue',
     main='Dados brasileiros',
     xlab='Ano')

#########################################################
#Sistema Gerenciador de Séries Temporais (SGS)

#Agora vamos para um exemplo prático. Neste próximo 
# exercício usaremos o PIB trimestral (série 1232 do SGS 
# do Banco Central do Brasil). Como sempre começamos
# importando os dados e, em seguida, visualizando eles.
library(readr)

dados <- read.csv('Econometria/pib.csv',sep=";")
head(dados)
# Estruturando os dados num formato para séries temporais

################################################
# Removendo tendências de séries univariadas
################################################

pib <- ts(dados,start=1991, freq=4)

pib

dim(pib)

pib <- pib[,-1]
plot(pib,main='PIB brasileiro',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='red',lty=1)


grid(col='darkgrey',
     lwd=2)
# Agora devemos rodar a regressão, para isso devemos 
# criar os dummies sazonais.

Q <- ordered(cycle(pib))

Q

pib.reg <- lm(pib~Q)

#  Utilizando o comando summary podemos obter 
# um resumo da regressão.

summary(pib.reg)


#Como podemos ver, o resumo já nos dá o p-valor do teste 
#F que neste caso é igual a 0.794. Logo, não rejeitamos 
#a hipótese nula de que não existe sazonalidade. Porém, 
#para fins didáticos vamos assumir que rejeitamos a 
#H0 e continuaremos com o procedimento
#de dessazonalizar os dados.

#Como podemos ver, a série dessazonalizada começa em 
#níveis negativos. Isto acontece pois ao subtrair hat(Yt)
#da nossa série original estamos também retirando a sua 
#média, captada por hat(alpha). Logo, 
#devemos "normalizar" nossa série adicionando sua média.

pib.des <- ts(resid(pib.reg),
              start = 1991, freq=1)

pib.hat <- ts(fitted(pib.reg),
              start=1991, freq=1)

par(mfrow=c(1,2))
plot(pib.des)

pib.desn <- pib.des + mean(fitted(pib.reg))
#Agora podemos fazer um gráco com a série original e 
#  sua versão dessazonalizada.

par(mfrow=c(1,1))

plot(pib,
     main='',
     xlab='Ano', ylab='',
     col='blue',
     bty='l')
par(new=TRUE)
plot(pib.desn,
     axes=F, ann=F,
     col='red',
     lty=2)

legend('topleft',
       c('PIB', 'PIB dessazonalizado'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')
grid(col='darkgrey')

