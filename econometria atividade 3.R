#carregando os dados 
library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)


library(readxl)
dados3 <- read_excel("Econometria/atv 3 econometria.xlsx")
View(dados3)
attach(dados3)

#construção do modelo 

ajuste<- lm(CO2 ~ PIB + Pop, dados3)
ajuste

#anova 

anova(ajuste)

#analise gráfica 

par(mfrow=c(2,2))

plot(ajuste)

par(mfrow=c(1,1))

#correlação 

dados3_2<- dados3[,2:3]
cor(dados3_2)
## as variavéis são altamente correlacionadas

pairs(dados3_2)
pairs.panels(dados3_2)
## as variáveis possuem um relacionamento linear forte e positivo entre si

summary(ajuste)
vif(ajuste)

##Conclusão 

#na anova a interação é significativa porém ao realizar os testes t
# as variáveis individualmente não são significativas, além disso 
#as variáveis possuem uma forte correlação e o valor de VIF apesar de não ser maior 
#que 10, está bastante próximo desse valor. Então concluímos que há multicolinearidade.



# Ajuste do modelo 

a<- CO2/Pop
b<-PIB/Pop
summary(lm(a~b))
