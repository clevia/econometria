#################################################################################
#Atividade 2 Econometria

#carregamento de pacotes

library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)

library(readxl)

# carregamento do banco de dados 

dados2 <- read_excel("Econometria/econometria-dadosatv2.xlsx")
View(dados2)
glimpse(dados2)
str(dados2)

#Verificação dos presupostos

## Relação linear entre a VD e a VI:
### VD: y
### VI: x
plot(dados2$x, dados2$y)

## Construção do modelo:

mod1 <- lm(y ~ x, dados2)

## Análise gráfica:

par(mfrow=c(2,2))

plot(mod1)

par(mfrow=c(1,1))

## Normalidade dos resíduos:
shapiro.test(mod1$residuals)


## Outliers nos resíduos:
summary(rstandard(mod1))


## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(mod1)


## Homocedasticidade (Breusch-Pagan):
bptest(mod1)


#Análise do modelo
summary(mod1)





ggplot(data = dados2, mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\",\")~~")),
                        label.x = 200, label.y = 50) +
  theme_get()

