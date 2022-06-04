######################### Regressão Linear Simples #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)
library(readxl)

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read_excel("Econometria/econometria-dados.xlsx") # Carregamento do arquivo csv
View(dados)                                 # Visualização dos dados em janela separada
glimpse(dados)                              # Visualização de um resumo dos dados
str(dados)

dados$y <- as.numeric(dados$y)
dados$x <- as.numeric(dados$x)



# Passo 3: Verificação dos pressupostos para a regressão linear


## Relação linear entre a VD e a VI:
### VD: y
### VI: x

plot(dados$x, dados$y)


## Construção do modelo:
mod <- lm(y ~ x, dados)


## Análise gráfica:

par(mfrow=c(2,2))

plot(mod)

### Interpretação: https://data.library.virginia.edu/diagnostic-plots/

par(mfrow=c(1,1))


## Normalidade dos resíduos:
shapiro.test(mod$residuals)


## Outliers nos resíduos:
summary(rstandard(mod))


## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(mod)


## Homocedasticidade (Breusch-Pagan):
bptest(mod)


# Passo 4: Análise do modelo
summary(mod)





ggplot(data = dados, mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\",\")~~")),
                        label.x = 4500, label.y = 2800) +
  theme_classic()
