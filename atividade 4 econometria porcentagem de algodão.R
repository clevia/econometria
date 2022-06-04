rm(list=ls(all=TRUE))
require(lattice)
require(dae)
require(ExpDes.pt)

y<- c(7,  7,  15, 11, 9,
      12, 17, 12, 18, 18,
      14, 18, 18, 19, 19,
      19, 25, 22, 19, 23,
      7,  10, 11, 15, 11);  y

n<- length(y); n
i<- 5 # numero de níveis do fator
r<- 5 # numero de repetições

algodao <- factor(rep(c(1:i), each=r), labels=c("15","20","25","30","35")); algodao
repeticoes<- factor(rep(c("1","2","3","4","5"),each=1));repeticoes

DIC<- data.frame(algodao,repeticoes,y); DIC

attach(DIC)
str(DIC)

xyplot(y ~ algodao, data=DIC, pch=c(19))
plot(DIC$algodao, DIC$y)
modelo <- lm(y ~ algodao, DIC)

par(mfrow=c(2,2))

plot(modelo)

par(mfrow=c(1,1))

dic(algodao, y)
