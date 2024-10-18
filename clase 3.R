library(wooldridge)
library(modelsummary)
library(dplyr)
View(bwght)

#Regresion con el ingreso de la familia
reg1<-lm(bwght~ faminc, data = bwght)

#Regresion con el n de cigarrillos
reg2<- lm(bwght~ cigs, data = bwght)

#modelo mas amplio
reg3<- lm(bwght~ cigs+faminc, data = bwght)

modelos<- list("modelo 1"= reg1,"modelo 2"=reg2, "modelo 3"=reg3, "modelo 4"=reg4, "modelo 5"=reg5,"modelo 6"=reg6)
modelsummary::modelsummary(modelos)

#ahora vamos a trasnformar de onzas a libras
reg4<- lm(bwghtlbs~ cigs+faminc, data = bwght)

#ahora vamos a usar paquetes de cigarrillos en vez de cigarros
reg5<- lm(bwght~ packs+faminc, data = bwght)

#ejercicio 2

bwght_alt<-bwght|>mutate(zbw=(bwght-mean(bwght))/sd(bwght),
                         zfi=(faminc-mean(faminc))/sd(faminc),
                         zcigs=(cigs-mean(cigs))/sd(cigs))
View(bwght_alt)
#modelo con las nuevas variables estandarizadas 
reg6<- lm(zbw~ zfi+zcigs, data = bwght_alt)


sigam_cigs<-sd(bwght$cigs)
sigam_bwght<-sd(bwght$bwght)


