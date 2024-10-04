e<- rnorm(100)
x<- runif(100, -5, 5)
y<- 4+3*x+e

#graficos
hist(e)
hist(x)
#GRAFICO DE DISPERCIÓN
plot(x,y)

#HAYAMOS B1
dx=x-mean(x)
dy=y-mean(y)

dxdy<- dx*dy
dx2<- dx^2

b1=sum(dxdy)/ sum(dx2)

#HAYAMOS B0
b0=mean(y)-b1*mean(x)

#MEJOR LINEA MCO
plot(x,y)
abline(a=b0,b=b1)


#REGRESION LINEAL MULTIPLE
e<- rnorm(100)
x1<- runif(100, -4, 4)
x2<- runif(100, 0, 3)
x3<- runif(100, -10, 10)
y<- 2+x1+3*x2+4*x3+e

#MATRIZ VECTOR 1´S
ones<- rep(1,100)
X<- cbind(ones,x1,x2,x3)

#b con MCO
#TRANSPUESTA
t(X)

t(X)%*%X
#INVERSA
solve(t(X)%*%X)

#TRANSPUESTA DE X POR Y
t(X)%*%y

#ahora todo/ BETAS/ EFECTOS MARGINALES
betas<- solve(t(X)%*%X)%*%t(X)%*%y


print(betas)



#AHORA SIN TANTA VUELTA
reg<- lm(y~x1+x2+x3)
summary(reg)

#OTRO PUNTO

library(wooldridge)

View(wage1)

reg<- lm(wage~ educ+exper+tenure+female,data=wage1)
summary(reg)


#UÑTIMO EJERCICIO
View(alcohol)
reg<- lm(abuse~ age+famsize+educ,data=alcohol)
summary(reg)