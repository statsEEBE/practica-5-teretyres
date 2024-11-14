## DISTRIBUCIÓN NORMAL

#1.Distribución poblacional

mu<- 95.3
sigma<- 5.7
#N-> N(mu, sigma)
curve(dnorm(x, mean=mu, sd=sigma), xlim=c(70,120), col='red')#graficar
rnorm(4, mu, sigma)

#2.Distribuciones de muestreo
#Un estadístico es una función de la muestra

#Suma muestral: Y=sum(xi)
#E(Y)= sum(n*mu)
#Var(Y)= n*sigma^2

#Media muestral/ Promedio: Xbarra=1/n*sum(xi)
#E(Xbarra)= mu
#Var(Xbarra)= sigma^2/n

#Varianza muestral: S^2= 1/(n-1)*sum(x-xbarra)^2
#Estandarización de la varianza: W=(n-1)*s^2/sigma^2 (chi^2)

#a): valor esperado de la suma de la muestra
#Estimación E(Y)
Y<- function(i){sum(rnorm(4, mu, sigma))}
Y100000<-sapply(1:100000, Y)#realizamos la muestra 100000 veces
mean(Y100000)
hist(Y100000, freq=FALSE)
curve(dnorm(x,4*mu, 2*sigma), col='red', add=TRUE)
#Cálculo E(Y)
E= 4*mu

#b): la varianza de la suma de la muestra
V= 100*sigma

#c): P(x>=103) (es de población, no muestreo)
1-pnorm(103, mu, sigma)

#d):probabilidad de que la media de la muestra sea menor de 98
#Estimación
xbar<- function(i){mean(rnorm(4, mu, sigma))}
xbar100000<- sapply(1:100000, xbar)
hist(xbar100000, freq=FALSE)
mean(xbar100000<98)
curve(dnorm(x,mu, sigma/sqrt(4)), col='red', add=TRUE)
#Cálculo
pnorm(98, mu, sigma/sqrt(4))

#e) probabilidad de que la varianza de la muestra sea más grange que 32
#Estimación
ssq<- function(i){var(rnorm(100, mu, sigma))}
ssq100000<- sapply(1:100000, ssq)
hist(ssq100000, freq=FALSE)
hist(ssq100000*(100-1)/sigma^2, freq= FALSE)
curve(dchisq(x, 100-1), col='red', add=TRUE)

#Cálculo
w<- 32*(100-1)/sigma^2
1- pchisq(w, 100-1)
