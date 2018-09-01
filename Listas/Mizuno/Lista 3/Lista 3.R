#PACOTES
library(readxl)
library(foreign)
library(tibble)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

#Quest?o 1############

    #Item a
sample(0:10,prob=c(rep(0.1,times=11)),replace=T)

    #Item b
a=rgamma(100,shape = 2,scale = 3)

    #Item c
b=rnorm(100,3,sqrt(9))

    #Item d
c=rexp(100,7)

#Quest?o 2############

par(mfrow=c(2,2))
par(mar=c(2,2.5,1.7,1.5))
hist(a,probability = T,main="Histograma+Fun??o de densidade",xlab=expression(x),ylab="Densidade")
curve(dgamma(x,shape = 2,scale = 3),add=T)
hist(b,probability = T,main="Histograma+Fun??o de densidade",xlab=expression(x),ylab="Densidade")
curve(dnorm(x,3,sqrt(9)),add=T)
hist(c,probability = T,main="Histograma+Fun??o de densidade",xlab=expression(x),ylab="Densidade")
curve(dexp(x,7),add=T)
par(mfrow=c(1,1))

#Quest?o 3 #####################################################################

    #Item a(SOLU??O 1)
1-ppois(0,5)
ppois(0,5,lower.tail = F)

    #item b(SOLU??O 2)
ppois(11,5)-ppois(2,5)
sum(dpois(3:10,5))

    #Item c
qpois(0.76,5)

    #Item d
qpois(0.73,5,lower.tail = F)

#Quest?o 4

    #Item a
pbeta(0.7,3,10,lower.tail = F)

    #Item b
pbeta(0.25,3,10)-pbeta(0.2,3,10)

    #Item c
qbeta(0.2,3,10)

    #item d
qbeta(0.12,3,10,lower.tail = F)

#Quest?o 5 ###################################################################

amostra1=rnorm(200,10,sqrt(5))
amostra2=rgamma(200,shape=2,scale=4)
amostra3=rpois(200,2)
amostra4=rchisq(200,2)
quantil.teo=qchisq(ppoints(200),2)

    #Item a(RETA PASSA MUITO LONGE DOS PONTOS)
qqplot(quantil.teo,amostra1,xlab="Quantil Teorico",ylab="Quantil Amostral",main="Grafico QQ-Plot")
qqline(amostra1,distribution = function(p)qchisq(p,df=2),col="red")

    #Item b
qqplot(quantil.teo,amostra2,xlab="Quantil Teorico",ylab="Quantil Amostral",main="Grafico QQ-Plot")
qqline(amostra1,distribution = function(p)qchisq(p,df=2),col="blue4")

    #Item c
qqplot(quantil.teo,amostra3,xlab="Quantil Teorico",ylab="Quantil Amostral",main="Grafico QQ-Plot")
qqline(amostra3,distribution = function(p)qchisq(p,df=2),col="forestgreen")
    #Item d
qqplot(quantil.teo,amostra4,xlab="Quantil Teorico",ylab="Quantil Amostral",main="Grafico QQ-Plot")
qqline(amostra4,distribution = function(p)qchisq(p,df=2),col="darkturquoise")

#Quest?o 6#################

    #Item a
pexp(5,5,lower.tail = F)

    #Item b
pexp(9,5)-pexp(2,5)

    #Item c
pexp(0,5)-pexp(0,5)

    #Item d
pexp(2,5)

    #Item e
dbinom(2,15,0.3)

    #Item f
pbinom(4,15,0.3)

    #Item g
pbinom(5,15,0.3)-pbinom(-1,15,0.3)

    #Item h
pbinom(9,15,0.3)-pbinom(2,15,0.3)

    #Item i
1-pbinom(3,15,0.3)

#Quest?o 7################

a=function(x){
  x^3/20*I(x>=1&x<=3)
}

    #Item a
P=integrate(a,2,3)

    #Item b
P=integrate(a,2,2.6)

    #Item c
P=integrate(a,2.8,3)

    #Item d
as.numeric(integrate(a,1,2)[1])+as.numeric(integrate(a,2.5,3)[1])

#Quest?o 8################

    #Item a
curve(punif(x,0,10),0,10,ylab="Probabilidade",main="Acumulada da distribui??o Uniforme")

    #Item b
curve(pgamma(x,shape = 2,scale=3),0,20,ylab="Probabilidade",main="Acumulada da distribui??o Gama")

    #Item c
curve(pnorm(x,3,sqrt(9)),0,100,ylab="Probabilidade",main="Acumulada da distribui??oo Normal")

    #Item d
curve(pexp(x,7),0,4,ylab="Probabilidade",main="Acumulada da distribui??o Exponencial")

    #Item e
a1=function(x){
  if (x>=1&x<=3){
    a=(x^4-1)/80
  }
  if (x>3){
    a=1
  }
  if (x<1){
    a=0
  }
  return(a)
}
a1(1.5)
v=Vectorize(a1)
curve(v,0,4,main="Fun??o de Distribui??o Acumulada de X",ylab="Probilidade Acumulada",xlab=expression(x))

    #Item f
prob=c(0,0.25,.5,.75,1,1)
x=c(-1:4)
plot(x,prob,type = "s",main="Fun??o de Distribui??o Acumulada de X" ,ylab="Probilidade Acumulada",xlab=expression(x))

#Quest?o 9##################

    #Item aa
a=c()
for (i in 1:101){
  a[i]=pbinom(i-1,100,0.85)
}
prob=c(0,a,rep(1,10))
x=c(-1:110)
plot(x,prob,type="s",main="Fun??o de Probabilidade",ylab="Probilidade",xlab=expression(x))

    #Item a2(OUTRA SOLU??O)
p=pbinom(c(-1:110),100,0.85)
x=c(-1:110)
plot(x,p,type="s",main="Fun??o de Probabilidade",ylab="Probilidade",xlab=expression(x))

    #Item b1
a=c()
for (i in 1:26){
  a[i]=ppois(i-1,5)
}
x=c(-5:25)
prob=c(rep(0,5),a)
plot(x,prob,type="s",main="Fun??o de Probabilidade",ylab="Probilidade",xlab=expression(x))

    #Item b2(OUTRA SOLU??O)
p=ppois(c(-5:26),5)
x=c(-5:26)
plot(x,p,type="s",main="Fun??o de Probabilidade",ylab="Probilidade",xlab=expression(x))

    #Item c
x=c(0:10)
prob=c(0,0.05,0.4,0.55,0.6,0.8,0.95,1,1,1,1)
plot(x,prob,type="s",main="Fun??o de Probabilidade",ylab="Probilidade",xlab=expression(x))

#Quest?o 10######################

amostra=rexp(1000,5)
media.amostral=mean(amostra)
media=1/5
hist(amostra, probability = T)
abline(v=media.amostral, col="blue")
abline(v=media, col="red")
curve(dexp(x,5),add=T)

#Quest?o 11#####################

prob=c(0.05,0.35,0.15,0.05,0.2,0.15,0,0,0.05)
x=1:9
ac=c(0,0.05,0.4,0.55,0.6,0.8,0.8,0.8,0.95,1,1,1,1)

    #Item a
plot(x,prob,type="h",ylab="Probabilidae",main="Fun??o de Probabilidade de X")

    #Itemb
plot(x=0:12,ac,type="s",ylab="Probabilidae",main="Fun??o de Probabilidade Acumulada de X",xlab=expression(x))

#Quest?o 12####################

f=function(x,k){
  ((gamma(k)*exp(-k*x-3))/k)*I(x>0)*I(k>0)
}
 
   #Item a
curve(f(x,4),-2,4)

  #Item b
curve(f(2,x),-2,4)

  #Item c e d
max=optimise(f,c(-1,10),maximum = T,k=2)
max
curve(f(2,x),-1,6)
abline(v = max$maximum, col="red")

#Quest?o 13##################

    #Item a1(RESULTADO TEORICO 3*length(x)/sum(x))
G=function(a,b,x){
  ver=b^(length(x)*a)*exp(-b*sum(x))*prod(x^(a-1))/(gamma(a))^length(x)
}
x=c(1,2,2.4,2.8,5,1,3,4,6.3,2.9)
EMV=optimise(G,c(0,10),maximum = T,x=c(1,2,2.4,2.8,5,1,3,4,6.3,2.9),a=3)
EMV
curve(G(a=3,x,c(1,2,2.4,2.8,5,1,3,4,6.3,2.9)),0,5,xlab="beta",ylab="Fun??o Verosimilhan?a")
abline(v = EMV$maximum, col="red")

    #Item a2(LOG)
GL=function(b,x){
  L=length(x)*3*log(b)+2*length(x)-b*sum(x)-length(x)*log(gamma(3))
  return(L)
}
x=c(1,2,2.4,2.8,5,1,3,4,6.3,2.9)
EMV1=optimise(GL,c(0,10),x=c(1,2,2.4,2.8,5,1,3,4,6.3,2.9),maximum = T)
curve(GL(x,c(1,2,2.4,2.8,5,1,3,4,6.3,2.9)),0,5,ylab="Fun??o Verosimilhan?a",main="Log-Verossimilhan?a da Normal",xlab = expression(beta))
abline(v = EMV1$maximum, col="blue")

    #Item b1(RESULTADO TEORICO mean(x))??????
N=function(m,aa){
 
  n=8
  a1=2*pi*10
  a2 = 0
  for(i in 1:n){
    a2= a2 + (aa[i]-m)^2
  }

  a3=-1/20
  L=a1^(-n/2)*exp(a3*sum(a2))
  return(L)
}
x=c(10,12,11,12.8,13,14.9,12,16.2)
EMV=optimise(N,maximum = T,aa=c(10,12,11,12.8,13,14.9,12,16.2),interval = c(0,20))
EMV
curve(N(x,c(25,23,22,21,27,39,35,33,32)),0,50)

    #Item b2(LOG)???????
NL=function(m,x){
  L=-(length(x)/2)*log(2*pi*10)-(1/2*10)*sum((x-m)^2)
}
EMV1=optimise(NL,maximum = T,x=c(10,12,11,12.8,13,14.9,12,16.2),interval = c(0,20))
EMV1
curve(NL(x,c(25,23,22,21,27,39,35,33,32)),0,50)

    #Item c1
GEO=function(p,x){
  p^length(x)*(1-p)^(sum(x)-length(x))
}
x=c(2,3,2,2,3,3,3,4,4,5,7,2,2,2)
EMV=optimise(GEO,c(0,1),maximum = T,x=c(2,3,2,2,3,3,3,4,4,5,7,2,2,2))
EMV
curve(GEO(x,c(2,3,2,2,3,3,3,4,4,5,7,2,2,2)),0,1,main="Geometrica",xlab="p",ylab="Fun??o Verosimilhan?a")
abline(v = EMV$maximum, col="red")

    #Item c2(LOG)
GEOL=function(p,x){
  length(x)*log(p)+(sum(x)-length(x))*log(1-p)
}
EMV1=optimise(GEOL,c(0,1),maximum = T,x=c(2,3,2,2,3,3,3,4,4,5,7,2,2,2))
EMV1
curve(GEOL(x,c(2,3,2,2,3,3,3,4,4,5,7,2,2,2)),0,1,main="Log-Verossimilhan?a da Geometrica",xlab=expression(p),ylab="Fun??o Verosimilhan?a")
abline(v = EMV$maximum, col="yellow")

    #Item d1(RESULTADO TEORICO sqrt(sum((x-10)^2)/length(x)))
N2=function(s,x){
  a1=length(x)
  a2=2*pi*s^2
  a3=(x-10)^2
  a4=-1/(2*s^2)
  L=a2^(-a1/2)*exp(a4*sum(a3))
  return(L)
}
x=c(25,23,22,21,27,39,35,33,32)
EMV=optimise(N2,c(0,20),maximum = T,x=c(25,23,22,21,27,39,35,33,32))
EMV
curve(N2(x,c(25,23,22,21,27,39,35,33,32)),0,50,main="Normal",xlab=expression(sigma),ylab="Fun??o Verosimilhan?a")
abline(v = EMV$maximum, col="blue4")

    #Item d2(LOG)
N2L=function(s,x){
  L=-(length(x)/2)*log(2*pi*s^2)+(-1/(2*s^2))*sum((x-10)^2)
  return(L)
}
EMV1=optimise(N2L,maximum = T,x=c(25,23,22,21,27,39,35,33,32),interval = c(0,60))
EMV1
curve(N2L(x,c(25,23,22,21,27,39,35,33,32)),0,100,main="Log-Verossimilhan?a da Normal",xlab=expression(sigma),ylab="Fun??o Verosimilhan?a")
abline(v = EMV1$maximum, col="blue2")

    #Item e1
EXP=function(a,x){
  L=a^length(x)*exp(-a*sum(x))
  return(L)
}
x=c(4,5,6.2,4,3,5,6.9,7,9,3)
EMV=optimise(EXP,c(0,1),maximum = T,x=c(4,5,6.2,4,3,5,6.9,7,9,3))
EMV
curve(EXP(x,c(4,5,6.2,4,3,5,6.9,7,9,3)),0,1,main="Exponencial",xlab=expression(lambda),ylab="Fun??o Verosimilhan?a")
abline(v = EMV$maximum, col="yellow")

    #Item e2(LOG)
EXPL=function(a,x){
  L=length(x)*log(a)-a*sum(x)
  return(L)
}
EMV1=optimise(EXPL,c(0,1),maximum = T,x=c(4,5,6.2,4,3,5,6.9,7,9,3))
EMV1
curve(EXPL(x,c(4,5,6.2,4,3,5,6.9,7,9,3)),0,1,main="Log-Verossimilhan?a da Exponencial",xlab=expression(lambda),ylab="Fun??o Verosimilhan?a")
abline(v = EMV1$maximum, col="green4")

#Quest?o 14#################

    #Item a(FOI CONSIDERADO GA(SHAPE,SCALE))
t=1000
n=100


mat.amostra=matrix(NA,ncol=t,nrow=n)
for (i in 1:t){
  mat.amostra[,i]=rgamma(n,shape=2,scale=8)
}
media.amostral=apply(mat.amostra,2,mean)
hist(media.amostral,probability = T)
curve(dnorm(x,16,sqrt(2*8^2/n)),add=T)

    #Item b(OLHAR A DEFINI??O DE DISTRIBUI??O GEOMETRICA DO R)
t=1000
n=1000

mat.amostra=matrix(NA,ncol=t,nrow=n)
for (i in 1:t){
  mat.amostra[,i]=rgeom(n,0.8)
}
media.amostral=apply(mat.amostra,2,mean)
hist(media.amostral,probability = T)
curve(dnorm(x,0.2/0.8,sqrt(0.2/0.8^2/n)),add=T)

    #Item c
t=1000
n=10

mat.amostr=matrix(NA,ncol=t,nrow=n)
for (i in 1:t){
  mat.amostra[,i]=runif(n,2,20)
}
media.amostral=apply(mat.amostra,2,mean)
hist(media.amostral,probability=T)
curve(dnorm(x,9,sqrt((18^2/12)/n)),add=T)

    #Item d
t=1000
n=10
x=c(1:7)
prob=c(0.05,0.35,0.15,0.05,0.2,0.15,0.05)
media.pop=mean(x)
var.pop=var(x)
mat.amostra=matrix(NA,ncol=t,nrow=n)
for (i in 1:t){
  mat.amostra[,i]=sample(x,n,replace = T)
}
media.amostral=apply(mat.amostra,2,mean)
hist(media.amostral,probability = T)
curve(dnorm(x,media.pop,sqrt(var.pop/n)),add=T)

    #Item e
t=1000
n=5

mat.amostra=matrix(NA,ncol=t,nrow=n)
for (i in 1:t){
  mat.amostra[,i]=rnorm(n,2,sqrt(8))
}
media.amostral=apply(mat.amostra,2,mean)
hist(media.amostral,probability = T)
curve(dnorm(x,2,sqrt(8/n)),add=T)

    #Item f
t=1000
n=100

mat.amostra=matrix(NA,ncol=t,nrow=n)
for (i in 1:t){
  mat.amostra[,i]=rbinom(n,20,0.8)
}
media.amostral=apply(mat.amostra,2,mean)
hist(media.amostral,probability = T)
curve(dnorm(x,16,sqrt(3.2/n)),add=T)

    #Item g
t=1000
n=1000

mat.amostra=matrix(NA,ncol=t,nrow=n)
for (i in 1:t){
  mat.amostra[,i]=rchisq(n,3)
}
media.amostral=apply(mat.amostra,2,mean)
hist(media.amostral,probability = T)
curve(dnorm(x,3,sqrt(6/n)),add=T)

#Quest?o 15##################################
n=1000
amX=rnorm(n,25,sqrt(70))
amT=rchisq(n,5)
amY=rexp(n,10)
amZ=rgamma(n,3,scale=5)
amW=rgamma(n,10,scale=5)

    #Item a
s=amZ+amW
hist(s,probability=T)
curve(dgamma(x,13,scale=5),add=T)

    #Item b
Q=2*amY
hist(Q,freq = F)
curve(dgamma(x,2,scale=10),add=T)

    #Item c
amX1=rnorm(n,25,sqrt(70))
amX2=rnorm(n,25,sqrt(70))
amX3=rnorm(n,25,sqrt(70))
amX4=rnorm(n,25,sqrt(70))
amX5=rnorm(n,25,sqrt(70))
Xbarra=(amX1+amX2+amX3+amX4+amX5)/5
R=(Xbarra-25)/sqrt(70/5)
hist(R,probability = T)
curve(dnorm(x),add=T)

    #Item d
s=3*amT
hist(s,probability=T)
curve(dgamma(x,shape=15/2,scale=1/2),add=T)

    #Item e
amX1=rnorm(n,25,sqrt(70))
amX2=rnorm(n,25,sqrt(70))
amX3=rnorm(n,25,sqrt(70))
E=amX1+amX2+amX3+20
hist(E,probability = T)
curve(dnorm(x,95,sqrt(210)),add=T)
