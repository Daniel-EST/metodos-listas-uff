# Pacotes ######################################################################
require(purrr)
require(dplyr)
require(readr)
require(ggplot2)
require(gridExtra)
require(sjPlot)
# Questao 01 ###################################################################
#a
q1a <- function(n,x)return(sample(x,n,replace = T))
q1A <- q1a(100,c(0:10))
#b
q1b <- rgamma(100,2,3) ; q1b
#c
q1c <- rnorm(100,3,sqrt(9)) ; q1c
#d
q1d<- rexp(100,7) ; q1d
# Questao 02 ####################################################################
dev.off()
par(mfrow = c(2,2))
hist(q1A,probability = T)
q1Ad <- function(x) return((1/11)*I(x>=0)*I(x<=10))
curve(q1Ad(x),add = T)
hist(q1b,probability = T)
curve(dgamma(x,2,3), add = T)
hist(q1c,probability = T)
curve(dnorm(x,3,3), add = T)
hist(q1d,probability = T)
curve(dexp(x,7), add = T)
par(mfrow = c(1,1))

# Questão 03 ####################################################################
#a
ppois(0,5,lower.tail = F)
#b
ppois(11,5) - ppois(3,5) 
#c
qpois(76/100,5)
#d
qpois(73/100,5,lower.tail = F)
# Questao 04 ####################################################################
#a
pbeta(7/10,3,10,lower.tail = F)
#b
pbeta(25/100,3,10) - pbeta(2/10,3,10)
#c
qbeta(20/100,3,10)
#d
qbeta(12/100,3,10,lower.tail = F)
# Questao 05 ####################################################################
dev.off()
par(mfrow = c(2,2))
#a
amostraQ5a <- rnorm(200,10,sqrt(5))
qqplot(amostraQ5a,qchisq(ppoints(200),2))
qqline(amostraQ5a,distribution = function(x)return(qchisq(x,2)), col = 'red')
#b
amostraQ5b <- rgamma(200,2,4)
qqplot(amostraQ5b,qchisq(ppoints(200),2))
qqline(amostraQ5b, distribution = function(x)return(qchisq(x,2)),col = 'red')
#c
amostraQ5c <- rpois(200,2)
qqplot(amostraQ5c,qchisq(ppoints(200),2))
qqline(amostraQ5c,distribution = function(x)return(qchisq(x,2)),col = 'red')
#d
amostraQ5d <- rchisq(200,2)
qqplot(amostraQ5d,qchisq(ppoints(200),2))
qqline(amostraQ5d,distribution = function(x)return(qchisq(x,2)),col = 'red')
par(mfrow = c(1,1))
# Questao 06 ####################################################################
#a
pexp(5,5,lower.tail = F)
pbinom(5,15,0.3,lower.tail = F)
#b
pexp(9,5) - pexp(2,5)
pbinom(9,15,0.3) - pbinom(2,15,0.3)
#c
dbinom(0,15,0.3)
#d
pexp(2,5)
pbinom(1,15,0.3)
#e
dbinom(2,15,0.3)
#f
pexp(5,5)
pbinom(4,15,0.3)
#g
#h
#i
# Questao 07 ####################################################################
# Criando a funcao
q7a <- function(a){
  if(a<1) return(0)
  if(a>3) return(1)
  return(integrate(function(x)return(x^3/20),1,a)[1][[1]])
}
#a
1-q7a(2)
#b
q7a(2.6) - q7a(2)
#c
1-q7a(2.8)
#d
q7a(2) + (1 - q7a(2.5))
# Questao 08 ####################################################################
#a
plot(-1:11,punif(-1:11,0,10), type = 's')
#b
curve(pgamma(x,2,3))
#c
curve(pnorm(x,3,3),-10,10)
#d
curve(pexp(x,7),-0.15,1.15)
#e
q7avec <- Vectorize(q7a)
curve(q7avec,0,4)
#f
q7fVal <- -1:6
q7fProbs <- c(0,rep(1/length(q7fVal),4),0,0,0)
{
  temp <- 0
  acum <- 0
  for(i in 2:length(q7fVal)){
    temp <- q7fProbs[i-1]
    acum[i] <- acum[i-1] + temp
  }
  acumQ7f <- acum
  rm(list = c('acum','i','temp'))
}
plot(-1:6,acumQ7f,type = 's')
# Questao 09 ####################################################################
#a
par(mfrow = c(1,2))
plot(dbinom(-1:110,100,0.85), type = 'h')
plot(pbinom(-1:110,100,0.85), type = 's')
par(mforw = c(1,1))
#b
par(mfrow = c(1,2))
plot(dpois(-5:25,5), type = 'h')
plot(ppois(-5:25,5), type = 's')
par(mfrow = c(1,1))
#c
# Dist
q9cVal <- 1:9
q9cProb <- c(0,5/100,35/100,15/100,5/100,20/100,15/100,5/100,0)
plot(q9cVal,q9cProb,type = 'h')
# Acumulada
{
  temp <- 0
  acum <- NA
  acum[1] <- 0
  for(i in 2:8){
    temp <- q9cProb[i-1]
    acum[i] <- acum[i-1] + temp
  }
  acumQ9c <- acum
  rm(list = c('acum','temp','i'))
}
plot(acumQ9c,type = 's')
# Questao 10 ###################################################################
q10Amostra <- rexp(1000,5)
hist(q10Amostra)
abline(v = mean(q10Amostra),col = 'red')
abline(v = 1/5)
# Questao 11 ###################################################################
par(mfrow  = c(1,2))
q11V <- 0:12
q11Prob <- c(0,0,0.05,0.35,0.15,0.05,0.20,0.15,0.00,0.00,0.05,0,0)
plot(q11V,q11Prob, type = 'h')
acum <- 0
temp <- 0
{
  for(i in 2:14){
    temp <- q11Prob[i-1]
    acum[i] <- acum[i-1] + temp
  }
  acumQ11 <- acum
  rm(list=c('acum','i','temp'))
  plot(acumQ11,type = 's')
}
par(mfrow = c(1,1))
# Questao 12 ###################################################################
# Funcao
gamaQ12 <- function(x,k){
  if(k<=0||x<=0) return(0)
  else{
    return((gamma(k)*exp((-k*x)-3))/k)
  }
}
gamaQ12 <- Vectorize(gamaQ12)
#a
curve(gamaQ12(x,4),-2,6)
#b
curve(gamaQ12(2,x),-2,10)
#c
maximoGamaQ12 <- optimize(gamaQ12,k = 2,lower = -1,upper = 1,maximum = T)[[1]]
#d
curve(gamaQ12(x,2),-2,2)
abline(v = maximoGamaQ12,col = 'blue')
# Questao 13 ###################################################################
#a
vq13 <- c(1,2,2.4,2.8,5,1,3,4,6.3,2.9)
EMVGam <- function(x,b){
  n <- length(x)
  return(b**(3*n)*exp(-b*sum(x))*prod(x**2)/gamma(3)**n)
}
maxvq13a <- optimize(EMVGam,x=vq13,lower = 0,upper = 10,maximum = T) ; maxvq13a
curve(EMVGam(vq13,x),0,10)
abline(v = maxvq13a$maximum)
LEMVGam <- function(x,b){
  n <- length(x)
  med <- mean(x)
  product <- prod(x**2)
  return(log(((b**(3*n))/(gamma(3)**n))*exp(-b*n*med))+log(2*sum(log(x))))
}
lmaxvq13a <- optimize(LEMVGam,x=vq13,lower = 0,upper = 10,maximum = T) ; lmaxvq13a
curve(LEMVGam(vq13,x),0,10)
abline(v =lmaxvq13a$maximum)
#b

#c
#d
#e
# Questao 14 ###################################################################
#a
amostraQ14a <- matrix(rep(NA,100000),ncol = 1000,nrow = 100)
for(i in 1 : 1000){
  amostraQ14a[,i] <- rgamma(100, shape = 2, scale = 8)
}
mediaAmostraQ14a <- apply(amostraQ14a,2,mean)
hist(mediaAmostraQ14a,freq = F)
curve(dnorm(x,mean(mediaAmostraQ14a),sqrt(var(mediaAmostraQ14a))),add =T)
#b
amostraQ14b <- matrix(rep(NA,100000),ncol = 1000,nrow = 100)
for(i in 1:1000){
  amostraQ14b[,i] <- rgeom(100,8/10)
}
mediaAmostraQ14b <- apply(amostraQ14b,2,mean)
hist(mediaAmostraQ14b,freq = F)
curve(dnorm(x,mean(mediaAmostraQ14b),sqrt(var(mediaAmostraQ14b))),add =T)
#c
amostraQ14c <- matrix(rep(NA,100000),ncol = 1000,nrow = 100)
for(i in 1:1000){
  amostraQ14c[,i] <- runif(100,2,20)
}
mediaAmostraQ14c <- apply(amostraQ14c,2,mean)
hist(mediaAmostraQ14c,freq = F)
curve(dnorm(x,mean(mediaAmostraQ14c),sqrt(var(mediaAmostraQ14c))),add =T)

#d
amostraQ14d <- matrix(rep(NA,100000),ncol = 1000,nrow = 100)
for(i in 1:1000){
  amostraQ14d[,i] <- sample(1:7,100,replace = T, prob = c(5/100,35/100,15/100,5/100,20/100,15/100,5/100))
}
mediaAmostraQ14d <- apply(amostraQ14d,2,mean)
hist(mediaAmostraQ14d,freq = F)
curve(dnorm(x,mean(mediaAmostraQ14d),sqrt(var(mediaAmostraQ14d))),add =T)
#e
amostraQ14e <- matrix(rep(NA,100000),ncol = 1000,nrow = 100)
for(i in 1:1000){
  amostraQ14e[,i] <- rnorm(100,2,sqrt(8))
}
mediaAmostraQ14e <- apply(amostraQ14e,2,mean)
hist(mediaAmostraQ14e,freq = F)
curve(dnorm(x,mean(mediaAmostraQ14e),sqrt(var(mediaAmostraQ14e))),add =T)
#f
# Deu preguica mas farei

# Questao 15 ###################################################################
X <- matrix(rep(NA,500),ncol = 100,nrow = 5)
for(i in 1:100){
  X[,i] <- rnorm(5,25,sqrt(70))
}
t <- rchisq(100,5)
Y <- rexp(100,10)
Z <- rgamma(100,3,5)
W <- rgamma(100,10,5)
#a
hist(Z+W,probability = T)
curve(dgamma(x,13,5),add = T)
#b
hist(2*Y, probability = T)
curve(dgamma(x,2,10),add = T)
#c
Xbarrac <- apply(X,2,mean)
M <- (Xbarrac - 25)/sqrt(70/5)
hist(M,freq = F)
curve(dnorm(x),add=T)
#d
hist(3*t,freq = F)
curve(dgamma(x,15/2,1/2),add=T)
#e
# Preguica
