#PACOTES 
library(dplyr)
library(tibble)
library(foreign)
library(readr)
library(readxl)
library(DescTools)
setwd("/Volumes/MIZUNO/Listas/Lista 5/HTML")
 
#Questao 1#########

    #Item a
TH.media=function(x,sigma,alfa,mi0){
  n <- length(x)
  x.barra <- mean(x)
  z <- qnorm(alfa)
  #Criando a Regiao Critica
  x.c <- mi0+sigma*z/sqrt(n)
  #Estimativas e RC
  cat("Estimativa Pontual;",x.barra,"\n","Regiao cirtica=(-inf,",x.c,"]")
  #Decidindo
  if (x.barra<x.c){
    cat("\n","Rejeita-se Ho a um nivel de significancia",alfa)
  }
  else{
    cat("\n","Nao rejeita-se Ho a um nivel de significancia",alfa)
  }
}

    #Item a(NAO ESTOU PADRONIZANDO) 
TH.media2=function(x,sigma,alfa,mi0){
  n <- length(x)
  x.barra <- mean(x)
  #Determinando a Regiao Critica
  x.c <- qnorm(p=alfa,mean=mi0,sd=sigma/sqrt(n))
  cat("Estimativa Pontual;",x.barra,"\n","Regiao cirtica=(-inf,",x.c,"]")
  #Decidindo
  if (x.barra<x.c){
    cat("\n","Rejeita-se Ho a um nivel de significancia",alfa)
  }
  else{
    cat("\n","Nao rejeita-se Ho a um nivel de significancia",alfa)
  }
}
a <- rnorm(10,2,5)
TH.media(a,sigma=5,alfa=.1,mi0=2)
TH.media2(a,sigma=5,alfa=.1,mi0=2)

    #Item b
TH.var=function(x,sigma20,alfa){
  n <- length(x)
  s2 <- var(x)
  #Criando a Regiao Critica
  q.c <- qchisq(df=n-1,lower.tail=FALSE,p=alfa)
  #Estimativas e RC
  cat("Estimativa Pontual;",s2,"\n","Regiao cirtica; [",q.c,"inf)")
  #Decidindo
  q <- (n-1)*s2/sigma20
  if (q>q.c){
    cat("\n","Rejeita-se Ho a um nivel de significancia",alfa)
  }
  else{
    cat("\n","Nao rejeita-se Ho a um nivel de significancia",alfa)
  }
}

    #Item c
TH.prop=function(x,h,alfa,p0){
  y <- c(1:length(x))
  for (i in 1:length(x)){
    if (x[i]<h){
      y[i] <- 0
    }
    if (x[i]>h){
      y[i] <- 1
    }
  }
  print(y)
  n <- length(y)
  p.chapeu <- mean(y)
  #Criando a Regiao Critica
  p.c1 <- qnorm(alfa/2,mean=p0,sd=sqrt(p0*(1-p0)/n))
  p.c2 <- qnorm(1-alfa/2,mean=p0,sd=sqrt(p0*(1-p0)/n))
  #Estimativas e RC
  cat("Estimativa Pontual;",p.chapeu,"\n","Regiao cirtica=(-inf;",p.c1,"]U[",p.c2,"inf]")
  #Decidindo
  if (p.chapeu>p.c1 | p.chapeu<p.c2){
    cat("\n","Rejeita-se Ho a um nivel de significancia",alfa)
  }
  else{
    cat("\n","Nao rejeita-se Ho a um nivel de significancia",alfa)
  }
}
a <- rnorm(10,10,1)
TH.prop(x=a,alfa=0.05,h=10,p0=0.3)

    #Item d
TH.media2=function(x,sigma,alfa,mi0){
  n <- length(x)
  x.barra <- mean(x)
  z <- (x.barra-mi0)*sqrt(n)/sigma
  #Criando a Regiao Critica (com base em z)
  z.c <- qnorm(alfa)
  #Estimativas e RC
  cat("Estimativa Pontual;",x.barra,"\n","Regiao cirtica=[-inf,",z.c,"]")
  #Decidindo
  if (z<z.c){
    cat("\n","Rejeita-se Ho a um nivel de significancia",alfa)
  }
  else{
    cat("\n","Nao rejeita-se Ho a um nivel de significancia",alfa)
  }
}
  
    #Item e
ET2=function(n,alfa,sigma2,mi0,mi){
  sigma <- sqrt(sigma2)
  #Criando a Regiao Critica
  z <- qnorm(alfa)
  x.c <- mi0+sigma*z/sqrt(n)
  #Calculando a probabilidade pedida(estou padronizando)
  z <- (x.c-mi)*sqrt(n)/sigma
  prob <- pnorm(z,lower.tail = FALSE)
  return(prob)
}

ET2.2=function(n,alfa,sigma2,mi0,mi){
  sigma <- sqrt(sigma2)
  #Criando a Regiao Critica 
  x.c <- qnorm(p=alfa,mean=mi0,sd=sigma/sqrt(n))
  #Calculando a probabilidade pedida
  prob <- pnorm(x.c,mi,sigma/sqrt(n),lower.tail=FALSE)
  return(prob)
}
ET2(n=10,alfa=0.1,sigma2=25,mi0=10,mi=5)
ET2.2(n=10,alfa=0.1,sigma2=25,mi0=10,mi=5)

    #Item f
curve(ET2(n=20,alfa=0.05,mi0=30,sigma2=10,x),0,60,ylab=expression(beta(mu)),xlab=expression(mu),col="red")
curve(ET2.2(n=20,alfa=0.05,mi0=30,sigma2=10,x),0,60,ylab=expression(beta(mu)),xlab=expression(mu),col="blue")

    #Item g
curve(ET2(n=20,alfa=0.05,mi0=30,sigma2=10,x),15,40,add=TRUE,ylab=expression(beta(mu)),xlab=expression(mu))
curve(ET2(n=30,alfa=0.05,mi0=30,sigma2=10,x),15,40,add=TRUE,col="red",ylab=expression(beta(mu)),xlab=expression(mu))
curve(ET2(n=50,alfa=0.05,mi0=30,sigma2=10,x),15,40,add=TRUE,col="blue",ylab=expression(beta(mu)),xlab=expression(mu))
curve(ET2(n=100,alfa=0.05,mi0=30,sigma2=10,x),15,40,add=TRUE,col="green",ylab=expression(beta(mu)),xlab=expression(mu))
legend(15, 1,
       c("20", "30", "50", "100"), title="n",
       col=c("black", "red","blue","green"),
       lty=c(1, 1))

    #Item h
poderBi=function(n,alfa,sigma2,mi0,mi){
  sigma <- sqrt(sigma2)
  #achando a regiao critica
  x.c1 <- qnorm(1-alfa/2,mean=mi0,sd=sqrt(sigma2/n))
  x.c2 <- qnorm(alfa/2,mean=mi0,sd=sqrt(sigma2/n))
  #funcao poder
  poder <- pnorm(x.c2,mean=mi,sd=sqrt(sigma2/n))+pnorm(x.c1,mean=mi,sd=sqrt(sigma2/n)
                                                       ,lower.tail=FALSE)
  return(poder)
}

    #Item i
curve(poderBi(n=20,alfa=0.05,mi=x,sigma2=25,mi0=30),10,50,ylab=expression(pi(mu)),xlab=expression(mu))

    #Item j
curve(poderBi(n=20,alfa=0.05,x,sigma2=25,mi0=30),10,50,add=TRUE,ylab=expression(pi(mu)),xlab=expression(mu))
curve(poderBi(n=30,alfa=0.05,x,sigma2=25,mi0=30),10,50,add=TRUE,col="red",ylab=expression(pi(mu)),xlab=expression(mu))
curve(poderBi(n=50,alfa=0.05,x,sigma2=25,mi0=30),10,50,add=TRUE,col="green",ylab=expression(pi(mu)),xlab=expression(mu))
curve(poderBi(n=100,alfa=0.05,x,sigma2=25,mi0=30),10,50,add=TRUE,col="blue",ylab=expression(pi(mu)),xlab=expression(mu))
legend(10,0.5,
       c("20", "30", "50", "100"), title="n",
       col=c("black", "red","green","blue"),
       lty=c(1, 1))

#Questao 2###############

    #Item a
TH.media=function(x,alfa,mi0){
  n <- length(x)
  x.barra <- mean(x)
  s <- sd(x)
  #Criando a Regiao Critica
  t <- qt(p=1-alfa,df=n-1)
  t.c <- mi0+t*s/sqrt(n)
  cat("Estimativa Pontual;",s^2,"\n","Regiao Critica;[",t.c,";Inf]")
  #Decidindo
  if (x.barra>t.c){
    cat("\n","Rejeita-se Ho a um nivel de significancia",alfa)
  }
  else{
    cat("\n","Nao rejeita-se Ho a um nivel de significancia",alfa)
  }
}

    #Item b
TH.media2=function(x,alfa,mi0){
  n <- length(x)
  x.barra <- mean(x)
  s <- sd(x)
  z <- (x.barra-mi0)*sqrt(n)/s
  #Criando a Regiao Critica
  z.c <- qnorm(alfa)
  #Decidindo
  if (z>z.c){
    cat("Rejeita-se Ho a um nivel de significancia",alfa)
  }
  else{
    cat("Nao rejeita-se Ho a um nivel de significancia",alfa)
  }
}

    #Item c
ET2=function(n,s,alfa,mi0,mi){
  #Criando a Regiao Critica
  t <- qt(p=1-alfa,df=n-1)
  t.c <- mi0+t*s/sqrt(n)
  #Calculando a probabilidade pedida(estou "padronizando")
  q <- (t.c-mi)*sqrt(n)/s
  prob <- pt(q,df=n-1)
  return(prob)
}

    #Item d
curve(ET2(x,mi0=10,s=1,alfa=0.05,n=20),0,20,ylab=expression(beta(mu)),xlab=expression(mu))

    #Item e
curve(ET2(x,mi0=10,s=1,alfa=0.05,n=20),5,15,add=TRUE,ylab=expression(beta(mu)),xlab=expression(mu))
curve(ET2(x,mi0=10,s=1,alfa=0.05,n=30),5,15,col="red",add=TRUE,ylab=expression(beta(mu)),xlab=expression(mu))
curve(ET2(x,mi0=10,s=1,alfa=0.05,n=50),5,15,col="green",add=TRUE,ylab=expression(beta(mu)),xlab=expression(mu))
curve(ET2(x,mi0=10,s=1,alfa=0.05,n=100),5,15,col="blue",add=TRUE,ylab=expression(beta(mu)),xlab=expression(mu))
legend(6, 0.5,
       c("20", "30", "50", "100"), title="n",
       col=c("black", "red","green","blue"),
       lty=c(1, 1))

    #Item f
curve(ET2(x,mi0=10,s=1,alfa=0.01,n=20),5,15,add=TRUE,ylab=expression(beta(mu)),xlab=expression(mu))
curve(ET2(x,mi0=10,s=1,alfa=0.05,n=20),5,15,col="red",add=TRUE,ylab=expression(beta(mu)),xlab=expression(mu))
curve(ET2(x,mi0=10,s=1,alfa=0.07,n=20),5,15,col="green",add=TRUE,ylab=expression(beta(mu)),xlab=expression(mu))
curve(ET2(x,mi0=10,s=1,alfa=0.1,n=20),5,15,col="blue",add=TRUE,ylab=expression(beta(mu)),xlab=expression(mu))
legend(6, 0.5,
      c("0.01", "0.05", "0.07", "0.1"), title=expression(alfa),
      col=c("black", "red","green","blue"),
      lty=c(1, 1))

    #Item g
poderUni=function(n,s,alfa,mi0,mi){
  z <- qt(df=n-1,p=1-alfa)
  #Definindo a regiao critica
  t.c <- s*z/sqrt(n)+mi0
  #Calcullando a probabilidade pedida
  p <- (t.c-mi)*sqrt(n)/s
  prob <- pt(df=n-1,p,lower.tail=FALSE)
}
curve(poderUni(n=20,s=1,alfa=0.01,mi0=5,mi=x),0,8,ylab=expression(pi(mu)),xlab=expression(mu))

#Questao 3##########
base=readRDS("BaseGenero.rds")

    #Item a
pvalorDir=function(x,alfa,mi0){
  s <- sd(x)
  x.barra <- mean(x)
  n <- length(x)
  cat(x.barra)
  #Calculando o p-valor
  p <- (x.barra-mi0)*sqrt(n)/s
  prob <- pt(df=n-1,p,lower.tail=FALSE)
  cat("O p-valor e:",prob,"\n")
  #Determinando se rejeita ou nao
  if (prob>alfa){
    cat("Nao rejeito a hipotese nula,ou seja,a media dos peso e inferior ou igual a 70Kg")
  }
  else if (alfa>prob){
    cat("Rejeito a hipotese nula,ou seja,a media dos peso e superior a 70Kg")
  }
}
base1 <- base %>% filter(Sexo=="Homem")
pvalorDir(x=base1$Peso,alfa=0.02,mi0=70)

    #Item b
TH.mediaEsq=function(x,alfa,mi0){
  x.barra <-mean(x)
  n <- length(x)
  s <- sd(x)
  q <- qt(df=n-1,alfa)
  cat(q)
  #Achando a Regiao Critica
  t.c <- mi0+q*s/sqrt(n)
  cat("Regiao cirtica=[-inf,",t.c,"]")
  #Determinando se rejeita ou nao
  if (x.barra>t.c){
    cat("\n","Nao rejeito a hipotese nula,ou seja,a media de idade e superior a 30 anos ou mais")
  }
  if (x.barra<t.c){
    cat("\n","Rejeito a hipotese nula,ou seja,a media de idade e inferior a 30 anos")
  }
}
base1 <- base %>% filter(Sexo=="Homem")
TH.mediaEsq(x=base1$Idade,alfa=0.01,mi0=30)

    #Item c
pvalorBi=function(x,alfa,mi0){
  s <- sd(x)
  x.barra <- mean(x)
  n <- length(x)
  #Calculando o p-valor(TEM Q TOMAR CUIDADO POIS NAO SABEMOS "ONDE" O VALOR DE X.BARRA ESTA 
  #ANTES DE COMECAR A FAZER AS CONTAS)
  if (x.barra>mi0){
    p <- (x.barra-mi0)*sqrt(n)/s
    prob <- 2*pt(df=n-1,p,lower.tail=FALSE)
    cat("O p-valor e:",prob,"\n")
  }
  else{
    p <- (x.barra-mi0)*sqrt(n)/s
    prob <- 2*pt(df=n-1,p)
    cat("O p-valor e:",prob,"\n")
  }
  #Determinando se rejeita ou nao
  if (prob>alfa){
    cat("Nao rejeito a hipotese nula,ou seja,a media dos peso e inferior ou igual a 70Kg")
  }
  else if (alfa>prob){
    cat("Rejeito a hipotese nula,ou seja,a media dos peso e superior a 70Kg")
  }
}
base1<-filter(base,base$Sexo=="Mulher") 
pvalorBi(x=base1$Idade,alfa=0.03,mi0=20)

    #Item d
TH.prop2=function(x,alfa,p0,h){
  #Determinando a proporcao observada
  y <- ifelse(x>h,1,0)
  p1 <- mean(y)
  n <- length(y)
  #Achando a Regiao Critica
  p.c <- qnorm(p=1-alfa,mean=p0,sd=sqrt(p0*(1-p0))/sqrt(n))
  #Determinando se rejeita ou nao 
  cat("Valor observado:",p1,"\n","Regiao cirtica=[",p.c,":1]","\n")
  if (p1>p.c){
    cat("Rejeito a hipotese nula,ou seja,a media dos peso e superior a 70Kg")
  }
  else if (p1<p.c){
    cat("Nao rejeito a hipotese nula,ou seja,a media dos peso e inferior ou igual a 70Kg")
  }
}
base1 <- base %>% filter(Sexo=="Mulher")
TH.prop2(x=base1$Peso,alfa=0.1,p0=0.65,h=70)

    #Item d(outra ideia para resolver a questao)
TH.prop3=function(x,alfa,p0){
  #Determinando a proporcao observada
  for (i in 1:length(x)){
    x[i] <- ifelse(x[i]>h,1,0)
  }
  print(x)
  p1 <- mean(x)
  n <- length(x)
  q <- qnorm(p=1-alfa)
  #Achando a Regiao Critica
  p.c <- p0+q*sqrt(p0*(1-p0))/sqrt(n)
  #Determinando se rejeita ou nao 
  cat("Valor observado:",p1,"\n","Regiao cirtica=[",p.c,":1]","\n")
  if (p1>p.c){
    cat("Rejeito a hipotese nula
        ,ou seja,a media dos peso e superior a 70Kg")
  }
  else if (p1<p.c){
    cat("Nao rejeito a hipotese nula,
        ou seja,a media dos peso e inferior ou igual a 70Kg")
  }
}
base1 <- base %>% filter(Sexo=="Mulher")
TH.prop3(x=base1$Peso,alfa=0.05,p0=0.65,h=70)

    #Usando p-valor
pvalor=function(x,alfa,h,p0){
  y <- ifelse(x>h,1,0)
  p1 <- mean(y)
  n <- length(y)
  #Calculando o p-valor
  pv <- pnorm(q=p1,mean=p0,sd=sqrt(p0*(1-p0)/n),lower.tail=FALSE)
  cat("p-valor e:",pv,"\n")
  if (alfa>pv){
    cat("Rejeitamos Ho, para im nivel de significancia:",alfa)
  }
  else if (alfa<pv){
    cat("Nao rejeitamos Ho, para im nivel de significancia:",alfa)
  }
}
pvalor(x=base1$Peso,p0=0.65,h=70,alfa=0.05)

#Questao 4######

    #item a
base1 <- base %>% filter(Sexo=="Homem")
t.test(base1$Peso, alternative = "greater", mu = 70,conf.level=0.95)
#Como alfa e maior que p-valor rejeitamos Ho,ou seja, ha evidencias que a media dos pesos e maior que 70Kg

    #Item b
base1 <- base %>% filter(Sexo=="Homem")
t.test(base1$Idade, alternative = "less", mu = 30,conf.level=0.99)
#Como alfa e maior que p-valor rejeitamos Ho,ou seja, ha evidencias que a media das idades e menor que 30 anos

    #Item c
base2 <- base %>% filter(Sexo=="Mulher")
t.test(base2$Idade,alternative="two.sided",conf.level=0.97,mu=20)
#Como alfa e maior que p-valor rejeitamos Ho,ou seja, ha evidencias que a media das idades e diferente que 20 anos

    #Item d
base1 <- base %>% filter(Sexo=="Mulher")
proporcao <- ifelse(base1$Peso>70,1,0)
prop.test(sum(proporcao), length(proporcao), alternative = "greater", p = 0.65, correct = FALSE,conf.level=0.95)

#Questao 5#########
ET2prop=function(x,alfa,p0,p){
  n <- length(x)
  #Determinando a Regiao Critica
  z <- qnorm(1-alfa)
  p.c <- p0+sqrt(p0*(1-p0))*z/sqrt(n)
  #Determinando a prob do erro tipo II
  p <- (p.c-p)*sqrt(n)/sqrt(p*(1-p))
  prob <- pnorm(p)
  cat("Probabilidade de erro tipo II e;",prob)
}
a <- rnorm(20)
ET2prop(a,0.05,0.5,0.5)

#Questao 6########
PoderBi=function(x,alfa,p0,p){
  n <- length(x)
  #Determinando a Regiao Critica
  z1 <- qnorm(1-alfa/2)
  z2 <- qnorm(alfa/2)
  p <- ifelse(p>=0&p<=1,p,1)
  p.c1 <-p0+sqrt(p0*(1-p0)/n)*z1
  p.c2 <-p0+sqrt(p0*(1-p0)/n)*z2
  #Achando a funcao poder
  p1 <- pnorm(p.c2,mean=p,sd=sqrt(p*(1-p)/n))
  p2 <- pnorm(p.c1,mean=p,sd=sqrt(p*(1-p)/n),lower.tail=F)
  prob <- p1+p2
  return(prob)
}
x <- rnorm(10,0.5,0.3)
PoderBi(x,alfa=0.01,p0=0.6,p=3)
PoderBi(x,alfa=0.01,p0=0.6,p=0.6)
curve(PoderBi(x,alfa=0.01,p0=0.6,x),-0.5,1.5
      ,ylab=expression(beta(p)),xlab=expression(p))
