#PACOTES
library(dplyr)
library(tibble)
library(foreign)
setwd("/Volumes/MIZUNO/Listas/Lista 4/HTML")

#QUESTAO 1########

    #Item a
IC.media1=function(x,conf,sigma){
  n<- length(x)
  x.barra <- mean(x)
  alfa <- 1-conf
  z <- qnorm(1-alfa)
  LI <- x.barra-z*sigma/sqrt(n)
  cat("IC(m,",100*conf,"%)=[",LI,";inf]")
}

    #Item b
IC.media2=function(x,conf,sigma){
  n<- length(x)
  x.barra <- mean(x)
  alfa <- 1-conf
  z <- qnorm(1-alfa)
  LS <- x.barra-z*sigma/sqrt(n)
  cat("IC(m,",100*conf,"%)=[",LI,";inf]")
}

    #Item c
IC.media3=function(x,conf,sigma){
  n<- length(x)
  x.barra <- mean(x)
  alfa <- 1-conf
  z1 <- qnorm(1-alfa/2)
  z2 <- qnorm(alfa/2)
  LS <- x.barra-z2*sigma/sqrt(n)
  LI <- x.barra-z1*sigma/sqrt(n)
  cat("IC(m,",100*conf,"%)=[",LI,";",LS,"]")
}

    #Item d
IC.grupo=function(n,q,conf,miv,sigma){
  mat.amostras = matrix(NA, ncol=q, nrow=n)
  mat=tibble(LS=c(1:q),LI=c(1:q),Pertence=c(1:q))
  for(i in 1:q){
    mat.amostras[,i] = rnorm(n,miv,sigma)
  }
  media <- apply(mat.amostras,2,mean)
  alfa <- 1-conf
  z1 <- qnorm(1-alfa/2)
  z2 <- qnorm(alfa/2)
  for (i in 1:q){
    #limite superior
    mat[i,1] <- media[i]-z2*sigma/sqrt(n)
    #limite inferior
    mat[i,2]  <- media[i]-z1*sigma/sqrt(n)
    if (mat[i,1]>=miv & mat[i,2]< miv){
      mat[i,3]=1
    }
    else{
      mat[i,3]=0
    }
  }
  print(apply(mat,2,mean)["Pertence"])
}
IC.grupo(n=25,q=1000,miv=30,conf=0.85,sigma=5)
#OUTRA IDEIA PARA QUESTAO
IC.grupo=function(n,q,conf,miv,sigma){
  mat.amostras = matrix(NA, ncol=q, nrow=n)
  x <- matrix(1:n,ncol=25,nrow=1)
  for(i in 1:n){
    mat.amostras[,i] = rnorm(n,miv,sigma)
  }
  media <- apply(mat.amostras,2,mean)
  for (i in 1:n){
    alfa <- 1-conf
    z1 <- qnorm(1-alfa/2)
    z2 <- qnorm(alfa/2)
    LS <- media[i]-z2*sigma/sqrt(n)
    LI <- media[i]-z1*sigma/sqrt(n)
    x[,i] <- ifelse(miv<=LS & miv>=LI,1,0)
  }
  print(mean(x))
}
IC.grupo(n=25,q=1000,miv=30,conf=0.85,sigma=5)

#QUESTAO 2#######
IC.prop=function(x,conf){
  n <- length(na.omit(x))
  alfa <- 1-conf
  p0 <- mean(x,na.rm = TRUE)
  z1 <- qnorm(1-alfa/2)
  z2 <- qnorm(alfa/2)
  LI <- p0-z1*sqrt(.5*.5)/sqrt(n)
  LS <- p0-z2*sqrt(.5*.5)/sqrt(n)
  cat("IC(m,",100*conf,"%)=[",LI,";",LS,"]")
}
a <- rbinom(10,1,1/2)
IC.prop(rbinom(10,1,1/2),0.9)

#QUESTAO 3###########
base=read.table("Base saude.txt",na.strings=9,dec=",",header=TRUE)
base$Sexo <- factor(base$Sexo,label=c("Feminino","Masculino"))
base$HIV <- factor(base$HIV,label=c("Nao","Sim"))
base$DST <- factor(base$DST,label=c("Nao","Sim"))
#Funcao o item c
IC.media3=function(x,conf,sigma){
  n<- length(na.omit(x))
  x.barra <- mean(x,na.rm=TRUE)
  s <- sigma
  alfa <- 1-conf
  z1 <- qt(p=1-alfa/2,df=n-1)
  z2 <- qt(p=alfa/2,df=n-1)
  LS <- x.barra-z2*s/sqrt(n)
  LI <- x.barra-z1*s/sqrt(n)
  cat("IC(m,",100*conf,"%)=[",LI,";",LS,"]")
}

    #Item a
IC.media3(x=base$Peso,conf=0.97,sigma=10)

    #Item b
base1 <- base %>% filter(HIV=="Sim")
IC.media3(x=base1$Estatura,conf=0.95)

    #Item d
base2 <- base %>% filter(HIV=="Nao",Sexo=="Feminino")
IC.media3(x=base2$Idade,conf=0.9)

#QUESTAO 4###########
IC.grupo <- function(x,y,conf){
  va1<- levels(y)[1]
  va2 <- levels(y)[2]
  x1 <- x[y==va1]
  x2 <- x[y==va2]
  alfa <- 1-conf
  IC.media=function(x,conf){
    n<- length(na.omit(x))
    x.barra <- mean(x,na.rm=TRUE)
    s <- sd(x,na.rm=TRUE)
    t1 <- qt(1-alfa/2,df=n-1)
    t2 <- qt(alfa/2,df=n-1)
    print(t1,t2)
    LS <- x.barra-t2*s/sqrt(n)
    LI <- x.barra-t1*s/sqrt(n)
    cat("IC(m,",100*conf,"%)=[",LI,";",LS,"]")
  }
  IC.media(x1,conf)
  IC.media(x2,conf)
}

#Questao 4.2#############
IC.grupo2 <- function(x,y,conf){
  alfa<- 1-conf
  for (i in 1:length(levels(y))){
    va <- levels(y)[i]
    a <- x[y==va]
    n<- length(na.omit(a))
    x.barra <- mean(a,na.rm=TRUE)
    s <- sd(a,na.rm=TRUE)
    t1 <- qt(1-alfa/2,df=n-1)
    t2 <- qt(alfa/2,df=n-1)
    LS <- x.barra-t2*s/sqrt(n)
    LI <- x.barra-t1*s/sqrt(n)
    cat(va,"IC(m,",100*conf,"%)=[",LI,";",LS,"]","\n")
  }
}

#QUESTAO 5###########
IC.grupo(x=base$Estatura,y=base$Sexo,conf=0.95)
IC.grupo2(x=base$Estatura,y=base$Sexo,conf=0.95)

#QUESTAO 6##############
IC.var=function(x,conf){
  n<- length(na.omit(x))
  va <- var(x,na.rm = TRUE)
  alfa <- 1-conf
  z1 <- qchisq(1-alfa/2,df=n-1)
  z2 <- qchisq(alfa/2,df=n-1)
  LS <- (n-1)*va/z2
  LI <- (n-1)*va/z1
  cat("IC(var,",100*conf,"%)=[",LI,";",LS,"]")
}

    #Item a
basea=base %>% filter(Sexo=="Masculino",HIV=="Sim")
IC.var(x=basea$Idade,conf=0.9)

    #Item b
IC.var(x=base$Estatura,conf=0.95)

    #Item c
baseb=base %>% filter(Escol==0)
IC.var(x=baseb$Peso,conf=0.97)

#QUESTAO 7##############
base <- readRDS("exames medicos.rds")

    #Item a
#HDL
qqnorm(base$LDL)
qqline(base$LDL)
#PARECE TER DIST NORMAL COM VAR DESCONHECIDA

#LDL
qqnorm(base$HDL)
qqline(base$HDL)
#NAO PARECE TER DIST NORMAL

#GLICOSE
qqnorm(base$glicose)
qqline(base$glicose)
#PARECE TER DIST NORMAL COM VAR DESCONHECIDA

#LINFOCITOS
qqnorm(base$linfocitos)
qqline(base$linfocitos)
#NAO PARECE TER DIST NORMAL

    #Item b
IC.media=function(x,conf){
  n<- length(x)
  x.barra <- mean(x)
  des <- sd(x)
  alfa <- 1-conf
  z <- qt(df=n-1,alfa/2)
  LI <- x.barra+z*des/sqrt(n)
  LS <- x.barra-z*des/sqrt(n)
  cat("IC(m,",100*conf,"%)=[",LI,";",LS,"]")
}
IC.media(x=base$LDL,conf=0.98)
IC.media(x=base$glicose,conf=0.98)

    #Item d
IC.var=function(x,conf){
  n<- length(x)
  x.barra <- mean(x)
  va <- var(x)
  alfa <- 1-conf
  z1 <- qchisq(df=n-1,1-alfa/2)
  z2 <- qchisq(df=n-1,alfa/2)
  LI<- (n-1)*va/z1
  LS <- (n-1)*va/z2
  cat("IC(var,",100*conf,"%)=[",LI,";",LS,"]")
}
IC.var(x=base$LDL,conf=0.95)
IC.var(x=base$glicose,conf=0.95)

    #Item e
IC.prop=function(x,conf){
  t <- length(na.omit(filter(base,glicose>45)[,3]))
  n <- length(na.omit(base[,3]))
  p <- t/n
  alfa <- 1-conf
  z2 <- qnorm(1-alfa/2)
  z1 <- qnorm(alfa/2)
  LI <- p-sqrt(p*(1-p)/n)*z2
  LS <- p-sqrt(p*(1-p)/n)*z1
  cat("IC(prop,",100*conf,"%)=[",LI,";",LS,"]")
}
IC.prop(base$glicose,0.95)

#QUESTAO 8##############
base1 <- readRDS("colesterol.rds")

    #Item a
#HDL
qqnorm(base1$LDL)
qqline(base1$LDL)
#PARECE TER DIST NORMAL COM VAR DESCONHECIDA
#LDL
qqnorm(base1$HDL)
qqline(base1$HDL)
#NAO PARECE TER DIST NORMAL

    #Item b
#HDL
IC.media=function(x,conf){
  n <- length(x)
  x.barra <- mean(x)
  alfa <- 1-conf
  t1 <- qt(df=n-1,1-alfa/2)
  t2<- qt(df=n-1,alfa/2)
  s <- sd(x)
  LS <- x.barra+s*t2/sqrt(n)
  LI <- x.barra+s*t1/sqrt(n)
  cat("IC(m,",100*conf,"%)=[",LI,";",LS,"]")
}
IC.media(base1$HDL,0.95)

#LDL (Supondo que o tamanho da amostra e grande)
IC.media2=function(x,conf){
  n <- length(x)
  x.barra <- mean(x)
  alfa <- 1-conf
  t1 <- qt(df=n-1,1-alfa/2)
  t2<- qt(df=n-1,alfa/2)
  s <- sd(x)
  LS <- x.barra+s*t2/sqrt(n)
  LI <- x.barra+s*t1/sqrt(n)
  cat("IC(m,",100*conf,"%)=[",LI,";",LS,"]")
}
IC.media(base1$LDL,0.95)

    #Item d
#HDL
IC.var=function(x,conf){
  n <- length(x)
  s <- var(x)
  alfa <- 1-conf
  c1 <- qchisq(df=n-1,alfa/2)
  c2 <- qchisq(df=n-1,1-alfa/2)
  LI <- (n-1)*s/c1
  LS <- (n-1)*s/c2
  cat("IC(var,",100*conf,"%)=[",LI,";",LS,"]")
}
IC.var(base1$HDL,0.95)

#LDL (Supondo que o tamanho da amostra e grande)
IC.var2=function(x,conf){
  n <- length(x)
  s <- var(x)
  alfa <- 1-conf
  c1 <- qchisq(df=n-1,alfa/2)
  c2 <- qchisq(df=n-1,1-alfa/2)
  LI <- (n-1)*s/c1
  LS <- (n-1)*s/c2
  cat("IC(var,",100*conf,"%)=[",LI,";",LS,"]")
}
IC.var(base1$LDL,0.95)
