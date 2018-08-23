# Lista 04
# Pacotes ######################################################################
require(readr)
# Questao 1 ####################################################################
# a)
IC.media.esq <- function(x,conf,sigma){
  n <- length(x)
  x.barra <- mean(x)
  alpha <- 1-conf
  z <- qnorm(1-alpha)
  LI <- x.barra-z*sigma/sqrt(n)
  LS <- "Inf"
  cat("IC(m,",100*conf,"%)=[",LI,";",LS,")")
}
IC.media.esq(rnorm(10),0.9,5)
# b)
IC.media.dir <- function(x,conf,sigma){
  n <- length(x)
  x.barra <- mean(x)
  alpha <- 1-conf
  z <- qnorm(1-alpha)
  LI <- '-Inf'
  LS <- x.barra+z*sigma/sqrt(n)
  cat("IC(m,",100*conf,"%)=(",LI,";",LS,"]")
}
IC.media.dir(rnorm(10),0.9,5)
# c)
IC.media <- function(x,conf,sigma){
  n <- length(x)
  x.barra <- mean(x)
  alpha <- 1-conf
  z <- qnorm(1-alpha/2)
  LI <- x.barra-z*sigma/sqrt(n)
  LS <- x.barra+z*sigma/sqrt(n)
  cat("IC(m,",100*conf,"%)=[",LI,";",LS,"]")
}
IC.media(rnorm(10),0.9,5)
# d)
IC.media.1=function(x,sigma,conf,mi){
  n<- length(x)
  x.barra <- mean(x)
  alfa <- 1 - conf
  z1 <- qnorm(1-alfa/2)
  z2 <- qnorm(alfa/2)
  LS <- x.barra-z2*sigma/sqrt(n)
  LI <- x.barra-z1*sigma/sqrt(n)
  pertence <- ifelse(mi< LS & mi> LI, 1,0)
  return(c(LimiteS = LS, LimiteI = LI, Media = x.barra, Pertence = pertence))
}
#Item d
mat.amostras = matrix(NA, ncol=1000, nrow=25)
for(i in 1:1000){
  mat.amostras[,i] = rnorm(25,30,5)
}
IC.amostral=apply(mat.amostras,2,IC.media.1,sigma = 5, conf = 0.9, mi = 30)
mean(IC.amostral['Pertence',])
rm('i')
# Questao 2 ####################################################################
IC.prop.conservative <- function(x,conf){
  n <- length(na.omit(x))
  p.chap <- mean(x, na.rm = TRUE)
  alpha <- 1 - conf
  erro <- sqrt((.5*.5)/n)*qnorm(1-alpha/2)
  LI <- p.chap - erro
  LS <- p.chap + erro
  cat("IC(p,",100*conf,"%)=[",LI,";",LS,"]")
}
IC.prop.conservative(rbinom(10,1,1/2),0.9)
# Questao 3 ####################################################################
baseSaude <- read_table2('Listas\\Lista 04\\Base-saude.txt',na = '9') ; head(baseSaude)