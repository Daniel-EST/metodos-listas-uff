# Pacotes ######################################################################
require(purrr)
require(dplyr)
require(readr)
require(ggplot2)
require(gridExtra)
require(sjPlot)
# Questao 1 ####################################################################
# Importacao
tratamento <- readxl::read_xls('Listas/Lista 02/Tratamento.xls', na = '999') ; head(tratamento)
#a
tratamento$Grupo <- tratamento$Grupo %>% 
  factor(labels = c('Adesao','Nao Adesao'))
tratamento$`Situação Atual no Trabalho` <- tratamento$`Situação Atual no Trabalho` %>% 
  factor(labels = c('Empregado', 'Desempregado'))
tratamento$`Frequenta algum grupo religioso` <- tratamento$`Frequenta algum grupo religioso` %>% 
  factor(labels = c('Nao','Sim'))
tratamento$escolaridade <- tratamento$escolaridade %>% 
  ordered(labels = (c('Analfabeto','Ensino Fundamental','Ensino Medio','Ensino Superior')))
#b
tratamento$faixa.etaria <- rep(NA,length(tratamento$Idade))
{
  for(i in 1:length(tratamento$Idade)){
    if(!(is.na(tratamento$Idade[i]))&&tratamento$Idade<18){
      tratamento$faixa.etaria[i] <- 0
    }
    if(!(is.na(tratamento$Idade[i]))&&tratamento$Idade>=18&&tratamento$Idade<60){
      tratamento$faixa.etaria[i] <- 1
    }
    if(!(is.na(tratamento$Idade[i]))&&tratamento$Idade>=60){
      tratamento$faixa.etaria[i] <- 2
    }
  }
  rm('i')
}
tratamento$faixa.etaria <- tratamento$faixa.etaria %>% 
  factor(labels = c('Adulto'))
#c
class(tratamento)
#d
summary(tratamento)
#e
readr::write_delim(tratamento, 'Listas/Lista 02/tratamento.txt')
readr::write_csv(tratamento, 'Listas/Lista 02/tratamento.csv')
saveRDS(tratamento, 'Listas/Lista 02/tratamento.RDS')
{
  for(i in 3:6){
    mtam <- file.size(dir()[3])
    if(file.size(dir()[i])<=mtam){
      menor <- dir()[i]
      mtam <- file.size(dir()[i])
    }
  }
  rm(ls='i')
}
# E um formato compactado apaenar para o R
# Questao 2 ####################################################################
# Importacao
tratamentoRDS <- readRDS('Listas/Lista 02/tratamento.RDS')

#a
tratamentoQ2a<- tratamentoRDS %>% 
  select(Idade,ansiedade,6)
write_delim(tratamentoQ2a,'Listas/Lista 02/tratamentoQ2a.txt')
#b
tratamentoQ2b <- tratamentoRDS %>%
  filter(escolaridade=='Ensino Fundamental'&Idade>50&`Frequenta algum grupo religioso`=='Nao')
write_csv(tratamentoQ2b, 'Listas/Lista 02/tratamentoQ2b.csv')
#c
tratamentoRDS <- tratamentoRDS %>% 
  arrange(Idade)
#d
tratamentoRDS <- tratamentoRDS %>% 
  rename(pont.ansiedade = ansiedade)
#e
tratamentoRDS$pont.total <- tratamentoRDS$pont.ansiedade + tratamentoRDS$depressão
#f
tratamentoRDS %>% 
  group_by(Grupo,`Situação Atual no Trabalho`) %>% 
    summarise(media.ansiedade = mean(pont.ansiedade,na.rm = T), media.depressao = mean(depressão,na.rm = T))
#g
tratamentoQ2g <- tratamentoRDS %>% 
  group_by(Grupo,escolaridade,`Frequenta algum grupo religioso`) %>% 
    summarise(max.pont.tot = max(pont.total, na.rm = T),mediana.pont.tot = median(pont.total,na.rm = T), Cincoquant.pont.tot = quantile(pont.total, 5/10, na.rm = T))

# Questao 3 ####################################################################
#a
grafQ3a <-  ggplot(tratamentoRDS, aes(x=Grupo)) +
  geom_bar() ;grafQ3a
#b
grafQ3b <-  ggplot(tratamentoRDS, aes(x = `Situação Atual no Trabalho`)) +
  geom_bar() ; grafQ3b
#c
grafQ3c <- ggplot(tratamentoRDS, aes(x = 'Total', y = Idade)) + 
  geom_boxplot() ; grafQ3c
#d
grafQ3d <- ggplot(tratamentoRDS, aes(x = `Frequenta algum grupo religioso`, y =depressão)) + 
  geom_boxplot() ; grafQ3d
#e
grafQ3e <- ggplot(tratamentoRDS, aes(x = Idade)) +
  geom_histogram(breaks = c(seq(60,100,5))) ; grafQ3e
#f
par(mfrow = c(2,2))
hist(tratamentoRDS$Idade,probability = T)
curve(dnorm(x,mean(tratamentoRDS$Idade), sqrt(var(tratamentoRDS$Idade))),add= T)
# Escolaridade e uma variavel qualitativa nominal
#hist(tratamentoRDS$escolaridade,probability = T)
#curve(dnorm(x,mean(tratamentoRDS$escolaridade), sqrt(var(tratamentoRDS$escolaridade))),add= T)
hist(tratamentoRDS$depressão,probability = T)
curve(dnorm(x,mean(tratamentoRDS$depressão, na.rm = T), sqrt(var(tratamentoRDS$depressão, na.rm = T))),add= T)
hist(tratamentoRDS$pont.ansiedade,probability = T)
curve(dnorm(x,mean(tratamentoRDS$pont.ansiedade,na.rm = T), sqrt(var(tratamentoRDS$pont.ansiedade, na.rm = T))),add= T)
par(mfrow = c(1,1))
#g
prop.table(table(tratamentoRDS$`Situação Atual no Trabalho`,tratamentoRDS$Grupo),2)
#h
gridExtra::grid.arrange(grafQ3a, grafQ3b, grafQ3c, nrow = 2) #Esse pacote ainda não está disponível para a versão 3.5.1 do R

# Questao 4 ####################################################################
summary(tratamentoRDS$depressão)
summary(tratamentoRDS$pont.ansiedade)
par(mfrow = c(2,1))
boxplot(tratamentoRDS$depressão)
boxplot(tratamentoRDS$pont.ansiedade)
par(mfrow = c(1,1))

# Questao 5 ####################################################################
# Questao 6 ####################################################################
#a
fa <- function(x,l) return(l * exp(-l*x))
#b
fb <- function(x){
  if(x>5||x< (-5)){
    stop('apenas calculado no interfvalo (-5,5)')
  } else {
    return(x**2)
  }
}
#c
fc <- function(x){
  if(x>2 && x<18){
    return((1/10)*exp(-x))
  } else {
    stop('apenas calculado no intervalo (2,18)')
  }
}
#d
gamaDist <- function(alfa,beta,x){
  (1/gamma(alfa)*beta^alfa)*x**(alfa-1)*exp(-x/beta)
}
#e
# Preguica denovo
# Questao 7 ####################################################################
curve(fa(x,1),-5,5)
curve(fb(x),-4.9,4.9)
curve(fc(x),2.1,17.9)
curve(gamaDist(1,1,x),-5,5)
# Questao 8 ####################################################################
sqde <- function(x){
  soma <- 0
  n <- length(x)
  for(i in 1:n){
    temp <- (x[i] - mean(x))
    soma <- soma + temp
  }
  return(sqrt((1/(n-1))*soma))
}
# Questao 9 ####################################################################
IMC <- function(altura,peso){
  return(altura/peso**2)
}
# Questao 10 ###################################################################
IMCclass <- function(x){
  if(IMC(x)<=18.5){
    return('Desnutrido')
  }
  if(IMC(x)>18.5&&IMC(x)<25){
    return('Normal')
  }
  if(IMC(x)>=25){
    return('Obeso')
  }
}
# Questao 11 ###################################################################
#a
{
  soma <- 0
  a <- 11
  for(i in seq(20,20*99,20)){
      temp <- 1/(12 + i)
      soma <- soma + temp
  }
  1 + soma
}
#b
{
  soma <- 0
  b <- 0
  a <- 10
  for(i in 1:99){
    temp <- -1/(11 + b)
    soma <- soma + temp
    temp <- -temp
    b <- b + 10
  }
  1 + soma
}
#c
{
  soma <- 0
  for(i in 1:100){
    temp <- i/15
    soma <- soma + temp
  }
  soma
}
#d
{
  soma <- 0
  for(i in c(1,seq(10,(10*100)-1,10))){
    temp <- i/10
    soma <- soma + temp
  }
  soma
}
#e
{
  soma <- 0
  for(i in 1:100){
    temp <- log(i)
    soma <- soma + temp
  }
  soma
}
# Questao 13 ###################################################################
#a
integrate(function(x)return(x^3),0,10)[1][[1]]
#b
q13b <- function(x)return((3/5)*(x**3+x))
expression(q13b,x)
integrate(q13b,0,5)[1][[1]] + integrate(q13b,7,10)[1][[1]] +
  integrate(q13b,11,15)[1][[1]]
#c
integrate(function(x)return(x**12*(1-x)**8),0,1)[1][[1]]
#d
integrate(function(x)return(3*exp(-3*x)),0,100)[1][[1]]
# Questao 14 ###################################################################
#a
D(expression(x**3),"x")
#b
#c
#d

# Questao 15 ###################################################################
#a
#b