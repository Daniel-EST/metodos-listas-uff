#PACOTES
library(readxl)
library(foreign)
library(tibble)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
getwd()
setwd("F:/Listas/Lista 2")
base=read_excel("Tratamento.xls",na=c("999","3"))
base2=readRDS("Tratamento3.rds")
##

#Questão 1############
    
    #Item a
base$Grupo=factor(base$Grupo,labels=c("Adesão","Não Adesão"))
base$`Situação Atual no Trabalho`=factor(base$`Situação Atual no Trabalho`,labels = c("Empregado","Desempregado"))
base$`Frequenta algum grupo religioso`=factor(base$`Frequenta algum grupo religioso`,labels=c("Não","Sim"))
base$escolaridade=factor(base$escolaridade,labels=c("Analfabeto","Ensino Fundamento","Ensino Medio"))

    #Item b
a=tibble(faixa=NA)
base2=cbind(base,a)
base2$faixa[base$Idade<18 & base$Idade>=0]="Jovem"
base2$faixa[base$Idade<60 & base$Idade>=18]="Adulto"
base2$faixa[base$Idade<=150 & base$Idade>=60]="3ª Idade"

    #Item b(Usando função Cut)
base$Faixa=cut(base$Idade,breaks=c(-1,18,59,150),labels=c("Jovem","Adulto","3ª Idade"))

    #Item c
class(base)

    #Item d
summary(base2)

    #Item  e(TROCAR BASE PARA BASE2 SE DER ERRADO)
write.table(base,"Tratamento1.txt")
write.csv(base,"Tratamento2.csv")
saveRDS(base,"Tratamento3.rds")
tamanho=c(file.size("Tratamento1.txt"),file.size("Tratamento1.txt"),file.size("Tratamento3.rds"))
tamanho

#Questão 2#################

    #Item a(USSANDO SELECT)
base4=select(base2,3,5,6)

    #Item a(CRIANDO UM PROGRAMA PARA ACHAR AS VARIAVEIS QUANTITATIVAS)
base3=tibble()
w=c()
j=1
base2=as.data.frame(base2)
for (i in 1:dim(base2)[2]){
  if (class(base2[1,i])=="numeric"){
    w[j]=i
    j=j+1
  }
}
base3=base2 %>% select(w)
write.table(base3,"Tratamento Quantitativa.txt")

    #Item b
nova=filter(base2,escolaridade=="Ensino Fundamento"&Idade>=50&`Frequenta algum grupo religioso`=="Não")
write.csv(nova,"nova.csv")
    
    #Item c(PODEIRA TER USADO SOMENTE A FUN??O ARRANGE)
base3=select(base,Idade,everything())%>%arrange(Idade)

    #Item d
base4=rename(base2,pont.ansiedade = ansiedade)
    
    #Item e
base3=group_by(base2,ansiedade,depressão)%>%summarise(pont.total=sum(ansiedade+depressão,na.rm=TRUE))
base4=inner_join(base2,base3,by=c("ansiedade","depressão"))
    
    #Item f
mediaG=base2%>%group_by(Grupo)%>%summarise(media.ansiedade=mean(ansiedade,na.rm=TRUE),
                                                       media.depressão=mean(depressão,na.rm=TRUE))
mediaS=base2%>%group_by(`Situação Atual no Trabalho`)%>%summarise(media.ansiedade=mean(ansiedade,na.rm=TRUE),
                                           media.depressão=mean(depressão,na.rm=TRUE))
    
    #Item g
base5=base4%>%group_by(Grupo)%>%summarise(maximo=max(pont.total,na.rm = TRUE),mediana=median(pont.total,na.rm=TRUE)
                                          ,decil=quantile(pont.total,c(.5)))
base5=base4%>%group_by(escolaridade)%>%summarise(maximo=max(pont.total,na.rm = TRUE),mediana=median(pont.total,na.rm=TRUE)
                                          ,decil=quantile(pont.total,c(.5)))
base5=base4%>%group_by(`Frequenta algum grupo religioso`)%>%summarise(maximo=max(pont.total,na.rm = TRUE),mediana=median(pont.total,na.rm=TRUE)
                                          ,decil=quantile(pont.total,c(.5)))

#Questão 3(FEITO USANDO O GGPLOT)#############

    #Item a
graf1=ggplot(base2,aes(x=Grupo))+geom_bar(aes(fill=Grupo))+ylab("Quantidae")
graf1

    #Item b
graf2=ggplot(base2,aes(x=`Situação Atual no Trabalho`))+geom_bar()+ylab("Qauntidade")
graf2

    #Item c
graf3=ggplot(base2,aes(y=Idade,x=Grupo))+geom_boxplot(color = "red",fill="yellow",na.rm=TRUE)+ylab("Idade em anos")
graf3

    #Item d
base3=filter(base2,!is.na(`Frequenta algum grupo religioso`)&!is.na(depressão)&!is.na(ansiedade))
graf1=ggplot(base3,aes(x=`Frequenta algum grupo religioso`,y=depressão))+geom_boxplot(color = "red",fill="yellow",na.rm=TRUE)
graf=ggplot(base3,aes(x=`Frequenta algum grupo religioso`,y=ansiedade))+geom_boxplot(color = "red",fill="yellow",na.rm=TRUE)
grid.arrange(graf1,graf)

    #Item e (BBINWIDHT MEXE NA LARGURA "DAS CAIXAS")
graf=ggplot(base2,aes(x=Idade))+geom_histogram(color = "red",binwidth = 2)
graf

    #Item f(USANDO HIST+CURVE)
hist(base2$Idade,probability = T,xlab="Idade",ylab="Densidade",main="Histograma da Idade")
curve(dnorm(x,mean(base2$Idade),sd(base2$Idade)),add=T)

hist(base2$depressão,probability = T,ylab="Densidade",xlab="Nivel de Depressão",main="Histograma da Depressão")
curve(dnorm(x,mean(base2$depressão,na.rm=T),sd(base2$depressão,na.rm=T)),add=T)

hist(base2$ansiedade,probability = T,ylab="Densidade",xlab="Nivel de Ansiedade",main="Histograma da Ansiedade")
curve(dnorm(x,mean(base2$ansiedade,na.rm=T),sd(base2$ansiedade,na.rm=T)),add=T)

    #Item f(USANDO QQ-PLOT)
base=sort(base2$Idade)
qqnorm(base)
qqline(base,col="green")

base=sort(base2$depressão)
qqnorm(base)
qqline(base,col="blue4")

base=sort(base2$ansiedade)
qqnorm(base)
qqline(base,col="red")

    #Item g
graf=sjt.xtab(base2$Grupo,base2$`Situação Atual no Trabalho`,var.labels = c("Grupo","Trabalho")
              ,show.col.prc = TRUE, show.summary = FALSE)
graf1=ggplot(base2,aes(x=Grupo))+geom_bar(aes(fill=`Situação Atual no Trabalho`))+ylab("Frequencia")
graf1

    #Item h
grid.arrange(graf1, graf2, graf3,ncol=1,nrow=3)

#Questão 4######

table(base2$`Frequenta algum grupo religioso`)
prop.table(table(base2$`Frequenta algum grupo religioso`))
graf=base2%>%filter(!is.na(`Frequenta algum grupo religioso`))%>%ggplot(aes(x=`Frequenta algum grupo religioso`))+geom_bar(fill="red")

#Questão 5###########

    #Item a1(USANDO A FUNÇÃO CUT)
base2$catidade=cut(base2$Idade,breaks = c(0,69,90,Inf),labels=c(1,2,3))

    #Item a2
catidade=tibble(catidade=NA)
base3=cbind(base2,catidade)
base3$catidade[base2$Idade<70]=1
base3$catidade[base2$Idade>=70&base2$Idade<=90]=2
base3$catidade[base2$Idade>90]=3


    #Item b1
base2$catdepre=cut(base2$depressão,breaks=c(-1,9,19,29),labels=c(1,2,3))

    #Item b2
catdepre=tibble(catdepre=NA)
base3=cbind(base3,catdepre)
base3$catdepre[base2$depressão<10&base2$depressão>=0]=1
base3$catdepre[base2$depressão<20&base2$depressão>=10]=2
base3$catdepre[base2$depressão<30&base2$depressão>=20]=3

 
#Questão 6##################
    #Item a
f1=function(y,x){
  y*exp(-x*y)*I(x>0)
}

    #Item b
f2=function(x){
  x^2*I(-5<x & x<5)
}

    #Item c1
f3=function(x){
  ((1/10)*exp(-x)-10)*I(x>2 & x<18)
}

    #Item c2(TEM Q USAR VECTORIZE PARA FAZER O GRAFICO)
f3=function(x){
  a=ifelse(x>2 & x<18,1,0)
  if (a==1){
    f=(1/10)*exp(-x)-10
    return(f)
  }  
  if (a==0){
    f=0
    return(f)
  }
}

    #Item d
f4=function(x,a,b){
  (((b^a)*x^(a-1)*exp(-b*x))/gamma(a))*I(x>0 & a>0 & b>0)
}

    #Item e
f5=function(x,a,b){
  (gamma(a+b)/(gamma(a)*gamma(b)))*x^(a-1)*(1-x)^(b-1)*I(x>0 & x<1 & a>0 & b>0)
}

#Questão 7###############

curve(f1(x,1),-10,30)
curve(f2(x),-5,5)
curve(f3,2,18)
curve(f4(x,1,1),1,30)
curve(f5(x,2,2),1,30)

#Questão 8####
 dp=function(x){
   s1=0
   for (i in x){
     s1=s1+((sum(i-mean(x)))^2)/length(x)
   }
   print(sqrt(s1))
 }

#Questão 9########
soma.media=function(x){
  s=sum(x)
  m=mean(x)
  print(s)
  print(m)
}

#Questão 10##########

IMC=function(Altura,Peso){
  Peso/(Altura)^2
}

#Questão 11############

IMC_novo=function(Altura,Peso){
  if (Peso/(Altura)^2>0 & Peso/(Altura)^2<18.5){
    return("Voce é considerado(a) desnutrido(a)")}
  if (Peso/(Altura)^2>18.5 & Peso/(Altura)^2<25){
    return("Voce é considerado(a) ormal")}
  if (Peso/(Altura)^2>25 & Peso/(Altura)^2<Inf){
    return("Voce é considerado(a) Obeso(a)")}}

#Questão 12###########

    #Item a
s=1
for (i in 1:99){
  s=s+1/(32+20*(i-1))
}
s

    #Item b
s=1
for (i in 1:99){
  s=s+((-1)^(i))/(11+10*(i-1))
}
s    

    #Item c
s=0
for (i in 1:100){
  s=s+i/15
}
s

    #item d
s=1/10
for (i in 1:99){
  s=s+10*(i)/10
}
s

    #Item e
s=0
for (i in 1:100){
  s=s+sqrt(log(i))
}
s

#Questão 13###############

    #Item a
f=function(x){
  a=x^3
  return(a)
}
t=integrate(f,lower=0,upper=10)  
class(t)

    #Item b1(USANDO O AS.NUMERIC)
f=function(x){
  a=3*(x^3+x)/5
  return(a)
}
  
I1=integrate(f,lower = 0,upper=5)[1]
I2=integrate(f,lower=7,upper=10)[1]
I3=integrate(f,lower=11,upper=15)[1]
I=as.numeric(I1)+as.numeric(I2)+as.numeric(I3)
I

    #Item b2(ACCESANDO OS VALORES DA LISTA)
f=function(x){
  a=3*(x^3+x)/5
  return(a)
}

integrate(f,lower = 0,upper=5)[[1]]+integrate(f,lower=7,upper=10)[[1]]+integrate(f,lower=11,upper=15)[[1]]

    #Item c
f=function(x){
  a=x^12*(1-x)^8
  return(a)
}
integrate(f,lower=0,upper=1) 

    #Item d
f=function(x){
  a=3*exp(-3*x)
  return(a)
}
integrate(f,lower = 0,upper=100)

#Questão 14###############

    #Item a
f=quote(x^3)
dx=D(f,"x")
dx

    #Item b
f=quote(cos(2*x)+exp(-3*x))
dx=D(f,"x")
dx

    #Item c
f=quote(3*x+log((x+y)))
dx=D(f,"x")
dx

    #Item d
f=quote(x*exp(-2*x)+log(1/x))
dx=D(f,"x")
dx

#Questão 15#################

    #Item a
f=quote(3*x^3-cos(x))
dx=D(f,"x")
dxx=D(dx,"x")
dxx

    #Item b
f=quote(3*x^3-y*cos(x))
dx=D(f,"x")
dxdy=D(dx,"y")
dxdy