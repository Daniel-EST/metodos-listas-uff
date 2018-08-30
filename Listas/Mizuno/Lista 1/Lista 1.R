#PACOTES
library(readxl)
library(foreign)
library(tibble)
library(dplyr)
library(readr)
getwd()
setwd("/Volumes/MIZUNO/Listas/Lista 1")

#Questao 1####

    #Item a
(8/22)^(-3)+gamma(4)/22

    #Item b
sqrt(25/102)+log10(33)

    #Item c
(100/22)^(-1/4)+exp(-2/3)-2/3

    #Item d
choose(10,4)-factorial(7)

    #Item e
beta(2,3)+gamma(9)+abs(-5)-log()

#Questao 2#################

    #Item a
seq(1:10)
    #Item b  
seq(1,100,by=3)

    #Item c 
seq(0,1000,by=50)

    #Item d
rep(c(1,2,5,3),c(6,4,2,3))

    #Item e  
rep(9:12,each=2,len=24)

#Questao 3###############

    #Criando A
a=matrix(c(rep(0:1,500),seq(1,1000),rep(c(10,20,30,40,50),200)),nrow=1000,ncol=3)

    #Item a
a[seq(1,1000,by = 2),1]
a[seq(1,1000,by = 2),3]
b=matrix(c(a[seq(1,1000,by = 2),1],a[seq(1,1000,by = 2),3]),nrow=500,ncol=2)

    #Item b
a[,1]=rep(seq(1:200),each=5)

    #Item c
a[seq(2,1000,by=2),]=c(0,0,0)

#Questao 4#################

    #Criando os Tibble
base1=tibble(Nome=c("Joao","Maria","Ana","Fabia","Rodrigo","Renato","Luciana","Guilherme","Gabriel","Diogo"),
            Idade=c(27,22,21,37,29,27,21,18,19,25),
            Altura=c(1.82,1.53,1.79,1.58,1.65,1.70,1.51,1.66,1.72,1.83),
            Genero=c("M","F","F","F","M","M","F","M","M","M"),
            EstadoCivil=c("Solteiro","Solterio","Casada","Solteiro","Casada","Solterio","Solterio","Solterio","Casada","Casada"),
            Bairro=c("Icarai","Inga","Botafogo","Lagoa","Boa Viagem","Leblon","Leblon","Inga","Icarai","Botafogo"))
base2=tibble(Nome=c("Joao","Maria","Ana","Fabia","Rodrigo","Renato","Gabriel","Diogo"),
             Peso=c(40,65,77,63,78,80,83,77))
base3=tibble(Nome=c("Joao","Maria","Ana","Fabia","Rodrigo","Renato","Gabriel","Diogo","Vicente","Fernando"),
             Opniao=c("Contra","Contra","A favor","Contra","A favor","Contra","A favor","A favor","A favor","A favor"))
base4=tibble(Bairro=c("Icarai","Inga","Boa Viagem","Botafogo","Leblon","Copacabana","Ipanema","Lagoa","Gavea","S?o Francisco"),
             Cidade=c("Niteroi","Niteroi","Niteroi","Rio de Janeiro","Rio de Janeiro","Rio de Janeiro","Rio de Janeiro","Rio de Janeiro","Rio de Janeiro","Rio de Janeiro"))

    #Item a
b1=inner_join(base1,base2,by="Nome")

    #Item b
saveRDS(b1,"Base modificada.rds")

    #Item c
b2=left_join(base2,base1,by="Nome")

    #Item d
write.csv2(b2,"Base modificada2.csv")

    #Item e
b3=full_join(base1,base3,by="Nome")
b4=full_join(b3,base3,by="Nome")

    #Item f
write.dta(b4,"Base modificada3.dta")

    #Item g 
b=tibble(Cidade=c(NA))
base5=cbind(base1,b)
base5$Cidade[base5$Bairro=="Icarai"|base5$Bairro=="Inga"|base5$Bairro=="Boa Viagem"]="Niteroi"
base5$Cidade[base5$Bairro=="Botafogo"|base5$Bairro=="Lagoa"|base5$Bairro=="Leblon"]="Rio de Janeiro"
base5

    #Item g(USANDO A FUN??O INNER_JOIN)
inner_join(base1,base4)

    #Item h
write.table(base5,"Base modificada4.txt")

    #Item i
file.size("Base modificada.rds")
file.size("Base modificada2.csv")
file.size("Base modificada3.dta")
file.size("Base modificada4.txt")


#Questao 5################
getwd()
setwd("/Listas/Lista 1")
base=read_excel("Banco1.xls")

    #Item a
a=max(base$Altura)

    #Item b
subset(base$Peso,base$Altura==max(base$Altura))

    #Item c
subset(base$Grupo,base$Peso==min(base$Peso))

    #Item d
base$Grupo=factor(base$Grupo,labels = c("Placebo","Tratamento A"))

    #Item e
write.csv2(base,"Banco1.csv",row.names=FALSE)
  
#Questao 6###############

    #Importando arquivo
base=read.csv("populacaototaljovem2010.csv")
base2=read_csv2("dados2010.csv")

    #Item a
b1=distinct(base2,codmun)

    #Item b
base2%>%group_by(codmun)%>%summarise(tot.Estupros=sum(Estupros,na.rm = TRUE))

    #Item c
a=tibble(idade.cat=c(NA))
base3=cbind(base2,a)
base3$idade.cat[base3$Idade<18]=1
base3$idade.cat[base3$Idade>=18]=2

    #Item c(USANDO A FUNCAO CUT)
base2$idade.cat=cut(base2$Idade,breaks=c(-Inf,17,Inf),labels=c("1","2"))

    #Item d
base3=mutate(base,municipio=substring(municipio,1,6))
b2=base3%>%group_by(municipio)%>%summarise(pop.tot=sum(populacaototal,na.rm = TRUE),
                                        jovem.tot=sum(populacaojovem,na.rm = TRUE))
b1=select()
    #Item e
base4 = rename(base3,maior.idade = idade.cat)
    
#Questao 7###############
    
    #Importando arquivo
base9=read.dta("basemae.dta")


    #Item a
base9=mutate(base9,GR=substring(munic_res,1,1),UF=substring(munic_res,1,2))
base9=select(base9,munic_nome_res,munic_res,GR,UF,everything())

    #Item b
b1=base9%>%select(UF,ano,agressao,estupro)%>%filter(UF==21 & ano>=2000 & ano<=2005)%>%summarise(media.agr=mean(agressao,na.rm=TRUE),
                                                                                                media.est=mean(estupro,na.rm=TRUE))
b1=round(b1,4)

    #Item c
base8=distinct(base9,meso_res,meso_nome_res,munic_res)

    #Iem d
a=base9%>%group_by(ano)%>%summarise(mediaH=mean(base9$homicidio,na.rm=TRUE),mediaS=mean(base9$suicidio,na.rm=TRUE))
                                                       
    #Item e
a=base9%>%group_by(ano,GR)%>%summarise(mediaH=mean(homicidio,na.rm=TRUE),mediaS=mean(suicidio,na.rm=TRUE))
base9%>%group_by(GR)%>%summarise(mediaH=mean(homicidio,na.rm=TRUE),mediaS=mean(suicidio,na.rm=TRUE))
e=base9%>%filter(ano=2000,GR=6)
mean(base9$homicidio,na.rm=TRUE)

    #Item f
base10=base9 %>% group_by(micro_nome_res) %>% 
  summarise(tot.homicidio=sum(homicidio,na.rm = T),tot.pop=sum(populacao,na.rm = T),
            tot.homicidiopaf=sum(homicidio_paf,na.rm = T))
base11=base9 %>% group_by(micro_nome_res) %>% 
  summarise(tot.homicidio=sum(homicidio,na.rm = T)/sum(populacao,na.rm = T),
            tot.homicidiopaf=sum(homicidio_paf,na.rm = T)/sum(populacao,na.rm = T))
