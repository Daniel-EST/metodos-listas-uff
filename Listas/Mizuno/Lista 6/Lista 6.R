##PACOTES
library(dplyr)
library(tibble)
library(foreign)
library(readr)
library(readxl)
library(DescTools)
library(ggplot2)
library(tidyr)
library(sjPlot)
setwd("/Volumes/MIZUNO/Listas/Lista 6")
##
#Questao 1#########

#Estamos fazendo um Teste de Aderencia

ac <- c(20,10,10,15,30,20,35)
prob<-c(rep(1/7,7))
chisq.test(ac,p=prob,correct=FALSE)
#Para um nivel de significancia de alfa=5%=0.05 temos que alfa>p-valor=0.0001167, entao rejeitamos Ho
#,ou seja, temos pelo menos um dos dias com probabilidade diferente

#Questao 2####

#Estou fazendo um Teste de Homogeniedade

t <- matrix(c(51,58,48,26,33,29,42,38,16,13,30,16),nrow=4,ncol=3)
colnames(t) <- c('Pouca','Media','Alta')
rownames(t) <- c("tipo 1", "tipo 2", "tipo 3", "tipo 4");t
chisq.test(t,correct=FALSE)
#Para um nivel de significancia de alfa=5%=0.05, temos que alfa>p-valor=0.008669.
#Portanto, temos evidencias para um nivel de significancia de 5% que naa temos independencia entre
#os tipos de tratamento e suas reacoes

#Estatistica Descritiva
par(mar=c(5,4,4,7))
p <- apply(t,2,function(x){x/sum(x,na.rm=TRUE)})
barplot(p,main = "Gráfico de Barras Empilhados", 
        col = c(5,2,7,3), xlab = "Reação", ylab = "Tipo de Câncer")
legend(x = "topright",xpd=TRUE,legend=c("tipo 1", "tipo 2", "tipo 3", "tipo 4"),
       inset = c(-0.60,0.5),fill=rainbow(4))

# Análise Descritiva(OUTRA IDEIA)
## Gráfico de Barras empilhados
t1 = prop.table(t, margin = 2)
barplot(t1, main = "Gráfico de Barras Empilhados", 
        col = c(5,2,7,3), xlab = "Reação", ylab = "Tipo de Câncer")
legend(x=4,y=1, xpd=TRUE, legend=c("tipo 1", "tipo 2", "tipo 3", "tipo 4"),
       fill=c(5,2,7,3))

#Questao 3#####
base3 <- read_xls("Banco Escalas Psicologia.xls",na="999")
base3 <- base3 %>%head(nrow(.)-5L)
base3$Grupo <- factor(base3$Grupo,labels = c("Controle", "Trauma","TEPT"))
base3$Sexo <- factor(base3$Sexo,labels = c("1", "2"))

    #Item a
#Estou usando ANOVA
#Verficando normalidade

base3$Idade[base3$Grupo=='Controle'] %>% ks.test(.,"pnorm",mean(.),sd(.),alternative = "two.sided")
base3$Idade[base3$Grupo=='Trauma'] %>% ks.test(.,"pnorm",mean(.),sd(.),alternative = "two.sided")
base3$Idade[base3$Grupo=='TEPT'] %>% ks.test(.,"pnorm",mean(.),sd(.),alternative = "two.sided")
#Para um nivel de significancia de 3% temos evidencias que as 3 variveis analisadas tem ditribuicao normal

#Verificando Homocedasticidade

LeveneTest(base3$Idade,base3$Grupo,center='mean')
#Para o valor de alfa dado no enunciado nao rejeitamos Ho, ou seja, temos evidencia que as variancias
#sao iguais

aov(Idade~Grupo,data = base3) %>% summary()
#Para o valor de alfa dado no enunciado rejeitamos Ho, ou seja, temos evidencia que ha 
#pelo menos uma par de diferenca entre as idades.

#Fazendo teste de acompanhamento.

comparacao <- aov(Idade~Grupo,data = base3)
TukeyHSD(comparacao)
PostHocTest(comparacao,method='bonferroni')
PostHocTest(comparacao,method='duncan')
#Para um nivel de significancia de 3% temos que pelo teste de Duncan que Trauma-Controle possuem medias
#diferentes para idade, teremos a mesma conclusao usando os testes de Bonferroni e Tukey

    #Item b
#Vamos tirar o individuos do Controle

base3.3 <- base3 %>% filter(Grupo!="Controle")

#Teste de Independencia

teste <- base3.3 %>% group_by(Grupo,Sexo) %>% summarise(quant=length(Sexo))
t <- matrix(c(23,50,26,57),ncol=2,nrow=2)
chisq.test(t,correct=FALSE)
#Para um nivel de significancia de 5% nao rejeitamos Ho,ou seja, nao existe alguma relacao entre 
#ter algum trauma e seu sexo

    #Item c
#Estou usando ANOVA

#Verificando normalidade

base3$Anos_escolaridade[base3$Grupo=='Controle'] %>% ks.test(.,"pnorm",mean(.,na.rm=TRUE)
                                                             ,sd(.,na.rm=TRUE),alternative = "two.sided")
base3$Anos_escolaridade[base3$Grupo=='Trauma'] %>% ks.test(.,"pnorm",mean(.,na.rm=TRUE)
                                                           ,sd(.,na.rm=TRUE),alternative = "two.sided")
base3$Anos_escolaridade[base3$Grupo=='TEPT'] %>% ks.test(.,"pnorm",mean(.,na.rm=TRUE)
                                                         ,sd(.,na.rm=TRUE),alternative = "two.sided")
#Para nivel de significancia de 5% temos que asd variaveis de interesse NAO tem distribuicao normal
#Portanto, nao podemos fazer um teste ANOVA

    #Item d
#Estou usando ANOVA

#Verificando normalidade

base3$QI[base3$Grupo=="Trauma"] %>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$QI[base3$Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$QI[base3$Grupo=="TEPT"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
#Para um nivel de significancia de 5% temos que as vairiveis envolvidas tem distribuicao normal

#Verificando Homocedasticidade

LeveneTest(base3$QI,base3$Grupo,center='mean')
#Para o valor de alfa dado no enunciado nao rejeitamos Ho, ou seja, temos evidencia que as variancias
#sao iguais

aov(QI~Grupo,data=base3) %>% summary()
#Para o valor de alfa dado no enunciado reijeitamos Ho, ou seja, temos evidencias que ha pelo menos
#um par de grupo que possume diferente QI

#Fazendo teste de comparacao com o teste de Tukey e Bonferroni

comparacao=aov(QI~Grupo,data=base3)
PostHocTest(comparacao, method = "bonferroni")
TukeyHSD(comparacao)
#Para  nivel de significancia de 5% temos pelo dois teste que Trauma=Controle,TEPT=Trauma 
#e TEPT!=Controle. 

#Fazendo grafico para ver quais medias soa iguais
plot(PostHocTest(comparacao, method = "bonferroni"))

    #Item e
#Verificando normalidade

#Controle
base3$RAVLT[base3$Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$WCST[base3$Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
          #Estou aproximando o pvalor para 3%.
base3$Stroop[base3$Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$Digitos[base3$Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$`Reproduçao Visual`[base3$Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
#Trauma
base3$RAVLT[base3$Grupo=="Trauma"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$WCST[base3$Grupo=="Trauma"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$Stroop[base3$Grupo=="Trauma"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$Digitos[base3$Grupo=="Trauma"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$`Reproduçao Visual`[base3$Grupo=="Trauma"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
#TEPT
base3$RAVLT[base3$Grupo=="TEPT"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$WCST[base3$Grupo=="TEPT"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$Stroop[base3$Grupo=="TEPT"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$Digitos[base3$Grupo=="TEPT"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base3$`Reproduçao Visual`[base3$Grupo=="TEPT"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
#Para um nivel de significancia de 5% nao rejeitamos Ho, ou seja, temos evidencias que as variveis 
#tem distribuicao normal

#Verificando Homocedasticidade

LeveneTest(base3$RAVLT,base3$Grupo,center='mean')
#Para o valor de alfa dado no enunciado nao rejeitamos Ho, ou seja, temos evidencia que as variancias
#sao iguais

LeveneTest(base3$WCST,base3$Grupo,center='mean')
#Para o valor de alfa dado no enunciado nao rejeitamos Ho, ou seja, temos evidencia que as variancias
#sao iguais

LeveneTest(base3$Stroop,base3$Grupo,center='mean')
#Para o valor de alfa dado no enunciado rejeitamos Ho, ou seja, temos evidencia que
#as variancias sao iguais e assim NAO podemos fazer teste ANOVA

LeveneTest(base3$Digitos,base3$Grupo,center='mean')
#Para o valor de alfa dado no enunciado rejeitamos Ho, ou seja, temos evidencia que
#as variancias sao iguais e assim NAO podemos fazer teste ANOVA

LeveneTest(base3$`Reproduçao Visual`,base3$Grupo,center='mean')
#Para o valor de alfa dado no enunciado nao rejeitamos Ho, ou seja, temos evidencia que as variancias
#sao iguais

#Fazendo ANOVA

aov(RAVLT~Grupo,data=base3) %>% summary()
#Para um nivel de significancia de 3% rejeitamos Ho, ou seja, temos que pelo menos um par de media 
#diferente

aov(WCST~Grupo,data=base3) %>% summary()
#Para um nivel de significancia de 3% nao rejeitamos Ho, ou seja, temos que as medias sao iguais

aov(`Reproduçao Visual`~Grupo,data=base3) %>% summary()
#Para um nivel de significancia de 3% rejeitamos Ho, ou seja, temos que pelo menos um par de media 
#diferente

#Analise de Acompanhamento.

comparacao <- aov(RAVLT~Grupo,data=base3)
PostHocTest(comparacao,method='bonferroni')
#Para uma nivel de significanciade 3% pelo teste de Bonferroni temos que a media de RAVTL
#para Trauma e Controle sao diferentes e o mesmo e concluido para TEPT e Controle

comparacao <- aov(`Reproduçao Visual`~Grupo,data=base3)
PostHocTest(comparacao,method='bonferroni')
#Para uma nivel de significanciade 3% pelo teste de Bonferroni temos que a media de Reproduçao Visual
#para TEPT e Controle sao diferentes.

#Questao 4######
base4 <- read_xls("Banco Escalas Psicologia.xls",na="999")
base4 <- base4 %>%head(nrow(.)-5L)
#Criando a nova variavel e mexendo na base
base4$Novo_Grupo[base4$Grupo==0]=0
base4$Novo_Grupo[base4$Grupo==1 | base4$Grupo==2]=1
base4$Novo_Grupo <- factor(base4$Novo_Grupo,labels=c('Controle','Trauma'))
base4$Grupo <- factor(base4$Grupo,labels = c("Controle", "Trauma","TEPT"))
base4 <- base4 %>% select(Grupo,Novo_Grupo,everything())

    #Item a
#Verficando normalidade

base4$Idade[base4$Novo_Grupo =='Controle'] %>% ks.test(.,"pnorm",mean(.),sd(.),alternative = "two.sided")
base4$Idade[base4$Novo_Grupo =='Trauma'] %>% ks.test(.,"pnorm",mean(.),sd(.),alternative = "two.sided")
#Para um nivel de significancia de 3% temos evidencias que as 3 variveis analisadas tem ditribuicao normal

#Verificando Homocedasticidade

LeveneTest(base4$Idade,base4$Novo_Grupo ,center='mean')
#Para o valor de alfa dado no enunciado nao rejeitamos Ho, ou seja, temos evidencia que as variancias
#sao iguais

aov(Idade~Novo_Grupo ,data = base4) %>% summary()
#Para um nivel de significancia de 3% rejeitamos Ho, ou seja, temos que pelo menos um par de media
#e diferente

#Analise de acompanhamento

comparacao=aov(Idade~Novo_Grupo,data=base4)
PostHocTest(comparacao, method = "bonferroni")
#Para uma nivel de significanciade 3% pelo teste de Bonferroni temos que a media de Trauma e 
#diferente da media de Controle

    #Item b
#Vamos tirar o individuos do Controle

base4.4 <- base4 %>% filter(Novo_Grupo!="Controle")

#Teste de Independencia

teste <- base4.4 %>% group_by(Novo_Grupo,Sexo) %>% summarise(quant=length(Sexo))
t <- matrix(c(49,107),ncol=1,nrow=2)
chisq.test(t,correct=FALSE)

    #Item c
#Verificando normalidade

base4$Anos_escolaridade[base4$Novo_Grupo=='Controle'] %>% ks.test(.,"pnorm",mean(.,na.rm=TRUE)
                                                             ,sd(.,na.rm=TRUE),alternative = "two.sided")
base4$Anos_escolaridade[base4$Novo_Grupo=='Trauma'] %>% ks.test(.,"pnorm",mean(.,na.rm=TRUE)
                                                           ,sd(.,na.rm=TRUE),alternative = "two.sided")
#Para nivel de significancia de 5% temos que asd variaveis de interesse NAO tem distribuicao normal
#Portanto, nao podemos fazer um teste ANOVA

    #Item d
#Testando Normalidade

base4$QI[base4$Novo_Grupo=="Trauma"] %>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base4$QI[base4$Novo_Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
#Para um nivel de significancia de 5% temos que as vairiveis envolvidas tem distribuicao normal

#Verificando Homocedasticidade

LeveneTest(base4$QI,base4$Novo_Grupo,center='mean')
#Para o valor de alfa dado no enunciado nao rejeitamos Ho, ou seja, temos evidencia que as variancias
#sao iguais

    #Item e
#Verificando normalidade

#Controle
base4$RAVLT[base4$Novo_Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base4$WCST[base4$Novo_Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
#Temos pelo ks.test que a varivel WCST no grupo Trauma nao tem distibuicao normal para um nivel
#de signinificania de 3%.
base4$Stroop[base4$Novo_Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base4$Digitos[base4$Novo_Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base4$`Reproduçao Visual`[base4$Novo_Grupo=="Controle"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
#Trauma
base4$RAVLT[base4$Novo_Grupo=="Trauma"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base4$WCST[base4$Novo_Grupo=="Trauma"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
base4$Stroop[base4$Novo_Grupo=="Trauma"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
#Temos pelo ks.test que a varivel Stroop no grupo Trauma nao tem distibuicao normal para um nivel
#de signinificania de 3%.
base4$Digitos[base4$Novo_Grupo=="Trauma"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
#Temos pelo ks.test que a varivel Digitos no grupo Trauma nao tem distibuicao normal para um nivel
#de signinificania de 3%.
base4$`Reproduçao Visual`[base4$Novo_Grupo=="Trauma"]%>%ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative = "two.sided")
#Temos pelo ks.test que a varivel `Reproduçao Visual` no grupo Trauma nao tem distibuicao normal para um nivel
#de signinificania de 3%.

#Nao podemos usar ANOVA para comparar as medias de Reproduçao Visual`,Digitos,WCST e Stroop
#,pois a hipotese de normalidade e furada em cada um desses casos quando analisada a distribuicao 
#em cada grupo.

#Teste de Homocedacidade.

LeveneTest(base4$RAVLT,base4$Grupo,center='mean')
#Para o valor de alfa dado no enunciado nao rejeitamos Ho, ou seja, temos evidencia que as variancias
#sao iguais

#Fazendo ANOVA

aov(RAVLT~Novo_Grupo,data=base4) %>% summary()
#Para o nivel de significancia dado no enunciado rejeitamos Ho, ou seja, temos pelo menos 
#um par de medias diferentes

#Analise de Acompanhamento

a <- aov(RAVLT~Novo_Grupo,data=base4)
PostHocTest(a,method='bonferroni')
plot(PostHocTest(a,method='bonferroni'))
#Para o nivel de significancia ddado no enunciado e pelo teste de Bonferroni temos que a media de RAVLT
#para Trauma e Controle sao diferentes
###
#Questao 5####
base5 <- read.csv2("EMBRAPA.csv")
base5 <- base5 %>% head(nrow(.)-3L)
base5$Tratamento <- factor(base5$Tratamento,labels=c("1",'2','3','4'))
#TESTANDO NORMALIDADE

#Peso26

#Tratamento 1
base5$Peso26[base5$Tratamento==1]%>%ks.test(.,"pnorm",mean(.,na.rm=TRUE),sd(.,na.rm=TRUE)
                                            ,alternative = "two.sided")
#Tratamento 2
base5$Peso26[base5$Tratamento==2]%>%ks.test(.,"pnorm",mean(.,na.rm=TRUE),sd(.,na.rm=TRUE)
                                            ,alternative = "two.sided")
#Tratamento 3
base5$Peso26[base5$Tratamento==3]%>%ks.test(.,"pnorm",mean(.,na.rm=TRUE),sd(.,na.rm=TRUE)
                                            ,alternative = "two.sided")
#Tratamento 4
base5$Peso26[base5$Tratamento==4]%>%ks.test(.,"pnorm",mean(.,na.rm=TRUE),sd(.,na.rm=TRUE)
                                            ,alternative = "two.sided")
#Peso26
base5$Peso26%>%ks.test(.,"pnorm",mean(.,na.rm=TRUE),sd(.,na.rm=TRUE)
                                            ,alternative = "two.sided")
#Para nivel de significancia de 5% rajeitamos Ho, ou seja, temos evidencias que que os Peso26 dos
#bezerros de cada tratamento tem distribuicao normal

    #Item a
#Usando ANOVA

#Verificando Homocedasticidade
#Analise Descritiva

ggplot(base5,aes(x=Tratamento,y=Peso26))+geom_boxplot(fill='red')

LeveneTest(base5$Peso26, base5$Tratamento, center = "mean")
#Como p-valor=0.1975>0.05=alfa nao rejeitamos Ho, ou seja, temos evidencia que as variancias sao iguais.

aov(Peso26 ~ Tratamento, data = base5) %>% summary()
#Como p-valor=0.3>0.05=alfa nao rejeitamos Ho, ou seja, temos evidencia que os pesos dos 
#bezerro sao iguais apos 26 semanas.

    #Item b
#Queremos fazer um TH para duas pop com variancia desconhecida
#Verificando normalidade do Peso0.

base5$Peso0 %>% ks.test(.,'pnorm',mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),alternative='two.sided')
#Para nivel de significancia de 5% rajeitamos Ho, ou seja, temos evidencias que que os Peso0 dos
#bezerros de cada tratamento tem distribuicao normal

#Verificando igualdade das variancias

VarTest(base5$Peso26,base5$Peso0,alternative='two.sided',ratio=1,conf.level=0.95)
#Para nivel de significancia de 5% rajeitamos Ho, ou seja, temos evidencias que as variancias do peso
#na semana 26 e na semana 0 sao diferentes

#Ho:M26-M0<=88 x M26-M0>88 

t.test(base5$Peso26,base5$Peso0,conf.level=0.95,alternative="greater",mu=88,var.equal=FALSE)
#Para um nivel de significancia de 5% nao rejeitamos Ho, ou seja, temos evidencias que a difenrenca 
#entre os pesos na semana 

#Questao 6#####
base6 <- read.csv2("Basevulnerabilidade.csv")
base6$situacao <- factor(base6$situacao,labels=c("Alugado",'Proprio'))
base6$esmae <- factor(base6$escmae,labels=c("Baixa",'Media','Alta'))
base6$vulnerabilidade <- factor(base6$vulnerabilidade,labels=c('0','1'))

    #Item a 

base <- base6 %>% group_by(vulnerabilidade,situacao) %>% summarise(N=length(situacao))
t <- matrix(c(18,5,7,10),nrow=2,ncol=2)
chisq.test(t,correct=FALSE)
#Para um nivel de significancia de 5% rejeitamos Ho, ou seja, temos evidencias que situacao de 
#vunerabilidade e situacao do imovel nao sao independentes.

#Analise Descritiva
t <- prop.table(t,2)
par(mar = c(5,4,4,7))
colnames(t) <- c("Nao vulneravel", "Vulneravel")
rownames(t) <- c("Alugado", "Proprio")
barplot(t,col=c('red','blue'),
        main = "Situacao x Vulnerabilidade",
        xlab = "Vulnerabilidade",
        ylab= "Proporcao")
legend(x = "topright", 
       legend = c("Alugado","Proprio"), 
       fill = c('red','blue'), 
       title = "Legenda",
       inset = c(-0.60,0.5),
       xpd = TRUE)

#Tabela de Dupla Entrada

sjt.xtab(base6$situacao,base6$vulnerabilidade,var.labels=c('Situacao da moradia',"Escalaridade mae")
         ,show.cell.prc=TRUE,show.summary=FALSE)

#Fazendo Teste de Independencia

base <- base6 %>% group_by(vulnerabilidade,escmae) %>% summarise(quant=length(escmae))

t <- matrix(c(10,8,7,3,8,4),nrow=2,ncol=3)
chisq.test(t,correct=FALSE)
#Para um nivel de significancia de 5%  nao rejeitamos Ho, ou seja, temo evidencia que a situcao de 
#vunerabiliadde e escolaridade da mae sao independentes.

#Fazendo Tabela de Dupla Entrada.

sjt.xtab(base6$vulnerabilidade,base6$escmae,var.labels=c('Vunerabilidade',"Escalaridade mae")
         ,show.cell.prc=TRUE,show.summary=FALSE)

    #Item b(esta faltando o dado idade)
#Estamos fazendo um TH para diferenca de idades.

#Questoa 7######
base7 <- read_xls("Banco Cancer Mama.xls")
base7$cancer <- factor(base7$cancer,labels = c('Nao','Sim'))
base7$menstruacao <- factor(base7$menstruacao,labels = c('Nao','Sim'))
base7$grauparentesco <- factor(base7$grauparentesco,labels = c('3o Grau','2o Grau','1o Grau'))
base7$antes <- as.numeric(base7$antes)
base7$depois <- as.numeric(base7$depois)

#Fazendo Teste de Homogeniedade

base <- base7 %>% group_by(cancer,menstruacao) %>% summarise(N=n())
                                                             
t <- matrix(c(21,10,6,20),nrow=2,ncol=2)
colnames(t) <- c('Nao','Sim')
rownames(t) <- c('Nao','Sim');t
chisq.test(t,correct=FALSE)
#Para um nivel de significancia de 5% rejeitamos Ho, ou seja, temos evidencias que as populacoes
#nao sao homogeneas

#Tabela de dupla entrada

sjt.xtab(base7$cancer,base7$menstruacao,var.labels=c('Cancer','Menstruacao antes dos 12')
         , show.summary = FALSE,show.col.prc = TRUE)

#Analise Descritiva

base10 <- base7 %>% group_by(cancer,menstruacao) %>% summarise(N=length(cancer))
s <- base10$N[base10$menstruacao=='Sim'] %>% sum()
n <- base10$N[base10$menstruacao=='Nao'] %>% sum()
base10$N<- ifelse(base10$menstruacao=='Sim',base10$N/s,base10$N/n)
base7 %>% ggplot(aes(x=menstruacao,fill=cancer))+geom_bar()

#Questao 8####
base8 <- base7 %>% filter(base7$cancer=="Sim")

#Verificando normalidade(ks.test)

ks.test(base8$antes,'pnorm',mean(base8$antes),sd(base8$antes,na.rm=TRUE))
ks.test(base8$depois,'pnorm',mean(base8$depois,na.rm=TRUE),sd(base8$depois,na.rm=TRUE))
#Para um nivel de significancia de 5% nao rejeitamo Ho, ou seja,usando os dois testes temos evidencia
#que as duas populcoes tem distribuicao normal

#Verificando igualdade das variancias

VarTest(base8$depois,base8$antes,alternative='two.sided',ratio=1)
#Para um nivel de significancia de 5% nao rejeitamo Ho, ou seja, temos evidencias ques as duas 
#variancias sao diferentes

#Estamos fazendo um TH para duas pop.
#Ho:Md-Ma<=0 x H1:Md-Ma>0

t.test(base8$depois,base8$antes,alternative='greater',mu=0,var.equal=FALSE)
#Para um nivel de significancia de 5% nao rejeitamo Ho, ou seja, temos evidencia que o tratamento 
#fez efeito.Portanto, ocorreu uma diminuicao no numero de celulas cancerigenas.

#Questao 9####

#Estamos fazendo um TH para duas pop com variancia desconhecia
#Verificando normalidade(ks.test)

sc <- base7$peso[base7$cancer=='Nao']
cc <- base7$peso[base7$cancer=="Sim"]

ks.test(cc,'pnorm',mean(cc),sd(cc))
ks.test(sc,'pnorm',mean(sc),sd(sc))
#Para um nivel de significancia de 5% nao rejeitamo Ho, ou seja, temos evidencia que as pelo dois
#teste as duas populcoes tem distribuicao normal

#Analise Descritiva

base7 %>% ggplot(aes(x=cancer,y=peso))+geom_boxplot(fill='red')

#Verieficando igualdade das variancias

VarTest(cc,sc,alternative='two.sided',ratio=1)
#Para um nivel de significancia de 5% nao rejeitamos Ho, ou seja, temos evidencias que as duas 
#variancias soa iguais

#Fazenodo t.test
#Ho: Msc-Mcc>=0 x H1: Msx-Mcc<0

t.test(sc,cc,alternative='less',conf.level=0.95,mu=0,var.equal=TRUE)
#Para um nivel de significancia de 5% nao rejeitamos Ho, ou seja, temos evidencia que paciente
#sem cancer tem pelo menos o mesmo peso do que os pacientes que tem cancer

#Questao 10####
base10 <- base7 %>% group_by(grauparentesco,cancer) %>% summarise(N=length(cancer))
s <- base10$N[base10$cancer=='Sim'] %>% sum()
n <- base10$N[base10$cancer=='Nao'] %>% sum();base10

#Tabela de dupla entrada

sjt.xtab(base7$grauparentesco,base7$cancer,var.labels=c("Grau de parentesco","Cancer"),show.col.prc=TRUE)

#Fazendo Teste de Homogeniedade

t <- matrix(c(8,10,7,12,15,5),nrow=2,ncol=3)
colnames(t) <- c('3o Grau','2o Grau','1o Grau')
rownames(t) <- c('Sim','Nao');t
chisq.test(t, correct = FALSE)

#Para um nivel de significancia de 5% rejeitamos Ho, ou seja, temos evidencia que as populacoes 
#nao sao homogeneas