# Pacotes ######################################################################
# Ativando pacote tibble
require(tibble)
# Ativando pacote purrr 
# Para usar o pipe %>%
require(purrr)
# Ativando pacote dplyr
require(dplyr)

# Questao 1 ####################################################################
#a
(8/22)^(-3) + gamma(4)/22
#b
sqrt(25/102) + log10(33)
#c
(100/22)^(-1/4) + exp(-2/3) - 2/3
#d 
(factorial(10)/factorial(6)* factorial(4)) - factorial(7)

# Questao 2 ####################################################################
#a
1:10
#b
seq(1,100,3)
#c
seq(0,1000,50)
#d
rep(c(1,22,5,3),c(6,2,2,4))
#e
rep(rep(9:12,rep(2,4)),3)

# Questao 3 ####################################################################
#Matriz
A <- matrix(c(rep(c(0,1),500),1:1000,rep(seq(10,50,10),200)),ncol = 3)
#a
aA <- matrix(c(A[seq(1,1000,2),1],A[seq(1,1000,2),3]),ncol = 2)
#b
bA <- matrix(c(rep(1:200,c(rep(5,200))),A[,2],A[,3]),ncol = 3,nrow = 1000)
#c
cA <- A
cA[seq(0,1000,2),] <- c(0,0,0)

# Questao 4 ####################################################################
# Criando os tibbles
baseQ4 <- tibble(Nome = c('Joao','Maria','Ana','Fabia','Rodrigo','Renato','Luciana','Guilherme','Gabriel','Diogo'), Idade = c(27,22,21,37,29,27,21,18,19,25),Altura = c(183,153,179,158,165,170,151,166,172,183)/100,Genero = c(1,0,0,0,1,1,0,1,1,1),Estado_Civil = c(0,0,1,0,1,0,0,0,1,1),Bairro = c('Icarai','Inga','Botafogo','Lagoa','Boa Viagem','Leblon','Leblon','Inga','Icarai','Botafogo'))
baseQ4Peso <- tibble(nome = c('Joao','Maria','Ana','Fabia','Rodrigo','Renato','Gabriel','Diogo'),peso = c(40,65,77,63,78,80,83,77))
baseQ4Op <- tibble(Nome = c('Joao','Maria','Ana','Fabia','Rodrigo','Renato','Gabriel','Diogo','Vicente','Fernando'),opniao = c(0,0,1,0,1,0,rep(1,4)))
baseQ4Cidade <- tibble(bairro = c('Inga','Icarai','Boa Viagem','Botafogo','Leblon','Copacabana','Ipanema','Lagoa','Gavea','Sao Francisco'),Cidade = c(0,0,0,rep(1,7)))

#tratando os tibbles
baseQ4$Genero <- baseQ4$Genero %>% 
  factor(labels = c('Feminino','Masculino'))
baseQ4$Estado_Civil <- baseQ4$Estado_Civil %>%
  factor(labels = c('Solteiro','Casado'))
baseQ4Op$opniao <-baseQ4Op$opniao %>% 
  factor(labels = c('Contra','A favor'))
baseQ4Cidade$Cidade <- baseQ4Cidade$Cidade %>% 
  factor(labels = c('Niteroi', 'Rio de Janeiro'))

#a
baseQ4a <- right_join(baseQ4,baseQ4Peso, by = c('Nome' = 'nome'))
#b
saveRDS(baseQ4a,'Listas/Lista 01/BaseQ4a.RDS')
#c
baseQ4c <- left_join(baseQ4,baseQ4Peso, by = c('Nome' = 'nome'))
#d
readr::write_csv(baseQ4c,'Listas/Lista 01/baseQ4c.csv')
#e
baseQ4e <- full_join(baseQ4,baseQ4Peso, by = c('Nome' = 'nome')) %>% 
  full_join(baseQ4Op, by = 'Nome')
#f
foreign::write.dta(baseQ4e,'Listas/Lista 01/baseQ4e.dta')
#g
baseQ4g <- full_join(baseQ4,baseQ4Cidade,by = c('Bairro' = 'bairro'))
#h
readr::write_delim(baseQ4g,'Listas/Lista 01/baseQ4g.txt')
#i
{
  a <- file.size(dir()[1])
  b <- NULL
  for(i in 1:4){
    if(a < file.size(dir()[i])){
      file.size(dir()[i])
      b <- dir()[i]
    }
  }
  (menorArq <- print(paste(b,'tem o menor tamanho','=',a,'bytes',sep = ' ')))
  rm(list = c('a','b','i'))
}

# Questao 5 ####################################################################
# Importacao
Banco1xls <- readxl::read_xls("Listas/Lista 01/Banco1.xls")
#a
max(Banco1xls$Altura)
#b
max(Banco1xls$Peso)
#c
Banco1xls$Grupo[which.min(Banco1xls$Altura)]
#d
Banco1xls$Grupo <- Banco1xls$Grupo %>% 
  factor(labels = c('Placebo','Tramento A'))
#e
readr::write_csv(Banco1xls,'Listas/Lista 01/Banco1xls.csv')

# Questao 6 ####################################################################
# Importacao
poptotjovem2010 <- readr::read_csv('Listas/Lista 01/populacaototaljovem2010.csv')
dados2010 <- readr::read_csv('Listas/Lista 01/dados2010.csv')
#a
dados2010mun <- distinct(dados2010,codmun) ; dados2010mun
#b
dados2010 %>% 
  group_by(codmun) %>% 
    summarise(tot.estupro = sum(Estupros , na.rm = T))
#c
# Adicionando nova variavel
dados2010$idade.cat <- rep(NA, length(dados2010$Idade))
for(i in 1:length(dados2010$Idade)){
  if(!(is.na(dados2010$Idade[i]))&&dados2010$Idade[i] < 18){
    dados2010$idade.cat[i] <- 1
  }
  if(!(is.na(dados2010$Idade[i]))&&dados2010$Idade[i] >= 18){
    dados2010$idade.cat[i] <- 2
  }
  if(is.na(dados2010$Idade[i])){
    dados2010$idade.cat[i] <- NA
  }
}
rm('i')
# Contabilizando
# Preciso pensar em uma otimizacao
dados2010 %>% 
  group_by(idade.cat) %>% 
    summarise(tot.afogamento = sum(Afogamentos , na.rm = T))
#d
dados2010 %>% 
  group_by(codmun) %>% 
    summarise(pop.tot = length(Idade) ,
              pop.jovem = sum(idade.cat[idade.cat==1&!(is.na(idade.cat))]))
#e
dados2010ren<- dados2010 %>% 
  rename(maior.idade = idade.cat)
#f
filter(dados2010,idade.cat == 1)
# ou
dados2010[dados2010$idade.cat==1&!(is.na(dados2010$idade.cat)),]
# Questao 7 ####################################################################
# Importacao
basemae <- foreign::read.dta('Listas/Lista 01/basemae.dta')
#a
basemaeUR <- basemae %>% group_by(munic_res) %>% 
  summarise(UR = paste0(strsplit(as.character(munic_res),'')[[1]][1],strsplit(as.character(munic_res),"")[[1]][2]))
basemaeGR <- basemae %>% group_by(munic_res) %>% 
  summarise(GR = paste0(strsplit(as.character(munic_res),'')[[1]][1]))
basemae <- basemae %>% full_join(basemaeUR, by = 'munic_res') %>% 
  full_join(basemaeGR,by= 'munic_res')
#b
basemae %>% 
  group_by(UR) %>%
    filter(between(ano,2000,2005), UF == 21) %>% 
      summarise(media.agressao = mean(agressao,na.rm = T) , media.estupro = mean(estupro, na.rm = T))
#c
basemeso <- basemae %>%
  group_by(meso_res) %>% 
    summarise(tot.mun = length(munic_res)) %>% 
      left_join(select(basemae,meso_res, meso_nome_res), by = 'meso_res')
#d
basemae %>%
  group_by(ano) %>% 
    summarise(media.homicidio = mean(homicidio,na.rm = T), media.suicidio = mean(suicidio, na.rm = T))
#e
basemae %>%
  group_by(GR) %>% 
   summarise(media.homicidio = mean(homicidio,na.rm = T), media.suicidio = mean(suicidio, na.rm = T))
#f
# Deu preguica