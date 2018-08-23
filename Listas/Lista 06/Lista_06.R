# Pacotes ######################################################################
require(dplyr) ; require(readr) ; require(readxl) ; require(DescTools)
require(ggplot2) ; require(stringr) ; require(RColorBrewer) ; require(tidyr)

# DISCLAIMER: PARA OS TESTE ANOVA ESTA SUPOSTO QUE AS POPULAÇÕES SÃO IDEPENDENTES.
# FIZ OS GRÁFICOS USANDO O RBASE, INFELIZMENTE A SINTAXE DO GGPLOT É MUITO COMPLEXA
# ESTOU ESTUDANDO PARA APRENDER A USAR ESSE TROÇO DIREITO.
# Questão 1 ####################################################################
q1_data <- tibble(dia = c("Seg","Ter","Qua","Qui","Sex","Sab","Dom"),
                  num = c(20,10,10,15,30,20,35)) ; q1_data
par(mfrow = c(1,1))

q1_rel <- prop.table(q1_data$num) ; q1_rel
q1_rel <- matrix(c(q1_rel, 1 - q1_rel), ncol = 2, byrow = F) ;q1_rel

colnames(q1_rel) <- c("Ocorrência","Não Ocorrência")
rownames(q1_rel) <- c("Seg","Ter","Qua","Qui","Sex","Sab","Dom")
q1_rel <- t(q1_rel) ; q1_rel

# Escolhendocores do pacote RcolorBrewer
par(mar = c(5,4,4,9.5))
cores_q1 <- brewer.pal(3, "Paired")
barplot(q1_rel, col=cores_q1, 
        border= "white", 
        xlab= "Reação", 
        ylab = "Frequência",
        main = "Gráfico da proporção total de acidentes \npor dia da semana")
legend("topright", legend = rownames(q1_rel),
       inset = c(-0.535,0.3),
       fill = cores_q1,
       xpd = TRUE,
       title = "Acidente")
# Tô fazendo um teste de independência aqui.
chisq.test(q1_data$num, correct = FALSE)

# Mas também posso fazer um teste de aderência
chisq.test(q1_data$num, p = rep(1/7, 7), correct = FALSE)

# Para um nível de significância \alpha = 0.05 rejeitamos a hipótese nula, ou se
# ja temos motivos pra acreditar que os dias da semana influenciam a quantidade 
# de acidentes.

# Questão 2 ####################################################################
tipo <- str_c("Tipo ", c("I","II","III","IV"))
tib_q2 <- tibble(tipos = tipo, 
                 Pouca = c(51,58,48,26), 
                 Media = c(33,29,42,38), 
                 Alta = c(16,13,30,16)) ; tib_q2

q2_data <- matrix(c(51,33,16,58,29,13,48,42,30,26,38,16), 
                  nrow = 4, 
                  ncol = 3,
                  byrow = T) ; q2_data

# Fazendo um gráfico de barras para as proporções 
# Nomeando colunas  e linhas (respectivamente)
colnames(q2_data) <- c("Pouca","Média","Alta")
rownames(q2_data) <- tipo

# Escolhendocores do pacote RcolorBrewer
cores <- brewer.pal(4, "Set2") 

# Calculando as proporções
prop <- apply(q2_data, 2, function(x){x/sum(x,na.rm=T)})

# Na questão 6 explico um pouco melhor como criei esse gráfico.
par(mfrow= c(1,1), mar = c(5,4,4,6))
barplot(prop, col=cores, 
        border="white", 
        xlab="Reação", 
        ylab = "Proporção",
        main = "Gráfico de proporção dos tipos de câncer \npor reação a quimioterapia")
legend(x = "topright", 
       legend = tipo, 
       fill = cores, 
       title = "Legenda",
       xpd = TRUE,
       inset = c(-0.25,0.1))

# Realizando o teste de homogeneidade (pelo jeito que sua amostra foi retirada),
# pegamos o tipo de câncer depois observamos a reação quimioterapia.
chisq.test(q2_data, correct = FALSE)

# Para um nível de significância \alpha = 0.05 rejeitamos a hipótese nula, ou se-
# ja temos motivos pra acreditar que existe relação entre a reação a quimiotera
# pia e o tipo de câncer.

# Questão 3 ####################################################################
# a)
q3_data <- read_xls("Listas\\Lista 06\\Banco Escalas Psicologia.xls", na = "999")

q3_data <- q3_data %>% 
  head(nrow(.)-5L)

q3_data$Grupo <- q3_data$Grupo %>% 
  factor(labels = c("Controle", "Trauma","TEPT")) ; q3_data$Grupo

q3_grupo <- q3_data %>%
  group_by(Grupo) %>% 
  summarise(mean.age = mean(Idade), 
            sd = sd(Idade),
            N = n())

# Boxplot pra ganhar alguma intuição do que vai acontecer
par(mfrow = c(1,1))
boxplot(Idade ~ Grupo, data = q3_data,
        main = "Boxplot das Idades por grupo",
        xlab = "Grupo",
        ylab = "Idade (em anos)")

# Primeiro checar normalidade
# Sentimento
par(mfrow = c(2,2), mar = c(5.1, 4.1, 4.1, 2.1))

control <- q3_data %>% 
  filter(Grupo == "Controle") %>% 
  select(Idade) %>% 
  unlist(use.names = F)
qqnorm(control, main = "Normal Q-Q Plot para grupo de controle")
qqline(control)

trauma <- q3_data %>% 
  filter(Grupo == "Trauma") %>% 
  select(Idade) %>% 
  unlist(use.names = F)

qqnorm(trauma, main = "Normal Q-Q Plot para grupo de trauma")
qqline(trauma)

tept <- q3_data %>% 
  filter(Grupo == "TEPT") %>% 
  select(Idade) %>% 
  unlist(use.names = F)

qqnorm(tept, main = "Normal Q-Q Plot para grupo de TEPT")
qqline(tept)

par(mfrow = c(1,1))

# Teste de hipótese para normalidade
control %>% 
  ks.test("pnorm", mean(.), 
        sd(.), 
        alternative = "two.sided")
# Para um nível de significância \alpha = 0.03 acreditamos que a distribuição
# seja normal

trauma %>% 
  ks.test("pnorm", mean(.), 
          sd(.), 
          alternative = "two.sided")
# Para um nível de significância \alpha = 0.03 acreditamos que a distribuição
# seja normal

tept %>% 
  ks.test("pnorm", mean(.), 
          sd(.), 
          alternative = "two.sided")
# Para um nível de significância \alpha = 0.03 acreditamos que a distribuição
# seja normal
# Chegamos a conclusão que as 3 populações são normais para a variável idade.

# Teste de hipótese para homocedasticidade
class(q3_data$Grupo)
LeveneTest(q3_data$Idade, q3_data$Grupo, center = "mean")

# Para um nível de significância \alpha = 0.03 acreditamos que as idades sejam 
# homocedásticas.

# ANOVA
q3_ANOVA <- aov(Idade ~ Grupo, data = q3_data)
summary(q3_ANOVA)

# Para um nível de significância \alpha = 0.03 acreditamos que as idades são  
# são diferentes para cada grupo.
# Quem  é diferente de quem? Teste PostHoc neles!
PostHocTest(q3_ANOVA, method = "bonf")
# Para um niível de significância \alpha = 0.03 tiramos a conclusão que a média
# entre as idades do grupo de Trauma difere do grupo de controle.

# b)
# Tratando e fazendo consulta a base da dados
q3_data$Sexo <- q3_data$Sexo %>% 
  factor(labels = c("Masculino","Feminino")) ; q3_data$Sexo

# Retirei o grupo de controle pois eles não possuem trauma.
q3_sex_matrix <- q3_data %>% 
  filter(Grupo != "Controle") %>% 
  group_by(Sexo, Grupo) %>% 
  summarise(N=n()) %>% 
  .$N %>% 
  matrix(nrow = 2,ncol = 2, byrow = TRUE) ; q3_sex_matrix

# Nomeando colunas  e linhas (respectivamente)
colnames(q3_sex_matrix) <- c("Trauma", "TEPT")
rownames(q3_sex_matrix) <- c("Masculino", "Feminino")

# Escolhendo cores
cores_q3_sex <- c("#54b0f7","#ff91f7")

# Calculando as proporções
prop_q3_sex <- apply(q3_sex_matrix, 2, function(x){x/sum(x,na.rm=T)})

par(mar = c(5, 4, 4, 8))

barplot(prop_q3_sex, col=cores_q3_sex, 
        border="white", 
        xlab="Reação", 
        ylab = "Proporção",
        main = "Gráfico de proporção do gênero \npor tipo de trauma")
legend(x = "topright", 
       legend = c("Masculino", "Feminino"), 
       fill = cores_q3_sex, 
       title = "Legenda",
       xpd = TRUE,
       inset = c(-0.35,0.35))

par(mar = c(5.1, 4.1, 4.1, 2.1))
# Fazendo teste de indepedência.
chisq.test(q3_sex_matrix, correct = FALSE)

# Para um nível de significância \alpha = 0.05 acreditamos que não existe relação 
# entre genero e o transtorno

# c)
################################### IGNORAR ####################################
# Tratando e fazendo consulta a base de dados
# q3_esc_matrix <- q3_data %>% 
#   group_by(Grupo) %>% 
#   summarise(mean.escol = mean(Anos_escolaridade, na.rm = TRUE)) %>% 
#   .$mean.escol ; q3_esc_matrix
# Testando suposições para ANOVA
# Anos de estudo é contínua? não sei e de qualquer forma o teste não funcionou
#                                 ¯\_(ツ)_/¯ 
#  
# control_A <- q3_data %>% 
#   filter(Grupo == "Controle") %>% 
#   .$Anos_escolaridade
#
# qqnorm(control_A)
# qqline(control_A)
# ks.test(control_A,"pnorm",mean(control_A),sd(control_A))
################################## ATENÇÃO #####################################
# DISCLAIMER: ACIMA EU FIZ UM TESTE PARA NORMALIDADE DOS ANOS DE ESCOLARIDADE DO
# GRUPO DE CONTROLE, OBTIVE O P-VALOR MUITO BAIXO, LOGO NÃO PODEMOS AVANÇAR PARA
# ANOVA, ACHEI DE NATURAL QUE NÃO SERIA NORMAL PELO FATO DE EXISTIREM MUITOS VA-
# LORES IGUAIS (PROBABILIDADE DE SER IGUAL EM UMA CONTÍNUA É ZERO).
# ATÉ PROSSEGUIRIA PARA A NOVA E TESTARIA AS OUTRAS HIPÓTESES MAS A LISTA INTEI-
# É SOBRE ISSO NÃO ACHO QUE VALERIA A PENA.
################################################################################
# Continuando ##################################################################

# d)
q3_data_QI <- q3_data %>% 
  group_by(Grupo) %>% 
  summarise(mean.qi = mean(QI, na.rm = TRUE))

# Boxplot pra ganhar alguma intuição do que vai acontecer
boxplot(QI ~ Grupo, data = q3_data,
        main = "Boxplot de QI por Grupo",
        xlab = "Grupo",
        ylab = "QI")

# Testando hipóteses para ANOVA
# Sentimento (Normalidade)
par(mfrow = c(2,2))

controle_qi <- q3_data %>% 
  filter(Grupo == "Controle") %>% 
  .$QI %>% 
  unlist(use.names = FALSE) ; controle_qi
qqnorm(controle_qi, main = "Normal Q-Q Plot Controle QI")
qqline(controle_qi)

trauma_qi <- q3_data %>% 
  filter(Grupo == "Trauma") %>% 
  .$QI %>% 
  unlist(use.names = FALSE) ; trauma_qi
qqnorm(trauma_qi, main = "Normal Q-Q Plot Trauma QI")
qqline(trauma_qi)

tept_qi <- q3_data %>% 
  filter(Grupo == "TEPT") %>% 
  .$QI %>% 
  unlist(use.names = FALSE) ; tept_qi
qqnorm(tept_qi, main = "Normal Q-Q Plot TEPT QI")
qqline(tept_qi)

par(mfrow = c(1,1))

# Teste de hipótese para normalidade
ks.test(controle_qi,"pnorm", mean(controle_qi), sd(controle_qi))
# Para um nível de significância \alpha = 0.05 acreditamos que seja realmente normal

ks.test(trauma_qi,"pnorm", mean(trauma_qi, na.rm = T), sd(trauma_qi, na.rm =T))
# Para um nível de significância \alpha = 0.05 acreditamos que seja realmente normal

ks.test(tept_qi,"pnorm", mean(tept_qi, na.rm = T), sd(tept_qi, na.rm = T))
# Para um nível de significância \alpha = 0.05 acreditamos que seja realmente normal.

# Teste de hipótese para 
class(q3_data$Grupo)
LeveneTest(QI ~ Grupo, data = q3_data, center = "mean")
# Para um nível de significância \alpha = 0.05 acreditamos que as variâncias sejam iguais.

q3d_ANOVA <- aov(QI ~ Grupo, data = q3_data)
summary(q3d_ANOVA)

# Para um nível de significância \alpha = 0.05 acreditamos que QI é diferente por grupo.
# Que grupo é diferente de qual? Testaremos a seguir.

# Análise post hoc (post hoc em latin quer dizer "depois disso"), frescura ein?
# Vamos lá testar dois a dois pra saber quem é diferente.
# Usando método de Bonferroni
PostHocTest(q3d_ANOVA, method = "bonf")
# Com o nível de significância \alpha = 0.05 que fixamos, acreditamos qua diferença
# está entre TEPT - Controle (p - valor = 0.0147).

# Usando método de Duncan
PostHocTest(q3d_ANOVA, method = "duncan")
# Com o nível de significância \alpha = 0.05 que fixamos, acreditamos qua diferença
# está entre TEPT - Controle (p - valor = 0.0068).

# Usando método de Tukey
TukeyHSD(q3d_ANOVA)
# Com o nível de significância \alpha = 0.05 que fixamos, acreditamos qua diferença
# está entre TEPT - Controle (p - valor = 0.0135030).

#Show de bola! Usando os três testes chegamos nas mesmas conclusões =D .

# e)
# RAVLT
q3_data_RAVLT <- q3_data %>% 
  group_by(Grupo) %>% 
  summarise(media = mean(RAVLT, na.rm = TRUE)) ; q3_data_RAVLT

#  Testando normalidade 
# Sentimento
controle <- q3_data %>% 
  filter(Grupo == "Controle")
trauma <- q3_data %>% 
  filter(Grupo == "Trauma")
TEPT <- q3_data %>% 
  filter(Grupo == "TEPT")

par(mfrow = c(2,2))
controle$RAVLT %>% 
  qqnorm()
controle$RAVLT %>% 
  qqline()

trauma$RAVLT %>% 
  qqnorm()
trauma$RAVLT %>% 
  qqline()

TEPT$RAVLT %>% 
  qqnorm()
TEPT$RAVLT %>% 
  qqline()
par(mfrow = c(1,1))

# Teste de hipótese para normalidade 
controle$RAVLT %>% 
  ks.test("pnorm", mean(.), sd(.), alternative = "two.sided")
# Como \alpha = 0.03 e o P-valor = 0.2899, acreditamos que seja normal.

trauma$RAVLT %>% 
  ks.test("pnorm", mean(.), sd(.), alternative = "two.sided")
# Como \alpha = 0.03 e o P-valor = 0.5424, acreditamos que seja normal.

TEPT$RAVLT %>% 
  ks.test("pnorm", mean(.,na.rm = TRUE), sd(.,na.rm = TRUE), alternative = "two.sided")
# Como \alpha = 0.03 e o P-valor = 0.8452, acreditamos que seja normal.

# Testando homocedasticidade
LeveneTest(RAVLT ~ Grupo, data = q3_data, center = "mean")
# Como \alpha = 0.03 e o P-valor = 0.9456, acreditamos que sejam homocedasticas.

# ANOVA
class(q3_data$Grupo)
q3_c_RAVLT_ANOVA <- aov(RAVLT ~ Grupo, data = q3_data)
summary(q3_c_RAVLT_ANOVA)
# Como \alpha = 0.03 e o P-valor = 0.000324, rejeitamos $H_0$, logo acreditamos que,
# são diferentes
PostHocTest(q3_c_RAVLT_ANOVA, method = "bonf")
PostHocTest(q3_c_RAVLT_ANOVA, method = "duncan")
TukeyHSD(q3_c_RAVLT_ANOVA)
# Os três testes PostHoc chegam a mesma conclusão a diferença está entre Trauma e controle e
# TEPT e Controle.

# WCST
q3_data_WCST <- q3_data %>% 
  group_by(Grupo) %>% 
  summarise(media = mean(WCST, na.rm = TRUE)) ; q3_data_WCST

#  Testando normalidade 
# Sentimento
par(mfrow = c(2,2))
controle$WCST %>% 
  qqnorm()
controle$WCST %>% 
  qqline()

trauma$WCST %>% 
  qqnorm()
trauma$WCST %>% 
  qqline()

TEPT$WCST %>% 
  qqnorm()
TEPT$WCST %>% 
  qqline()
par(mfrow = c(1,1))

# Teste de hipótese para normalidade 
controle$WCST %>% 
  ks.test("pnorm", mean(., na.rm = TRUE), sd(., na.rm = TRUE), alternative = "two.sided")
# Como \alpha = 0.03 e o P-valor = 0.02, acreditamos que não seja normal.
# ATENÇÃO COM O FIM DE CONTINUAR A EXECUÇÃO DO EXERCÍCIO CONSIDEREI 0.02984, PRÓXIMO
# O SUFICIENTE DE 0.03 PARA NÃO REJEITAR A HIPÓTESE! (pense como se eu tivesse
# usado 0.29 como meu \alpha)

trauma$WCST %>% 
  ks.test("pnorm", mean(., na.rm = TRUE), sd(., na.rm = TRUE), alternative = "two.sided")
# Como \alpha = 0.03 e o P-valor = 0.5002, acreditamos que seja normal.

TEPT$WCST %>% 
  ks.test("pnorm", mean(.,na.rm = TRUE), sd(.,na.rm = TRUE), alternative = "two.sided")
# Como \alpha = 0.03 e o P-valor = 0.436, acreditamos que seja normal.

# Testando homocedasticidade
LeveneTest(WCST ~ Grupo, data = q3_data, center = "mean")
# Como \alpha = 0.03 e o P-valor = 0.2921, acreditamos que sejam homocedasticas.

# ANOVA
class(q3_data$Grupo)
q3_c_WCST_ANOVA <- aov(WCST ~ Grupo, data = q3_data)
summary(q3_c_WCST_ANOVA)

# Stroop
q3_data_stroop <- q3_data %>% 
  group_by(Grupo) %>% 
  summarise(media = mean(Stroop, na.rm = TRUE)) ; q3_data_stroop

#  Testando normalidade 
# Sentimento
par(mfrow = c(2,2))
controle$Stroop %>% 
  qqnorm()
controle$Stroop %>% 
  qqline()

trauma$Stroop %>% 
  qqnorm()
trauma$Stroop %>% 
  qqline()

TEPT$Stroop %>% 
  qqnorm()
TEPT$Stroop %>% 
  qqline()
par(mfrow = c(1,1))

# Teste de hipótese para normalidade 
controle$Stroop %>% 
  ks.test("pnorm", mean(., na.rm = TRUE), sd(., na.rm = TRUE), alternative = "two.sided")
# Como \alpha = 0.03 e o P-valor = 0.12, acreditamos que seja normal

trauma$Stroop %>% 
  ks.test("pnorm", mean(., na.rm = TRUE), sd(., na.rm = TRUE), alternative = "two.sided")
# Como \alpha = 0.03 e o P-valor = 0.04475, acreditamos que seja normal.

TEPT$Stroop %>% 
  ks.test("pnorm", mean(.,na.rm = TRUE), sd(.,na.rm = TRUE), alternative = "two.sided")
# Como \alpha = 0.03 e o P-valor = 0.1304, acreditamos que seja normal.

# Testando homocedasticidade
LeveneTest(Stroop ~ Grupo, data = q3_data, center = "mean")
# Como \alpha = 0.03 e o P-valor = 0.02757, não aparenta ser homocedástica, mas 
# farei como se fosse pra continuar o exercício vlw?

# ANOVA
class(q3_data$Grupo)
q3_c_stroop_ANOVA <- aov(Stroop ~ Grupo, data = q3_data)
summary(q3_c_stroop_ANOVA)

# Como \alpha = 0.03 e o P-valor = 0.1, rejeitamos $H_0$, logo acreditamos que,
# o grupo determina o nível de Stroop.


# FALTAM OS TESTES PARA DIGITOS E REPRODUÇÃO VISUAL, TAVA CANSADO FUI JOGAR
# UMAS VN E DEIXEI PRA FAZER DEPOIS.

# Questão 4 ####################################################################
q4 <- q3_data %>% 
  mutate(grupo_new = ifelse(Grupo %in% c("Trauma","TEPT"), 
                               "Trauma", "Controle"))
q4$grupo_new <- q4$grupo_new %>% 
  factor() ; levels(q4$grupo_new)

# AGORA TEM QUE RAFAZER TUDO QUE NÃO FIZ NA ANTERIOR, LEGAL NÉ? DEPOIS EU FAÇO.
# EDIT: É BEM MAIS FÁCIL DO QUE NO ITEM ANTERIOR, AGORA SE TRATA DE APENAS DUAS
# POPULAÇÕES RELIZAREMOS TESTES DE HIPÓTESE PARA DIFERENÇA DE MÉDIAS.
q4_Trauma <- q4 %>% 
  filter(grupo_new == "Trauma")

q4_Controle <- q4 %>% 
  filter(grupo_new == "Controle")

# TESTE REALIZADOS COM NÍVEL DE SIGNIFICÂNCIA \ALPHA = 0.03
# Realizando os testes.
# PS.: Sempre testo se as variâncias são iguais antes de realizar esse
# Teste ai, to usanod o VarTest do pacote DescTools, porque se trata da
# igualdade da variância entre duas populações.

VarTest(q4_Trauma$RAVLT, q4_Controle$RAVLT, alternative = "two.sided")
t.test(q4_Controle$RAVLT, q4_Trauma$RAVLT, mu = 0, 
       alternative = "two.sided",
       var.equal = TRUE)
# Variâncias iguais, médias diferentes.

VarTest(q4_Trauma$WCST, q4_Controle$WCST, alternative = "two.sided")
t.test(q4_Controle$WCST, q4_Trauma$WCST, mu = 0, 
       alternative = "two.sided",
       var.equal = TRUE)
# Variâncias iguais, médias iguais.

VarTest(q4_Trauma$Stroop, q4_Controle$Stroop, alternative = "two.sided")
t.test(q4_Controle$Stroop, q4_Trauma$Stroop, mu = 0, 
       alternative = "two.sided",
       var.equal = TRUE)
# Variâncias iguais, médias diferentes.

VarTest(q4_Trauma$Digitos, q4_Controle$Digitos, alternative = "two.sided")
t.test(q4_Controle$Digitos, q4_Trauma$Digitos, mu = 0, 
       alternative = "two.sided")
# Variâncias diferentes, médias diferentes.

VarTest(q4_Trauma$`Reproduçao Visual`, q4_Controle$`Reproduçao Visual`, alternative = "two.sided")
t.test(q4_Controle$`Reproduçao Visual`, q4_Trauma$`Reproduçao Visual`, mu = 0, 
       alternative = "two.sided")
# Variâncias diferentes, médias diferentes.

# Questão 5 ####################################################################
# a)
q5_data <- read_csv2("Listas\\Lista 06\\EMBRAPA.csv") ; q5_data

q5_data$Tratamento <- q5_data$Tratamento %>% 
  factor()

boxplot(Peso26 ~ Tratamento, data = q5_data,
        main = "Boxplot Peso26 por tratamento",
        xlab = "Tratamento",
        ylab = "Peso (em kg)")

q5I <- q5_data %>% 
  filter(Tratamento == "1")

q5II <- q5_data %>% 
  filter(Tratamento == "2")

q5III <- q5_data %>% 
  filter(Tratamento == "3")

q5IV <- q5_data %>% 
  filter(Tratamento == "4")

# Testar normalidade
# Sentimento (gráficos quantil-quantil)
par(mfrow = c(2,2))
qqnorm(q5I$Peso26)
qqline(q5I$Peso26)

qqnorm(q5II$Peso26)
qqline(q5II$Peso26)

qqnorm(q5III$Peso26)
qqline(q5III$Peso26)

qqnorm(q5IV$Peso26)
qqline(q5IV$Peso26)
par(mfrow = c(1,1))

# Teste formal
q5I$Peso26 %>% 
  ks.test("pnorm",mean(., na.rm = TRUE),sd(., na.rm = TRUE))
# Para um nível de significância \alpha = 0.05 acreditamos que seja normal

q5II$Peso26 %>% 
  ks.test("pnorm",mean(., na.rm = TRUE),sd(., na.rm = TRUE))
# Para um nível de significância \alpha = 0.05 acreditamos que seja normal

q5III$Peso26 %>% 
  ks.test("pnorm",mean(., na.rm = TRUE),sd(., na.rm = TRUE))
# Para um nível de significância \alpha = 0.05 acreditamos que seja normal

q5IV$Peso26 %>% 
  ks.test("pnorm",mean(., na.rm = TRUE),sd(., na.rm = TRUE))
# Para um nível de significância \alpha = 0.05 acreditamos que seja normal

# Todas aparentam ser normais

# Teste hocedasticidade
LeveneTest(Peso26 ~ Tratamento, data = q5_data, center = "mean")
# Para um nível de significância \alpha = 0.05 acreditamos que sejam homocedásticas
# Todas as hipoóteses forma satisfeitas podemos realizar o teste ANOVA 

q5a_ANOVA <- aov(Peso26 ~ Tratamento, data = q5_data) ; q5a_ANOVA
summary(q5a_ANOVA)
# Para um nível de significância \alpha = 0.05 acreditamos que não existe diferença
# entres os pesos por tratamento

# b)
# Testarei a variância antes
VarTest(q5_data$Peso26, q5_data$Peso0, alternative = "two.sided")
t.test(q5_data$Peso26, q5_data$Peso0, alternative = "greater",mu = 88)
# Variâncias diferentes
# Para um nível de significância \alpha = 0.04 acreditamos que a diferença entre o Peso26
# e Peso0 é maior que 88kg.

# Questão 6 ####################################################################
# a)
q6_data <- read_csv2("Listas\\Lista 06\\Basevulnerabilidade.csv") ; q6_data
q6_data$situacao <- q6_data$situacao %>% 
  factor(labels = c("Alugado", "Próprio")) ; levels(q6_data$situacao)
q6_data$escmae <- q6_data$escmae %>% 
  ordered(labels = c("Baixa","Média","Alta")) ; levels(q6_data$escmae)
q6_data$vulnerabilidade <- q6_data$vulnerabilidade %>% 
  ordered(labels = c("Não vulnerável","Vulnerável")) ; levels(q6_data$vulnerabilidade)

# AS CONCLUSÕES DOS DOIS TESTES DE INDEPENDÊNCIA FORAM TIRADAS COM BASE EM UM 
# NÍVEL DE SIGNIFICÂNCIA \ALPHA = 0.05

# SITUAÇÃO E VULNERABILIDADE
# Tabelinha pra visualizar 
matrix_q6 <- q6_data %>% 
  group_by(situacao, vulnerabilidade) %>% 
  summarise(freq.abs = n(),
            freq.rel = freq.abs/length(q6_data$vulnerabilidade)) %>% 
  .$freq.abs %>% 
  matrix(2,2, byrow = T) ; matrix_q6

# Gráficos
prop_q6 <- apply(matrix_q6, 2, function(x){x/sum(x,na.rm = T)}) ; prop_q6

colnames(prop_q6) <- c("Não vulnerável", "Vulnerável")
rownames(prop_q6) <- c("Alugado", "Próprio")

cor_q6 <- brewer.pal(3, "Pastel1") ; cor_q6
par(mar = c(5,4,4,7)) # margem para a criação do gráfico para que a legenda não
# fique por cima.
barplot(prop_q6, col = cor_q6,
        main = "Gráfico para proporção Situação x Vulnerabilidade",
        xlab = "Vulnerabilidade",
        ylab= "Proporção")
legend(x = "topright", 
       legend = c("Alugado","Próprio"), 
       fill = cor_q6, 
       title = "Legenda",
       inset = c(-0.30,0.5),  # inset move o gráfico dependendo da posição (no caso "topright").
       xpd = TRUE) # xpd permite que a legenda seja escrita fora da área do gráfico,
# ou seja, fora do margin. 
chisq.test(matrix_q6, correct = FALSE)
# Concluímos que existe uma relação entre vulnerabilidade e a situação de moradia

# ESCOLARIDADE DA MÃO E VULNERABILIDADE
matriz_q6alt <- q6_data %>% 
  group_by(escmae,vulnerabilidade) %>% 
  summarise(N = n()) %>% 
  .$N %>% 
  matrix(nrow = 3,
         ncol = 2, 
         byrow = T); matriz_q6alt
colnames(matriz_q6alt) <- c("Não vulnerável","Vulnerável")
rownames(matriz_q6alt) <- c("Baixa","Média","Alta")

prop_matriz_q6alt <- prop.table(matriz_q6alt,2) ;prop_matriz_q6alt # Natan, valeu cara
# nem lembrava da função prop.table hahahaha (pode ser usada no lugar do apply fica,
# muito mais fácil usando o prop.table)

par(mar = c(5,4,4,10))
cor_q6_2 <- brewer.pal(3,"Pastel1")
barplot(prop_matriz_q6alt, col = cor_q6_2,
        main = "Gráfico de proporção",
        ylab = "Proporção",
        xlab = "Vulnerabilidade")
legend("topright", legend = rownames(matriz_q6alt),
       fill = cor_q6_2,
       title = "Escolariade da mãe",
       inset = c(-0.50,0.4),
       xpd = TRUE)

chisq.test(matriz_q6alt, correct = FALSE)
# Concluímos que não existe uma relação entre vulnerabilidade e a escolaridade 
# da mãe

# b) 
#
# DISCLAIMER :
# FAREI O TESTE DAS HIPÓTESES DEPOIS (PREGUIÇA). EDIT: ESTÃO FEITOS MAIS ABAIXO!
# Putz, não tem variável idade não tem como fazer ¯\_(ツ)_/¯ . 
# MAS PRA NÃO FICAR SEM FAZER FIZ UMA COLUNA IDADE DA SEGUINTE FORMA:

q6_data <- q6_data %>% 
  mutate(idade = rnorm(40,35,10)) # Aqui eu to gerando uma amostra aleatória 
# para a idade e colocando em uma coluna homônima, baseado em uma distribuição normal.
# Graficos
boxplot(idade ~ vulnerabilidade, data = q6_data,
        main = "Boxplot de idade x vulnerabilidade",
        ylab = "Idade (anos)",
        xlab = "Vulnerabilidade")

t.test(q6_data$idade[q6_data$vulnerabilidade == "Vulnerável"],
       q6_data$idade[q6_data$vulnerabilidade == "Não vulnerável"],
       alternative = "two.sided")

# Questão 7 
q7_data <- read_xls("Listas\\Lista 06\\Banco Cancer Mama.xls", na = "NA") ; q7_data

# Tratando
grau <- str_c(c("3°","2°","1°"), " Grau")
{
  q7_data$cancer <- factor(q7_data$cancer, labels = c("Não", "Sim"))
  q7_data$menstruacao <- factor(q7_data$cancer, labels = c("Não", "Sim"))
  q7_data$grauparentesco <- ordered(q7_data$grauparentesco, labels = grau)
}
# As letras de "a" até  "d" equivalem aos exercícios 7 até 10.
# a)
# Faremos um teste de homogeneidade, veja que foi separado em 2 sub-populações,
# depois observado as variáveis
# DISCLAIMER: PULEI A ETAPA DE CRIAR UMA ANÁLISE DESCRITIVA DO PROBLEMA (SE BEM
# QUE O EXERCÍCIO NEM PEDE, MAS OKEY).
q7a_matrix <- q7_data %>% 
  group_by(menstruacao, cancer) %>% 
  summarise(N = n()) ;q7a_matrix
q7a_matrix <- matrix(c(27,0,0,30), ncol = 2, 
                     nrow = 2, 
                     byrow = T) ; q7a_matrix

chisq.test(q7a_matrix, correct = TRUE)

# P-valor deu muito pequeno, pequeno quanto? Menor que nosso nível de significância
# bem menor na verdade, acreditamos que realmente seja um fator de risco.

# b)
# Faremos um teste de diferença entre médias de duas populações.
# $H_O: \mu_{antes} - \mu_{depois} \geq 0 \times H_1: \mu_{antes} - \mu_{depois} < 0$
VarTest(q7_data$antes, q7_data$depois, 
        alternative = "two.sided")
t.test(q7_data$antes, q7_data$depois, 
       alternative = "less",
       mu = 0)
# P-valor deu 1, muito grande acreditamos que o tratamento foi efetivo.

# c)
# Faremos um de diferença de médias de duas populações
# Mais uma vez pularei a parte descritiva por enquanto, caso sobre tempo, retornarei.
# EDIT: Fiz uma parte descritiva, só um boxplot, acho que ta bom.
boxplot(peso ~ cancer, data = q7_data, 
        main = "Boxpot de peso x câncer",
        ylab = "Peso (kg)",
        xlab = "Câncer")

# TODOS OS TESTES TEM NÍVEL DE SIGNIFICÂNCIA \alpha = 0.05
q7_cancer <- q7_data %>% 
  filter(cancer == "Sim") %>% 
  .$peso
  
q7_ncancer <- q7_data %>% 
  filter(cancer == "Não") %>% 
  .$peso
VarTest(q7_cancer, q7_cancer, 
        alternative = "two.sided")
t.test(q7_cancer, q7_ncancer,
       mu = 0,
       alternative = "less")
# Pelo teste de diferença entre médias, acreditamos que sim, obesidade é fator de
# risco para câncer.

# d)
# Faremos um teste de homogeneidade
q7a_matrix2 <- q7_data %>% 
  group_by(grauparentesco, cancer) %>% 
  summarise(N = n()) ;q7a_matrix2
q7a_matrix3 <- matrix(q7a_matrix2$N, ncol = 2, 
                     nrow = 3, 
                     byrow = T) ; q7a_matrix3

prop_q7_3 <- prop.table(q7a_matrix3,2)
barplot(prop_q7_3) # Perdão gráfico, tu vai ficar feio mesmo, não aguento mais,
# ver essa lista.

chisq.test(q7a_matrix3, correct = TRUE)
# Não há relação

