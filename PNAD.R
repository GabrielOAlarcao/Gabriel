library(PNADcIBGE)
library(dplyr)
library(fastDummies)

# Realizando o download dos dados
PNADc <- data.frame()

for (i in 2015:2016){
  for (j in 1:4){
    dadosPNADc <- get_pnadc(year=i, quarter= j, design = F)
    PNADc <- rbind(PNADc, dadosPNADc)
  }
}



# Filtrando para algumas variáveis de interesse
PNAD <- PNADc %>%
  select(Ano, Trimestre, UF, UPA, Estrato, ID_DOMICILIO, 
         V1008, # Número de Seleção do Domicílio
         V1014, # Painel
         V1022, # Situação do Domicílio (Urbana ou Rural)
         V2001, # Número de pessoas no domicílio
         V2005, # Condição no Domícilio 
         V2007, # Sexo
         V2009, # Idade do morador
         V2010, # Cor ou raça
         V3001, # Sabe ler e escrever
         VD4008, # Posição na ocupação no trabalho principal
         VD4001, # Condição em relação à força de trabalho
         VD4002, # Condição de ocupação
         VD3004, # Nível de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade) padronizado para o Ensino fundamental com duração de 9 anos
         VD3005, # Anos de estudo (pessoas de 5 anos ou mais de idade) padronizado para o Ensino fundamental com duração de 9 anos
         VD4017, # Rendimento mensal efetivo do trabalho principal
         V4039) %>% # Quantas horas ... trabalhava normalmente, por semana, nesse trabalho principal
  filter(UF %in% c("Minas Gerais", "São Paulo", "Rio de Janeiro", "Espírito Santo")) %>%
  mutate_at(vars(V1022), function(x) ifelse(x == "Urbana", 0, 1)) %>%
  mutate_at(vars(V2005), function(x) ifelse(x == 1, 1, 0)) %>% # Criando variável para chefe do Domícilio
  mutate_at(vars(V2007), function(x) ifelse(x == 1, 1, 0)) %>% # 1 para Homem, 0 para Mulher
  mutate_at(vars(V2005), function(x) ifelse(x == 1, 1, 0)) %>%
  mutate(VD3004 = case_when(VD3004 %in% c("Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente") ~ "Sem_instrucao_ou_Fundamental_incompleto",
                            VD3004 %in% c("Fundamental completo ou equivalente", "Médio incompleto ou equivalente") ~ "Fundamental_completo_ou_Medio_incompleto",
                            VD3004 %in% c("Médio completo ou equivalente", "Superior incompleto ou equivalente") ~ "Medio_completo_ou_Superior_incompleto",
                            VD3004 == c("Superior Completo") ~ "Superior_completo")) %>%
  mutate_at(vars(VD4001), function(x) ifelse(x == 1, 1, 0)) %>% # 1 para Empregado(a), 0 para Desempregado(a)
  #mutate(Domicilio = UPA+V1008) %>% # Identificador de domicílio
  mutate_at(vars(V3001), function(x) ifelse(x == 1, 0, 1)) %>% # O para saber ler, 1 para não saber
  mutate_at(vars(V3001), function(x) ifelse(x == 1, 0, 1)) %>%
  mutate(filho_filha = ifelse(V2005==4 | V2005==5| V2005==6 , 1, 0)) %>%
  mutate(Domicilio = paste0(UPA, V1008)) %>%
  mutate(Data = paste0(Ano, Trimestre)) %>%
  rename(Idade = V2009,
         Raca = V2010,
         Sabe_ler_escrever = V3001,
         Educ = VD3005,
         Ocupacao = VD4008,
         Salario = VD4017,
         Regiao = V1022,
         Horas_Trabalhadas = V4039) %>%
  mutate(UF = case_when(UF %in% c("Minas Gerais") ~ "MG",
                        UF %in% c("São Paulo") ~ "SP",
                        UF %in% c("Rio de Janeiro") ~ "RJ",
                        UF %in% c("Espírito Santo") ~ "ES")) 
  dummy_cols(., select_columns = c("UF", Raca, "Nivel_Escolaridade", "VD4008"))


## Criando algumas variáveis auxiliares para a análise dos dados de painel
# Dummy se a pessoa é casada ou não
Z <- PNAD %>%
  select(Data, V2005, Domicilio, ID_DOMICILIO) %>%
  group_by(Domicilio, Data) %>%
  transform(V2005_1 = lead(V2005)) %>%
  mutate(casado = ifelse(V2005==2|V2005==3|V2005==1 & V2005_1==2|V2005==1  & V2005_1==3, 1, 0)) %>%
  group_by(ID_DOMICILIO) %>%
  summarise(
    nLinhas = max(casado))


nrowID <- PNAD %>%
  group_by(ID_DOMICILIO) %>%
  summarise(count = n())
nCount <- as.integer(nrowID$count)

# Replicando a variável para toda base
Casados <- rep(Z$nLinhas, times = nCount)
PNAD$Casados <- Casados

#Número de filhos por domicílio
X <- PNAD %>%
  group_by(Domicilio, Data) %>%
  summarise(
    num_filhos = sum(Filho_Filha)
  ) %>%
  group_by(Domicilio) %>%
  summarise(
    filhos_domicilio = max(num_filhos))

nrowDomicilio <- PNAD %>%
  group_by(Domicilio) %>%
  summarise(count = n())

# Replicando a variável para toda base
nCount1 <- as.integer(nrowDomicilio$count)
Filhos_Domicilio <- rep(X$filhos_domicilio, times = nCount1)
PNAD$Filhos_Domicilio <- Filhos_Domicilio

#Número de filhos com menos de 6 anos
Filhos_06 <- ifelse(PNAD$V2005==4 | PNAD$V2005==5|PNAD$V2005==6 & PNAD$Idade <6 , 1, 0)
PNAD$Filhos_06 <- Filhos_06

Y <- PNAD %>%
  group_by(Domicilio, Data) %>%
  summarise(
    num_filhos06 = sum(Filhos_06)
  ) %>%
  group_by(Domicilio) %>%
  summarise(
    filhos06_domicilio = max(num_filhos06)
  )
nrowDomicilio <- PNAD %>%
  group_by(Domicilio) %>%
  summarise(count = n())

# Replicando a variável para toda base
nCount1 <- as.integer(nrowDomicilio$count)
Filhos06_Domicilio <- rep(Y$filhos06_domicilio, times = nCount1)
PNAD$Filhos06_Domicilio <- Filhos06_Domicilio

#Número de pessoas por domicílio
Q <- PNAD %>%
  group_by(Domicilio, Data) %>%
  summarise(
    num_pessoas = n()
  ) %>%
  group_by(Domicilio) %>%
  summarise(
    pessoas_domicilio = max(num_pessoas)
  )
nrowPessoas <- PNAD %>%
  group_by(Domicilio) %>%
  summarise(count = n())

# Replicando a variável para toda base
nCount2 <- as.integer(nrowPessoas$count)
Pessoas_Domicilio <- rep(Q$pessoas_domicilio, times = nCount2)
PNAD$Pessoas_Domicilio <- Pessoas_Domicilio

#Renda do domicílio
W <- PNAD %>%
  group_by(Domicilio, Data) %>%
  summarise(
    renda_dom_periodo = sum(Salario)
  ) %>%
  group_by(Domicilio) %>%
  summarise(
    renda_dom = max(renda_dom_periodo)
  )

nrowDomicilio <- PNAD %>%
  group_by(Domicilio) %>%
  summarise(count = n())

# Replicando a variável para toda base
nCount1 <- as.integer(nrowDomicilio$count)
Renda_Dom <- rep(W$renda_dom, times = nCount1)
PNAD$Renda_Dom <- Renda_Dom
PNAD$Renda_Dom[is.na(PNAD$Renda_Dom)] <- 0

#Renda do cônjuge
renda_conjuge <- ifelse(PNAD$Casados==1 & PNAD$Sexo==1, PNAD$Renda_Dom- PNAD$Salario, PNAD$Salario)
Renda_Conjuge <- abs(renda_conjuge)
PNAD$Renda_Conjuge <- Renda_Conjuge

### --------------------------------------------------------------------------------------------------
#Filtar apenas para os assalariados
PNAD_Sudeste_Assalariados <- PNAD %>%
  filter(!log(salario) %in% c(-Inf, NA, NaN))

# ---------------------------------------
summary(regressao1 <- lm(log(salario) ~ Educ + Idade + Idade^2, data = PNAD_Sudeste_Assalariados))

# ---------------------------------------
summary(regressao2 <- lm(log(salario) ~ Educ + Idade + Idade^2 + Chefe_Domicilio + Sexo + Casados, data = PNAD_Sudeste_Assalariados))

# ---------------------------------------
summary(regressao3 <- lm(log(salario) ~  Idade + Idade^2 + Chefe_Domicilio + Sexo + Regiao + Casados +  Nivel_Escolaridade_Fundamental_completo_ou_Medio_incompleto + Nivel_Escolaridade_Medio_completo_ou_Superior_incompleto +   Nivel_Escolaridade_Superior_completo, data = PNAD_Sudeste_Assalariados))

# ---------------------------------------
summary(regressao4 <- lm(log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas, data = PNAD_Sudeste_Assalariados))

# Teste de especificação --------------------------------------
library(lmtest)
resettest(regressao4, power = 2:3, type = "regressor")

# Teste de endogeneidade --------------------------------------
regressao4 <- lm(log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas, data = PNAD_Sudeste_Assalariados)
#Regressão sobre educ
summary(Regressao_Educ <- lm(Educ ~Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas + Sabe_ler_escrever + Pessoas_Domicilio, data = PNAD_Sudeste_Assalariados))

# Obtendo os resíduos (v2) --------------------------------------
v2 <- Regressao_Educ$residuals
PNAD_Sudeste_Assalariados$v2 <- v2

# Reestimando a regressão com v2 --------------------------------------
summary(regressao5 <- lm(log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas + v2, data = PNAD_Sudeste_Assalariados))

library(lmtest)
library(sandwich)
# Teste robusto para heterocedastidade --------------------------------------
coeftest(regressao5 , vcov = vcovHC(regressao5, type = "HC0"))

# Regressão por Variáveis Instrumentais --------------------------------------
library(AER)
summary(modeloIV <- ivreg(log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas | Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas + Sabe_ler_escrever + Pessoas_Domicilio , data = PNAD_Sudeste_Assalariados))

# Comparando com o modelo por MQO --------------------------------------
summary(modeloMQO <- lm(log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas, data = PNAD_Sudeste_Assalariados))

# Teste de restrições sobreidentificadoras --------------------------------------

# MQ2E
modeloIV <- ivreg(log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas |Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas + Sabe_ler_escrever + Pessoas_Domicilio , data = PNAD_Sudeste_Assalariados)

# P-valor da distribuição qui-quadrado --------------------------------------
summary(modeloIV, diagnostics = TRUE)

# Estimando para efeitos fixos --------------------------------------
library(plm)
summary(modeloEF <- plm(log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas, model = "within", index = c("Data"), data = PNAD_Sudeste_Assalariados))

# Medindo os efeitos através do tempo para Educ e Superior Completo --------------------------------------
summary(modeloEF1 <- plm(log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Educ:Data_Periodo9  + Nivel_Escolaridade_Superior_completo:Data_Periodo9 + Horas_Trabalhadas , model = "within", index = c("Data"), data = PNAD_Sudeste_Assalariados))

# Estimando para efeitos aleatórios --------------------------------------
summary(modeloEA <- plm(log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas, model = "random", index = c("ID_DOMICILIO","Data"), data = PNAD_Sudeste_Assalariados))

# Teste de Hausman
phtest(modeloEF, modeloEA)

# Teste para autocorrelação dos resíduos --------------------------------------
pbgtest(modeloEF)

#Estimando por primeiras diferenças --------------------------------------
summary(modeloPD <- plm(log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas, model = "fd", index = c("Data"), data = PNAD_Sudeste_Assalariados))


# Modelo de Efeitos aleatórios Correlacionados --------------------------------------
summary(modeloEAC <- plm(log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio + Horas_Trabalhadas, model = "between", index = c("ID_DOMICILIO", "Data"), data = PNAD_Sudeste_Assalariados))

# Estimação por modelo logit --------------------------------------
summary(modeloLogit <- glm(Empregado ~ Educ + Filhos06_Domicilio + Regiao + Casados  + Renda_Dom + Idade + Idade^2 + Sexo  + V2010_Preta + V2010_Parda  + Nivel_Escolaridade_Superior_completo + UF_MG + UF_ES + UF_RJ, family = binomial(link = "logit"), data = PNAD_Sudeste))


# Calculando os efeitos marginais (Efeito parcial na média) -------------------------------
library(mfx)
logitmfx(Empregado ~ Educ + Filhos06_Domicilio + Regiao + Casados  + Renda_Dom + Idade + Idade^2 + Sexo  + Nivel_Escolaridade_Superior_completo + UF_MG + UF_ES + UF_RJ, data = PNAD_Sudeste)


# Modelo Tobit --------------------------------------
library(AER)
summary(modeloTobit <- tobit(formula = log(salario) ~ Educ + Idade + Idade^2 +  Chefe_Domicilio + Sexo + Regiao + Casados + UF_MG + UF_ES + UF_RJ + Nivel_Escolaridade_Superior_completo + Filhos06_Domicilio , data = PNAD_Sudeste))


# Modelo de Poisson -------------------------------
PNAD_Sudeste$Educ[is.na(PNAD_Sudeste$Educ)] <- 0
summary(modeloPoisson <- glm(Filhos_Domicilio ~ Educ + Idade + Idade^2 + Nivel_Escolaridade_Sem_instrucao_ou_Fundamental_incompleto + Nivel_Escolaridade_Superior_completo + Casados + Renda_Dom + Regiao , family = poisson(link = "log"), data = PNAD_Sudeste))

#Análise de superdispersão
summary(modeloQuasiPoisson <- glm(Filhos_Domicilio ~ Educ + Idade + Idade^2 + Nivel_Escolaridade_Sem_instrucao_ou_Fundamental_incompleto + Nivel_Escolaridade_Superior_completo + Casados + Renda_Dom + Regiao, family = quasipoisson, data = PNAD_Sudeste))