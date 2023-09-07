### TCC ----------------------------------------------
### Estudo de análise de choques no preço da gasolina e da energia elétrica e seus
### efeitos sobre a inflação, produção industrial e os juros reais. Análise feita atráves
### de modelos VAR.


rm(list = ls())
cat("\014")

library(dplyr)
library(tidyverse)
library(tidyr)
library(vars)
library(ggplot2)
library(readxl)
library(sidrar)
library(tseries)
library(aTSA)
library(forecast)
library(treemapify)
library(ggplot2)
library(rbcb)


setwd("C:/Users/gabri/OneDrive/Documentos/TCC")

acumular <- function(serie){
  factor <- data.frame("serie" = 1+(serie/100))
  
  acumulado <- (factor*lag(factor,1)*lag(factor,2)*lag(factor,3)*lag(factor,4)*
                  lag(factor,5)*lag(factor,6)*lag(factor,7)*lag(factor,8)*
                  lag(factor,9)*lag(factor,10)*lag(factor,11)-1)
  
  return(acumulado$serie + 1)
}

# 1) Tratamento dos dados ------------------------------------------------------
# Gasolina
gasolina_2001 = read_excel("mensal-brasil-2001-a-2012.xlsx", sheet = "BRASIL", range = "A13:E289") %>%
  filter(PRODUTO == "GASOLINA COMUM") %>%
  dplyr::select(c(1,5)) %>%
  rename(data_tidy = 1,
         p_gasolina = 2)

gasolina_2013 = read_excel("mensal-brasil-desde-jan2013.xlsx", sheet = "BRASIL - DESDE JANEIRO DE 2013", 
                           range = "A17:E716") %>%
  filter(PRODUTO == "GASOLINA COMUM") %>%
  dplyr::select(c(1,5)) %>%
  dplyr::add_row(., `MÊS` = as.Date("2020-09-01"),`PREÇO MÉDIO REVENDA` = 4.237, .before = 93) %>%
  rename(data_tidy = 1,
         p_gasolina = 2)

gasolina = rbind(gasolina_2001, gasolina_2013)


# Tarifa de energia elétrica industrial
energia_industrial = read_excel("data.xlsx", sheet = "Export", range = "C2:D272") %>%
  filter(Mes != is.na(Mes) & Mes != "Total") %>%
  mutate(Mes = seq.Date(from = as.Date("2003-01-01"), to = as.Date("2022-10-01"), by = "months")) %>%
  rename(data_tidy = 1,
         p_energia = 2)

# PLD - Preço de Liquidação de Diferenças
pld_energia = read_excel("./Historico_do_Preco_Medio_Mensal_-_janeiro_de_2001_a_novembro_de_2022.xls") %>%
  mutate(energia = rowMeans(.[2:5])) %>%
  rename(data_tidy = 1) %>%
  dplyr::select(data_tidy, energia)


# Série de IPCA 
ipca = get_sidra(api = '/t/1737/n1/all/v/63/p/all/d/v63%202') %>%
  dplyr::select("Mês (Código)",
         Valor) %>%
  rename(data_tidy = 1) %>%
  mutate(data_tidy = seq.Date(from = as.Date("1979-12-01"), length.out = nrow(.), by = "months")) %>%
  rename(ipca = 2) %>%
  mutate(ipca_acum = (acumular(ipca)-1)*100)

ipca_acum = read_excel("C:/Users/gabri/OneDrive/Documentos/TCC/series.xlsx",
                  sheet = "Sheet 1",
                  range = "A1:F229") %>% dplyr::select(c(1,6)) %>% rename(ipca_acum = 2)

ipca = left_join(ipca, ipca_acum)

# Série da PIM
pim = get_sidra(api = '/t/8159/n1/all/v/11600/p/all/c544/129314/d/v11600%205') %>%
  dplyr::select("Mês (Código)",
         Valor) %>%
  rename(data_tidy = 1) %>%
  mutate(data_tidy = seq.Date(from = as.Date("2002-01-01"), length.out = nrow(.), by = "months")) %>%
  rename(pim = 2)

# Série Selic ex-post
selic = get_series(4390, as = "data.frame") %>%
  filter(date >= "2001-01-01") %>%
  rename(data_tidy = date,
         selic = 2) %>%
  mutate_at(vars(selic), ~acumular(.)) %>%
  filter(data_tidy >= "2002-01-01") %>%
  left_join(., ipca) %>%
  #mutate(ipca = (ipca/100)+1) %>%
  mutate(selic = ((selic/(1+ipca_acum/100))-1)*100) %>% dplyr::select(-c(ipca, ipca_acum))

# Selic Nominal
selic_n = get_series(4189, as = "data.frame") %>%
  filter(date >= "2002-01-01") %>%
  rename(data_tidy = date,
         selic_n = 2)

# Selic mensal
selic_m = get_series(4390, as = "data.frame") %>%
  filter(date >= "2001-01-01") %>%
  rename(data_tidy = date,
         selic = 2)

## Agrupando as séries
gasolina_1 = left_join(gasolina, ipca, by = "data_tidy") %>%
  left_join(., pim, by = "data_tidy") %>%
  left_join(., selic, by = "data_tidy") %>%
  filter(data_tidy > "2001-12-01") %>%
  #mutate(selic = selic - ipca) %>%
  na.omit()

energia_1 = left_join(energia_industrial, ipca, by = "data_tidy") %>%
  left_join(., pim, by = "data_tidy") %>%
  left_join(., selic, by = "data_tidy") %>%
  #mutate(selic = selic - ipca) %>%
  na.omit()

# Séries Anualizadas
pim = get_sidra(api = '/t/8159/n1/all/v/11600/p/all/c544/129314/d/v11600%205') %>%
  dplyr::select("Mês (Código)",Valor) %>%
  rename(data_tidy = 1) %>%
  mutate(data_tidy = seq.Date(from = as.Date("2002-01-01"), length.out = nrow(.), by = "months")) %>%
  rename(pim = 2)

fontes = left_join(energia_industrial, gasolina)
#fontes[213,3] = fontes[212,3]

fontes = fontes %>%
  filter(data_tidy <= "2022-06-01") %>%
  mutate_at(vars(p_gasolina, p_energia), function(x) rollmean(x, k=12, fill = NA, align = "right")) %>%
  mutate_at(vars(p_gasolina, p_energia), function(x) (x/lag(x,12)-1)*100)

# gasolina = gasolina %>%
#   mutate(p_gasolina = rollmean(p_gasolina, k=12, fill = NA)) %>%
#   mutate_at(vars(p_gasolina), function(x) (x/lag(x,12)-1)*100)
# 
# energia = energia_industrial %>%
#   mutate(p_energia = rollmean(p_energia, k=12, fill = NA)) %>%
#   mutate_at(vars(p_energia), function(x) (x/lag(x,12)-1)*100)

ipca = get_sidra(api = '/t/1737/n1/all/v/63/p/all/d/v63%202') %>%
  dplyr::select("Mês (Código)",
                Valor) %>%
  rename(data_tidy = 1) %>%
  mutate(data_tidy = seq.Date(from = as.Date("1979-12-01"), length.out = nrow(.), by = "months")) %>%
  rename(ipca = 2) %>%
  mutate(ipca_acum = (acumular(ipca)-1)*100)


selic = get_series(4390, as = "data.frame") %>%
  #filter(date >= "2001-01-01") %>%
  rename(data_tidy = date,
         selic = 2) %>%
  mutate_at(vars(selic), ~acumular(.)) %>%
  #filter(data_tidy >= "2002-01-01") %>%
  left_join(., ipca) %>%
  #mutate(ipca = (ipca/100)+1) %>%
  mutate(selic = ((selic/(1+ipca_acum/100))-1)*100) %>% dplyr::select(-c(ipca, ipca_acum))

series_anual = left_join(fontes,ipca) %>%
  left_join(.,selic) %>%
  left_join(.,pim) %>%
  mutate(data_tidy = as.Date(data_tidy)) %>%
  filter(data_tidy <= "2021-12-01" & data_tidy >= "2005-01-01") %>%
  dplyr::select(-ipca) %>%
  openxlsx::write.xlsx(.,"C:/Users/gabri/OneDrive/Documentos/TCC/series_anual.xlsx",
                       col.names = T, row.names = F, overwrite = T)


### Exportando dataset para o EViews
series_1 = left_join(gasolina, energia) %>%
  filter(data_tidy <= "2021-12-01") %>%
  openxlsx::write.xlsx(.,"C:/Users/gabri/OneDrive/Documentos/TCC/series_1.xlsx",
                      col.names = T, row.names = F, overwrite = T)

series_2 = left_join(gasolina_1, energia_1) %>%
  mutate_at(vars(-c(data_tidy, ipca, selic)), function(x) log(x/lag(x))*100) %>%
  filter(data_tidy <= "2021-12-01" & data_tidy >= "2003-01-01") %>%
  openxlsx::write.xlsx(.,"C:/Users/gabri/OneDrive/Documentos/TCC/series_2.xlsx",
                       col.names = T, row.names = F, overwrite = T)

# PLD
series_4 = left_join(gasolina_1, pld_energia) %>%
  mutate_at(vars(c(p_gasolina, pim)), function(x) log(x/lag(x))*100) %>%
  filter(data_tidy <= "2021-12-01") %>%
  openxlsx::write.xlsx(.,"C:/Users/gabri/OneDrive/Documentos/TCC/series_4.xlsx",
                       col.names = T, row.names = F, overwrite = T)

## Séries com PLD ao invés da tarifa de energia e selic nominal
series_3 = left_join(gasolina, ipca) %>%
  left_join(., pim) %>%
  left_join(.,pld_energia) %>%
  left_join(., selic_n) %>%
  mutate_at(vars(c(p_gasolina, pim)), function(x) log(x/lag(x))*100) %>%
  filter(data_tidy >= "2001-12-01" & data_tidy <= "2021-12-01") %>%
  openxlsx::write.xlsx(.,"C:/Users/gabri/OneDrive/Documentos/TCC/series_3.xlsx",
                       col.names = T, row.names = F, overwrite = T)


### VARIÁVEIS EM NÍVEL ### ------------------------------------------------------

## Teste de raíz unitária
# Gasolina ---------------------------------------------
# 1) ADF
tseries::adf.test(gasolina_1$p_gasolina, k=2)


# 2) KPSS
summary(ur.kpss(gasolina$p_gasolina,type="mu"))
tseries::kpss.test(gasolina$p_gasolina, "Level")

# 3) Phillips Perron
tseries::pp.test(gasolina$p_gasolina)

# Energia -----------------------------------------------
# 1) ADF
tseries::adf.test(energia$p_energia, k=2)

# 2) KPSS
summary(ur.kpss(energia$p_energia,type="mu"))
tseries::kpss.test(energia$p_energia, "Level")

# 3) Phillips Perron
tseries::pp.test(energia$p_energia)

# PIM ----------------------------------------------------
# 1) ADF
tseries::adf.test(gasolina$pim, k=2)

# 2) KPSS
summary(ur.kpss(gasolina$pim,type="mu"))
tseries::kpss.test(gasolina$pim, "Level")

# 3) Phillips Perron
tseries::pp.test(gasolina$pim)

# IPCA ---------------------------------------------------
# 1) ADF
tseries::adf.test(gasolina$ipca, k=2)

# 2) KPSS
summary(ur.kpss(gasolina$ipca,type="mu"))
tseries::kpss.test(gasolina$ipca, "Level")

# 3) Phillips Perron
tseries::pp.test(gasolina$ipca)

# SELIC REAL ---------------------------------------------------
# 1) ADF
tseries::adf.test(gasolina$selic, k=2)

# 2) KPSS
summary(ur.kpss(gasolina$selic,type="mu"))
tseries::kpss.test(gasolina$selic, "Level")

# 3) Phillips Perron
tseries::pp.test(gasolina$selic)

## GASOLINA --------------------------------------------------------------------
gasolina_diff = gasolina_1 %>%
  mutate_at(vars(-c(data_tidy, ipca)), function(x) x - lag(x)) %>%
  filter(data_tidy <= "2021-12-01") %>%
  na.omit()

### Estimação do Modelo VAR estrutural ---------------------------------------
# 1) Gasolina ----------------------------------------------------------------
gasolina_series = ts(gasolina_diff[,2:5], start = c(2003,02), frequency = 12)
# Seleção do lag do VAR
lag_var = VARselect(gasolina_series, lag.max = 12, type = "const")
lag_var$selection

# A partir do critério de Schwartz, o modelo escolhido é o com 1 lags
var_est = VAR(gasolina_series, p = 1, season = NULL, type = "const")
summary(var_est)

# Estimando o VAR estrutural
# Matriz A
a_mat <- diag(3)
diag(a_mat) <- 1
a_mat[2, 1] <- NA
a_mat[3, 1] <- NA
a_mat[3, 2] <- NA

# Matriz B
b_mat <- diag(3)
diag(b_mat) <- NA
#print(b_mat)

# VAR estrutural
svar = SVAR(var_est, Amat = a_mat, hessian = T)
svar

# Funções de Impulso Resposta
ipca_imp = irf(svar, response = "ipca", impulse = "p_gasolina", 
               n.ahead = 20, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(ipca_imp)

pim_imp = irf(svar, response = "pim", impulse = "p_gasolina", 
              n.ahead = 20, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(pim_imp)

# Decomposição da Variância
var_fevd = fevd(svar, n.ahead = 10)
plot(var_fevd)

# 2) Energia ----------------------------------------------------------------
energia_series = ts(energia_diff[,2:4], start = c(2003,02), frequency = 12)

# Seleção do lag do VAR
lag_var = VARselect(energia_series, lag.max = 12, type = "none")
lag_var$selection

# A partir do critério de Schwartz, o modelo escolhido é o com 1 lags
var_est = VAR(energia_series, p = 1, season = NULL, type = "const")
summary(var_est)

# Estimando o VAR estrutural
# Matriz A
a_mat <- diag(3)
diag(a_mat) <- 1
a_mat[2, 1] <- NA
a_mat[3, 1] <- NA
a_mat[3, 2] <- NA

# Matriz B
b_mat <- diag(3)
diag(b_mat) <- NA
#print(b_mat)

# VAR estrutural
svar = SVAR(var_est, Amat = a_mat, Bmat = b_mat, max.iter = 10000)
svar

# Funções de Impulso Resposta
ipca_imp = irf(svar, response = "ipca", impulse = "p_energia", 
               n.ahead = 20, ortho = TRUE, boot = TRUE)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(ipca_imp)

pim_imp = irf(svar, response = "pim", impulse = "p_energia", 
              n.ahead = 20, ortho = TRUE, boot = TRUE)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(pim_imp)


### VARIÁVEIS EM LOG ### ------------------------------------------------------
gasolina_log = gasolina_1 %>%
  mutate_at(vars(-c(data_tidy, ipca, selic)), function(x) log(x/lag(x))*100) %>%
  filter(data_tidy <= "2021-12-01") %>%
  na.omit() %>% dplyr::select(-ipca_acum)

energia_log = energia_1 %>%
  mutate_at(vars(-c(data_tidy, ipca, selic)), function(x) log(x/lag(x))*100) %>%
  filter(data_tidy <= "2021-12-01") %>%
  na.omit() %>% dplyr::select(-ipca_acum)

## Teste de raíz unitária
# 1) ADF
tseries::adf.test(gasolina_log$p_gasolina, k=2)


# 2) KPSS
summary(ur.kpss(gasolina_log$p_gasolina,type="mu"))
tseries::kpss.test(gasolina_log$p_gasolina, "Level")

# 3) Phillips Perron
tseries::pp.test(gasolina_log$p_gasolina)

# Energia -----------------------------------------------
# 1) ADF
tseries::adf.test(energia_log$p_energia, k=2)

# 2) KPSS
summary(ur.kpss(energia_log$p_energia,type="mu"))
tseries::kpss.test(energia_log$p_energia, "Level")

# 3) Phillips Perron
tseries::pp.test(energia_log$p_energia)

# PIM ----------------------------------------------------
# 1) ADF
tseries::adf.test(gasolina_log$pim, k=2)

# 2) KPSS
summary(ur.kpss(gasolina_log$pim,type="mu"))
tseries::kpss.test(gasolina_log$pim, "Level")

# 3) Phillips Perron
tseries::pp.test(gasolina_log$pim)

# IPCA ---------------------------------------------------
# 1) ADF
tseries::adf.test(gasolina_log$ipca, k=2)

# 2) KPSS
summary(ur.kpss(gasolina_log$ipca,type="mu"))
tseries::kpss.test(gasolina_log$ipca, "Level")

# 3) Phillips Perron
tseries::pp.test(gasolina_log$ipca)

# SELIC REAL ---------------------------------------------------
# 1) ADF
tseries::adf.test(gasolina_log$selic, k=2)

# 2) KPSS
summary(ur.kpss(gasolina_log$selic,type="mu"))
tseries::kpss.test(gasolina_log$selic, "Level")

# 3) Phillips Perron
tseries::pp.test(gasolina_log$selic)

# 1) Gasolina ----------------------------------------------------------------
gasolina_series = ts(gasolina_log[,2:5], start = c(2002,02), frequency = 12)
# Seleção do lag do VAR
lag_var = VARselect(gasolina_series, lag.max = 12, type = "const")
lag_var$selection

## Modelagem a partir do critério de Schawartz ---------------------------------
# A partir do critério de Schwartz, o modelo escolhido é o com 2 lags
var_est = VAR(gasolina_series, p = 2, season = NULL, type = "const")
summary(var_est)

# Testando a correlação dos resíduos
df = c()
for (i in 1:12){
  bg = round(serial.test(var_est, lags.bg = i, type = "BG")$serial$statistic, 4)
  
  df = rbind(df,bg)
}

df


# Funções de Impulso Resposta

ipca_imp = irf(var_est, response = "ipca", impulse = "p_gasolina", 
               n.ahead = 10, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(ipca_imp,
     main = "Resposta do IPCA ao choque na gasolina")


pim_imp = irf(var_est, response = "pim", impulse = "p_gasolina", 
              n.ahead = 10, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(pim_imp,
     main = "Resposta da PIM ao choque na gasolina")

selic_imp = irf(var_est, response = "selic", impulse = "p_gasolina", 
                n.ahead = 36, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(selic_imp,
     main = "Resposta da SELIC ao choque na gasolina")

# Decomposição da Variância
var_fevd = fevd(var_est, n.ahead = 10)
plot(var_fevd)


## Modelagem a partir do critério de Akaike ---------------------------------
# A partir do critério de Schwartz, o modelo escolhido é o com 12 lags
var_est = VAR(gasolina_series, p = 12, season = NULL, type = "const")
summary(var_est)

# Estimando o VAR estrutural
# Matriz A
#a_mat <- diag(3)
#diag(a_mat) <- 1
#a_mat[2, 1] <- NA
#a_mat[3, 1] <- NA
#a_mat[3, 2] <- NA

# Matriz B
#b_mat <- diag(3)
#diag(b_mat) <- NA
#print(b_mat)

# VAR estrutural
#svar = SVAR(var_est, Amat = a_mat, hessian = T)
#svar

# Funções de Impulso Resposta
ipca_imp = irf(var_est, response = "ipca", impulse = "p_gasolina", 
               n.ahead = 10, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(ipca_imp)

pim_imp = irf(var_est, response = "pim", impulse = "p_gasolina", 
              n.ahead = 10, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(pim_imp)

selic_imp = irf(var_est, response = "selic", impulse = "p_gasolina", 
                n.ahead = 10, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(selic_imp)

# Decomposição da Variância
var_fevd = fevd(svar, n.ahead = 10)
plot(var_fevd)


# 2) Energia ----------------------------------------------------------------
energia_1 = left_join(pld_energia, ipca, by = "data_tidy") %>%
  left_join(., pim, by = "data_tidy") %>%
  left_join(., selic, by = "data_tidy") %>%
  dplyr::select(-ipca_acum) %>%
  na.omit()

energia_log = energia_1 %>%
  mutate_at(vars(-c(data_tidy, ipca, selic, energia)), function(x) log(x/lag(x))*100) %>%
  filter(data_tidy <= "2021-12-01") %>%
  na.omit()


energia_series = ts(energia_log[,2:5], start = c(2003,02), frequency = 12)

# Seleção do lag do VAR
lag_var = VARselect(energia_series, lag.max = 12, type = "const")
lag_var$selection

## Modelagem a partir do critério de Schawartz ---------------------------------
# A partir do critério de Schwartz, o modelo escolhido é o com 2 lags
var_est = VAR(energia_series, p = 2, season = NULL, type = "const")
summary(var_est)

# Testando a correlação dos resíduos
df = c()
for (i in 1:12){
  bg = round(serial.test(var_est, lags.bg = i, type = "BG")$serial$statistic, 4)
  
  df = rbind(df,bg)
}

df

# Funções de Impulso Resposta
ipca_imp = irf(var_est, response = "ipca", impulse = "p_energia", 
               n.ahead = 10, ortho = TRUE, boot = TRUE)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(ipca_imp,
     main = "Resposta do IPCA ao choque na energia elétrica")

pim_imp = irf(var_est, response = "pim", impulse = "p_energia", 
              n.ahead = 10, ortho = TRUE, boot = TRUE)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(pim_imp,
     main = "Resposta do PIM ao choque na energia elétrica")

selic_imp = irf(var_est, response = "selic", impulse = "p_energia", 
                n.ahead = 48, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(selic_imp,
     main = "Resposta do SELIC ao choque na energia elétrica")

# Decomposição da Variância
var_fevd = fevd(svar, n.ahead = 10)
plot(var_fevd)

## Modelagem a partir do critério de Akaike ---------------------------------
# A partir do critério de Schwartz, o modelo escolhido é o com 12 lags
var_est = VAR(energia_series, p = 12, season = NULL, type = "const")
summary(var_est)

# Estimando o VAR estrutural
# Matriz A
#a_mat <- diag(3)
#diag(a_mat) <- 1
#a_mat[2, 1] <- NA
#a_mat[3, 1] <- NA
#a_mat[3, 2] <- NA

# Matriz B
#b_mat <- diag(3)
#diag(b_mat) <- NA
#print(b_mat)

# VAR estrutural
#svar = SVAR(var_est, Amat = a_mat, Bmat = b_mat, max.iter = 10000)
#svar

# Funções de Impulso Resposta
ipca_imp = irf(var_est, response = "ipca", impulse = "p_energia", 
               n.ahead = 10, ortho = TRUE, boot = TRUE)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(ipca_imp)

pim_imp = irf(var_est, response = "pim", impulse = "p_energia", 
              n.ahead = 10, ortho = TRUE, boot = TRUE)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(pim_imp)

selic_imp = irf(var_est, response = "selic", impulse = "p_energia", 
                n.ahead = 10, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(selic_imp)

# Decomposição da Variância
var_fevd = fevd(svar, n.ahead = 10)
plot(var_fevd)


### ----------------------------------------------------------------------------####

# IRF com séries anualizadas
pim = get_sidra(api = '/t/8159/n1/all/v/11600/p/all/c544/129314/d/v11600%205') %>%
  dplyr::select("Mês (Código)",
                Valor) %>%
  rename(data_tidy = 1) %>%
  mutate(data_tidy = seq.Date(from = as.Date("2002-01-01"), to = as.Date("2022-11-01"), by = "months")) %>%
  rename(pim = 2) 

fontes = left_join(energia_industrial, gasolina)
fontes[213,3] = fontes[212,3]

fontes = fontes %>%
  mutate_at(vars(p_gasolina, p_energia), function(x) (x/lag(x)-1)*100) %>%
  mutate(p_gasolina = (acumular(p_gasolina)-1)*100,
         p_energia = (acumular(p_energia)-1)*100)


ipca = get_sidra(api = '/t/1737/n1/all/v/63/p/all/d/v63%202') %>%
  dplyr::select("Mês (Código)",
                Valor) %>%
  rename(data_tidy = 1) %>%
  mutate(data_tidy = seq.Date(from = as.Date("1979-12-01"), length.out = nrow(.), by = "months")) %>%
  rename(ipca = 2) %>%
  mutate(ipca_acum = (acumular(ipca)-1)*100)


selic = get_series(4390, as = "data.frame") %>%
  #filter(date >= "2001-01-01") %>%
  rename(data_tidy = date,
         selic = 2) %>%
  mutate_at(vars(selic), ~acumular(.)) %>%
  #filter(data_tidy >= "2002-01-01") %>%
  left_join(., ipca) %>%
  #mutate(ipca = (ipca/100)+1) %>%
  mutate(selic = ((selic/(1+ipca_acum/100))-1)*100) %>% dplyr::select(-c(ipca, ipca_acum))

series_anual = left_join(fontes,ipca) %>%
  left_join(.,selic) %>%
  left_join(.,pim) %>%
  mutate(data_tidy = as.Date(data_tidy)) %>%
  filter(data_tidy <= "2021-12-01" & data_tidy >= "2004-01-01") %>%
  dplyr::select(-ipca)


# Gasolina
gasolina_log = series_anual %>%
  dplyr::select(-p_energia) %>%
  relocate(pim, .after = ipca_acum) %>%
  rename(ipca = ipca_acum) %>%
  mutate_at(vars(p_gasolina,ipca, selic), function(x) x-lag(x)) %>%
  mutate_at(vars(pim), function(x) log(x/lag(x))*100) %>% na.omit()
  

gasolina_series = ts(gasolina_log[,2:5], start = c(2004,01), frequency = 12)
# Seleção do lag do VAR
lag_var = VARselect(gasolina_series, lag.max = 12, type = "const")
lag_var$selection

## Modelagem a partir do critério de Schawartz ---------------------------------
# A partir do critério de Schwartz, o modelo escolhido é o com 2 lags
var_est = VAR(gasolina_series, p = 4, season = NULL, type = "const")
summary(var_est)

# Testando a correlação dos resíduos
serial.test(var_est)


# Funções de Impulso Resposta

ipca_imp = irf(var_est, response = "ipca", impulse = "p_gasolina", 
               n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = T)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(ipca_imp,
     main = "Resposta do IPCA ao choque na gasolina")


pim_imp = irf(var_est, response = "pim", impulse = "p_gasolina", 
              n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = T)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(pim_imp,
     main = "Resposta da PIM ao choque na gasolina")

selic_imp = irf(var_est, response = "selic", impulse = "p_gasolina", 
                n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = T)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(selic_imp,
     main = "Resposta da SELIC ao choque na gasolina")


# Energia
energia_log = series_anual %>%
  dplyr::select(-p_gasolina) %>%
  relocate(pim, .after = ipca_acum) %>%
  rename(ipca = ipca_acum) %>%
  mutate_at(vars(p_energia,ipca, selic), function(x) x-lag(x)) %>%
  mutate_at(vars(pim), function(x) log(x/lag(x))*100) %>% na.omit()

energia_series = ts(energia_log[,2:5], start = c(2005,02), frequency = 12)

# Seleção do lag do VAR
lag_var = VARselect(energia_series, lag.max = 12, type = "const")
lag_var$selection

## Modelagem a partir do critério de Schawartz ---------------------------------
# A partir do critério de Schwartz, o modelo escolhido é o com 2 lags
var_est = VAR(energia_series, p = 4, season = NULL, type = "const")
summary(var_est)

# Testando a correlação dos resíduos
serial.test(var_est)

# Funções de Impulso Resposta
ipca_imp = irf(var_est, response = "ipca", impulse = "p_energia", 
               n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = T)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(ipca_imp,
     main = "Resposta do IPCA ao choque na energia elétrica")

pim_imp = irf(var_est, response = "pim", impulse = "p_energia", 
              n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = T)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(pim_imp,
     main = "Resposta do PIM ao choque na energia elétrica")

selic_imp = irf(var_est, response = "selic", impulse = "p_energia", 
                n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = T)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(selic_imp,
     main = "Resposta do SELIC ao choque na energia elétrica")


## Séries em nível
# Gasolina
gasolina_log = series_anual %>%
  dplyr::select(-p_energia) %>%
  relocate(pim, .after = ipca_acum) %>%
  rename(ipca = ipca_acum) %>%
  #mutate_at(vars(p_gasolina,ipca, selic), function(x) x-lag(x)) %>%
  mutate(pim = log(pim))


gasolina_series = ts(gasolina_log[,2:5], start = c(2004,01), frequency = 12)
# Seleção do lag do VAR
lag_var = VARselect(gasolina_series, lag.max = 12, type = "const")
lag_var$selection

## Modelagem a partir do critério de Schawartz ---------------------------------
# A partir do critério de Schwartz, o modelo escolhido é o com 2 lags
var_est = VAR(gasolina_series, p = 2, season = NULL, type = "const")
summary(var_est)

serial.test(var_est)

# Funções de Impulso Resposta

ipca_imp = irf(var_est, response = "ipca", impulse = "p_gasolina", 
               n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = F)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(ipca_imp,
     main = "Resposta do IPCA ao choque na gasolina")


pim_imp = irf(var_est, response = "pim", impulse = "p_gasolina", 
              n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = F)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(pim_imp,
     main = "Resposta da PIM ao choque na gasolina")

selic_imp = irf(var_est, response = "selic", impulse = "p_gasolina", 
                n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = F)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(selic_imp,
     main = "Resposta da SELIC ao choque na gasolina")


# Energia
energia_log = series_anual %>%
  dplyr::select(-p_gasolina) %>%
  relocate(pim, .after = ipca_acum) %>%
  rename(ipca = ipca_acum) %>%
  #mutate_at(vars(p_energia,ipca, selic), function(x) x-lag(x)) %>%
  mutate(pim = log(pim))

energia_series = ts(energia_log[,2:5], start = c(2004,01), frequency = 12)

# Seleção do lag do VAR
lag_var = VARselect(energia_series, lag.max = 12, type = "const")
lag_var$selection

## Modelagem a partir do critério de Schawartz ---------------------------------
# A partir do critério de Schwartz, o modelo escolhido é o com 2 lags
var_est = VAR(energia_series, p = 2, season = NULL, type = "const")
summary(var_est)

serial.test(var_est)

# Funções de Impulso Resposta
ipca_imp = irf(var_est, response = "ipca", impulse = "p_energia", 
               n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = F)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(ipca_imp,
     main = "Resposta do IPCA ao choque na energia elétrica")

pim_imp = irf(var_est, response = "pim", impulse = "p_energia", 
              n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = F)

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(pim_imp,
     main = "Resposta do PIM ao choque na energia elétrica")

selic_imp = irf(var_est, response = "selic", impulse = "p_energia", 
                n.ahead = 36, ortho = TRUE, boot = TRUE, cumulative = F)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(selic_imp,
     main = "Resposta do SELIC ao choque na energia elétrica")

