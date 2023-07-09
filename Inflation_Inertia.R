### Estimando a inércia inflacionária para dois grupos de países
## Emergentes: Brasil, México, Indonésia, Turquia e Colômbia
## Desenvolvidos: Estados Unidos, Alemanha, Reino Unido, França e Canadá

library(fredr)
library(sidrar)
library(dplyr)
library(tseries)
library(aTSA)
library(forecast)
library(tidyverse)
library(tidyr)
library(urca)
library(tseries)
library(flextable)
library(forecast)
library(tsDyn)
library(AICcmodavg)
library(zoo)
library(stats)

## Estabelecendo a chave de acesso ao FRED
fredr_set_key("6665a041528ca2272bbfcf8bc267c940")


## Extraindo os dados
# Emergentes
mexico <- fredr_series_observations("CPALTT01MXQ657N") %>%  # CPALTT01MXM659N
  select(date, value) %>%
  #mutate_at(vars(value), function(x) (x-lag(x))/lag(x)*100) %>%
  rename(mexico = 2)

indonesia <- fredr_series_observations("CPALTT01IDQ657N") %>%  # CPALTT01IDM659N
  select(date, value) %>%
  #mutate_at(vars(value), function(x) (x-lag(x,12))/lag(x,12)*100) %>%
  rename(indonesia = 2)
  
chile <- fredr_series_observations("CPALTT01CLQ657N") %>%  # CPALTT01CLM659N
  select(date, value) %>%
  #mutate_at(vars(value), function(x) (x-lag(x))/lag(x)*100) %>%
  rename(chile = 2)

colombia <- fredr_series_observations("COLCPALTT01GPQ") %>%  # COLCPALTT01GYM
  select(date, value) %>%
  rename(colombia = 2)

brasil <- fredr_series_observations("CPALTT01BRQ657N") %>%  # CPALTT01BRM659N
  select(date, value) %>%
  rename(brasil = 2)

india <- fredr_series_observations("CPALTT01INQ657N") %>%  # CPALTT01INM659N
  select(date, value) %>%
  rename(india = 2)

# Desenvolvidos
eua <- fredr_series_observations("CPALTT01USQ657N") %>%  # Taxa
  select(date, value) %>%
  rename(eua = 2)

alemanha <- fredr_series_observations("CPALTT01DEQ657N") %>%  # Taxa
  select(date, value) %>%
  rename(alemanha = 2)

uk <- fredr_series_observations("CPALTT01GBQ657N") %>%  # Índice
  select(date, value) %>%
  #mutate_at(vars(value), function(x) (x-lag(x))/lag(x)*100) %>%
  rename(uk = 2)

franca <- fredr_series_observations("CPALTT01FRQ657N") %>%  # Índice
  select(date, value) %>%
  #mutate_at(vars(value), function(x) (x-lag(x))/lag(x)*100) %>%
  rename(franca = 2)

canada <- fredr_series_observations("CPALTT01CAQ657N") %>%  # Índice
  select(date, value) %>%
  #mutate_at(vars(value), function(x) (x-lag(x))/lag(x)*100) %>%
  rename(canada = 2)

australia <- fredr_series_observations("CPALTT01AUQ657N") %>%  # Índice
  select(date, value) %>%
  #mutate_at(vars(value), function(x) (x-lag(x))/lag(x)*100) %>%
  rename(australia = 2)


## Juntando todas as bases de dados em uma só
inflation_emergentes = left_join(mexico, indonesia) %>%
  left_join(., chile) %>%
  left_join(., colombia) %>%
  left_join(., brasil) %>% 
  left_join(., india) %>% filter(date >= "1981-01-01")

inflation_desenv = left_join(eua, alemanha) %>%
  left_join(., uk) %>%
  left_join(., franca) %>%
  left_join(., canada) %>%
  left_join(., australia) %>%
  na.omit() %>%
  filter(date >= "1981-01-01")


inflation = left_join(inflation_emergentes, inflation_desenv)

## Em um primeiro momento serão estimados as raízes unitárias das séries para cada país, de modo a observar
## se as séries são estacionárias (e se existe inércia, no caso de não estacionariedade). Para observar diferentes
## dinâmicas, esses testes serão realizados para cada uma das décadas também. Posteriormente, serão estimados
## modelos autoregressivos para observar a existência de inércia nessas séries. Será, por último, feita uma 
## comparação da dinâmica inflacionária no pré e pós crise do covid-19.

## Teste de raiz unitária (Augmented Dickey-Fuller e Phillips-Perron)

### Augmented Dickey-Fuller
# Década de 80 (81-90)
emerging_80s = inflation %>%
  filter(date <= "1990-10-01")

t_value = c()
for (i in names(emerging_80s)[2:13]){
  x = ts(emerging_80s[,i])
  adf_test = tseries::adf.test(x)
  y = adf_test$p.value
  t_value = rbind(t_value, y)
  
}

inflation_80s = t_value %>%
  as.data.frame() %>%
  rename(P_valor = 1) %>%
  mutate(Countries = names(emerging_80s)[2:13]) %>%
  remove_rownames(.) %>%
  relocate(Countries)

# Década de 90 (91-00)
emerging_90s = inflation %>%
  filter(date > "1990-10-01" & date <= "2000-10-01")

t_value = c()
for (i in names(emerging_90s)[2:13]){
  x = ts(emerging_90s[,i])
  adf_test = tseries::adf.test(x)
  y = adf_test$p.value
  t_value = rbind(t_value, y)
  
}

inflation_90s = t_value %>%
  as.data.frame() %>%
  rename(P_valor = 1) %>%
  mutate(Countries = names(emerging_90s)[2:13]) %>%
  remove_rownames(.) %>%
  relocate(Countries)

# Década de 00 (00-10)
emerging_00s = inflation %>%
  filter(date > "2000-10-01" & date <= "2010-10-01")

t_value = c()
for (i in names(emerging_00s)[2:13]){
  x = ts(emerging_00s[,i])
  adf_test = tseries::adf.test(x)
  y = adf_test$p.value
  t_value = rbind(t_value, y)
  
}

inflation_00s = t_value %>%
  as.data.frame() %>%
  rename(P_valor = 1) %>%
  mutate(Countries = names(emerging_00s)[2:13]) %>%
  remove_rownames(.) %>%
  relocate(Countries)

# Década de 10 (10-20)
emerging_10s = inflation %>%
  filter(date > "2010-10-01" & date <= "2020-10-01")

t_value = c()
for (i in names(emerging_10s)[2:13]){
  x = ts(emerging_10s[,i])
  adf_test = tseries::adf.test(x)
  y = adf_test$p.value
  t_value = rbind(t_value, y)
  
}

inflation_10s = t_value %>%
  as.data.frame() %>%
  rename(P_valor = 1) %>%
  mutate(Countries = names(emerging_10s)[2:13]) %>%
  remove_rownames(.) %>%
  relocate(Countries)

# Toda a série
t_value = c()
for (i in names(inflation)[2:13]){
  x = ts(inflation[,i])
  adf_test = tseries::adf.test(x)
  y = adf_test$p.value
  t_value = rbind(t_value, y)
  
}

inflation_all = t_value %>%
  as.data.frame() %>%
  rename(P_valor = 1) %>%
  mutate(Countries = names(inflation)[2:13]) %>%
  remove_rownames(.) %>%
  relocate(Countries)


## Todas as décadas
ur_inflation = left_join(inflation_80s, inflation_90s, by = "Countries") %>%
  left_join(., inflation_00s, by = "Countries") %>%
  left_join(., inflation_10s, by = "Countries") %>%
  left_join(., inflation_all, by = "Countries") %>%
  mutate(Countries = c("México","Indonésia","Chile","Colômbia","Brasil","Índia","EUA","Alemanha","UK","França","Canadá","Austrália"))

names(ur_inflation)[-1] = c("80s","90s","00s","10s","All")

ft <- flextable::flextable(ur_inflation)
ft <- add_header_row(ft,
                     colwidths = c(1, 5),
                     values = c(" ", "Augmented Dickey-Fuller (p-valor)")) %>%
  theme_box() %>%
  align(align = "center", part = "all") %>%
  color(i = ~ All < 0.1, j = ~ All, color = "red") %>%
  color(i = ~ `80s` < 0.1, j = ~ `80s`, color = "red") %>%
  color(i = ~ `90s` < 0.1, j = ~`90s`, color = "red") %>%
  color(i = ~ `00s` < 0.1, j = ~`00s`, color = "red") %>%
  color(i = ~ `10s` < 0.1, j = ~ `10s`, color = "red")

ft

### Phillips-Perron
# Década de 80 (81-90)
emerging_80s = inflation %>%
  filter(date <= "1990-10-01")

t_value = c()
for (i in names(emerging_80s)[2:13]){
  x = ts(emerging_80s[,i])
  adf_test = tseries::pp.test(x)
  y = adf_test$p.value
  t_value = rbind(t_value, y)
  
}

inflation_80s = t_value %>%
  as.data.frame() %>%
  rename(P_valor = 1) %>%
  mutate(Countries = names(emerging_80s)[2:13]) %>%
  remove_rownames(.) %>%
  relocate(Countries)

# Década de 90 (91-00)
emerging_90s = inflation %>%
  filter(date > "1990-10-01" & date <= "2000-10-01")

t_value = c()
for (i in names(emerging_90s)[2:13]){
  x = ts(emerging_90s[,i])
  adf_test = tseries::pp.test(x)
  y = adf_test$p.value
  t_value = rbind(t_value, y)
  
}

inflation_90s = t_value %>%
  as.data.frame() %>%
  rename(P_valor = 1) %>%
  mutate(Countries = names(emerging_90s)[2:13]) %>%
  remove_rownames(.) %>%
  relocate(Countries)

# Década de 00 (00-10)
emerging_00s = inflation %>%
  filter(date > "2000-10-01" & date <= "2010-10-01")

t_value = c()
for (i in names(emerging_00s)[2:13]){
  x = ts(emerging_00s[,i])
  adf_test = tseries::pp.test(x)
  y = adf_test$p.value
  t_value = rbind(t_value, y)
  
}

inflation_00s = t_value %>%
  as.data.frame() %>%
  rename(P_valor = 1) %>%
  mutate(Countries = names(emerging_00s)[2:13]) %>%
  remove_rownames(.) %>%
  relocate(Countries)

# Década de 10 (10-20)
emerging_10s = inflation %>%
  filter(date > "2010-10-01" & date <= "2020-10-01")

t_value = c()
for (i in names(emerging_10s)[2:13]){
  x = ts(emerging_10s[,i])
  adf_test = tseries::pp.test(x)
  y = adf_test$p.value
  t_value = rbind(t_value, y)
  
}

inflation_10s = t_value %>%
  as.data.frame() %>%
  rename(P_valor = 1) %>%
  mutate(Countries = names(emerging_10s)[2:13]) %>%
  remove_rownames(.) %>%
  relocate(Countries)

# Toda a série
t_value = c()
for (i in names(inflation)[2:13]){
  x = ts(inflation[,i])
  adf_test = tseries::pp.test(x)
  y = adf_test$p.value
  t_value = rbind(t_value, y)
  
}

inflation_all = t_value %>%
  as.data.frame() %>%
  rename(P_valor = 1) %>%
  mutate(Countries = names(inflation)[2:13]) %>%
  remove_rownames(.) %>%
  relocate(Countries)


## Todas as décadas
ur_inflation = left_join(inflation_80s, inflation_90s, by = "Countries") %>%
  left_join(., inflation_00s, by = "Countries") %>%
  left_join(., inflation_10s, by = "Countries") %>%
  left_join(., inflation_all, by = "Countries") %>%
  mutate(Countries = c("México","Indonésia","Chile","Colômbia","Brasil","Índia","EUA","Alemanha","UK","França","Canadá","Austrália"))

names(ur_inflation)[-1] = c("80s","90s","00s","10s","All")

ft <- flextable::flextable(ur_inflation)
ft <- add_header_row(ft,
                     colwidths = c(1, 5),
                     values = c(" ", "Phillips-Perron (p-valor)")) %>%
  theme_box() %>%
  align(align = "center", part = "all") %>%
  color(i = ~ All < 0.1, j = ~ All, color = "red") %>%
  color(i = ~ `80s` < 0.1, j = ~ `80s`, color = "red") %>%
  color(i = ~ `90s` < 0.1, j = ~`90s`, color = "red") %>%
  color(i = ~ `00s` < 0.1, j = ~`00s`, color = "red") %>%
  color(i = ~ `10s` < 0.1, j = ~ `10s`, color = "red")

ft

## Nesse segundo momento, serão estimados auto arimas para cada um dos países, de maneira a observar como o
## fator inercial se comporta ao longo do tempo


### ARIMA
# Estimando o modelo AR(q) para cada um dos países
models = list()
for (i in names(inflation)[2:13]){
  print(i)
  model1 = arima(inflation[,i], order = c(1,0,0)) 
  model2 = arima(inflation[,i], order = c(2,0,0))
  model3 = arima(inflation[,i], order = c(3,0,0))
  model4 = arima(inflation[,i], order = c(4,0,0))
  
  aic <- c(model1$aic, model2$aic, model3$aic, model4$aic)
  q = which.min(aic)
  
  model = arima(inflation[,i], order = c(q,0,0))
  
  models[[length(models) + 1]] = model
  
}

names(models) <- names(inflation)[-1]

# Extraindo o coeficiente p (soma dos coeficientes AR)
p_coefs = c()
for (i in names(inflation)[2:13]){
  print(i)
  
  coef = models[[i]][["coef"]]
  
  p = sum(coef[1:(length(coef)-1)])
  
  p_coefs = rbind(p_coefs, p)
  
}

coef_p = p_coefs %>%
  as.data.frame() %>%
  mutate(Countries = names(inflation[2:13])) %>%
  relocate(Countries) %>%
  rename("Coeficiente P" = 2) %>%
  remove_rownames()


# Década de 80
inflation_80s = inflation %>%
  filter(date <= "1990-10-01")

models_80 = list()
for (i in names(inflation)[2:13]){
  
  model1 = arima(inflation_80s[,i], order = c(1,0,0)) 
  model2 = arima(inflation_80s[,i], order = c(2,0,0))
  
  aic <- c(model1$aic, model2$aic)
  q = which.min(aic)
  
  model = arima(inflation_80s[,i], order = c(q,0,0))
  
  models_80[[length(models_80) + 1]] = model
  
}

names(models_80) <- names(inflation)[-1]

# Extraindo o coeficiente p (soma dos coeficientes AR)
p_coefs_80 = c()
for (i in names(inflation)[2:13]){
  
  coef = models_80[[i]][["coef"]]
  
  p = sum(coef[1:(length(coef)-1)])
  
  p_coefs_80 = rbind(p_coefs_80, p)
  
}

coef_p_80 = p_coefs_80 %>%
  as.data.frame() %>%
  mutate(Countries = names(inflation[2:13])) %>%
  relocate(Countries) %>%
  rename("Coeficiente P" = 2) %>%
  remove_rownames()


# Década de 90
inflation_90s = inflation %>%
  filter(date > "1990-10-01" & date <= "2000-10-01")

models_90 = list()
for (i in names(inflation)[2:13]){
  
  model1 = arima(inflation_90s[,i], order = c(1,0,0)) 
  model2 = arima(inflation_90s[,i], order = c(2,0,0))
  
  aic <- c(model1$aic, model2$aic)
  q = which.min(aic)
  
  model = arima(inflation_90s[,i], order = c(q,0,0))
  
  models_90[[length(models_90) + 1]] = model
  
}

names(models_90) <- names(inflation)[-1]

# Extraindo o coeficiente p (soma dos coeficientes AR)
p_coefs_90 = c()
for (i in names(inflation)[2:13]){
  
  coef = models_90[[i]][["coef"]]
  
  p = sum(coef[1:(length(coef)-1)])
  
  p_coefs_90 = rbind(p_coefs_90, p)
  
}

coef_p_90 = p_coefs_90 %>%
  as.data.frame() %>%
  mutate(Countries = names(inflation[2:13])) %>%
  relocate(Countries) %>%
  rename("Coeficiente P" = 2) %>%
  remove_rownames()


# Década de 00
inflation_00s = inflation %>%
  filter(date > "2000-10-01" & date <= "2010-10-01")

models_00 = list()
for (i in names(inflation)[2:13]){
  
  model1 = arima(inflation_00s[,i], order = c(1,0,0)) 
  model2 = arima(inflation_00s[,i], order = c(2,0,0))
  
  aic <- c(model1$aic, model2$aic)
  q = which.min(aic)
  
  model = arima(inflation_00s[,i], order = c(q,0,0))
  
  models_00[[length(models_00) + 1]] = model
  
}

names(models_00) <- names(inflation)[-1]

# Extraindo o coeficiente p (soma dos coeficientes AR)
p_coefs_00 = c()
for (i in names(inflation)[2:13]){
  
  coef = models_00[[i]][["coef"]]
  
  p = sum(coef[1:(length(coef)-1)])
  
  p_coefs_00 = rbind(p_coefs_00, p)
  
}

coef_p_00 = p_coefs_00 %>%
  as.data.frame() %>%
  mutate(Countries = names(inflation[2:13])) %>%
  relocate(Countries) %>%
  rename("Coeficiente P" = 2) %>%
  remove_rownames()


# Década de 10
inflation_10s = inflation %>%
  filter(date > "2010-10-01" & date <= "2020-10-01")

models_10 = list()
for (i in names(inflation)[2:13]){
  
  model1 = arima(inflation_10s[,i], order = c(1,0,0)) 
  model2 = arima(inflation_10s[,i], order = c(2,0,0))
  
  aic <- c(model1$aic, model2$aic)
  q = which.min(aic)
  
  model = arima(inflation_10s[,i], order = c(q,0,0))
  
  models_10[[length(models_10) + 1]] = model
  
}

names(models_10) <- names(inflation)[-1]

# Extraindo o coeficiente p (soma dos coeficientes AR)
p_coefs_10 = c()
for (i in names(inflation)[2:13]){
  
  coef = models_10[[i]][["coef"]]
  
  p = sum(coef[1:(length(coef)-1)])
  
  p_coefs_10 = rbind(p_coefs_10, p)
  
}

coef_p_10 = p_coefs_10 %>%
  as.data.frame() %>%
  mutate(Countries = names(inflation[2:13])) %>%
  relocate(Countries) %>%
  rename("Coeficiente P" = 2) %>%
  remove_rownames()

## Tabela com o coeficiente p para os países para cada década
ar_inflation = left_join(coef_p_80, coef_p_90, by = "Countries") %>%
  left_join(., coef_p_00, by = "Countries") %>%
  left_join(., coef_p_10, by = "Countries") %>%
  left_join(., coef_p, by = "Countries") %>%
  mutate(Countries = c("México","Indonésia","Chile","Colômbia","Brasil","Índia","EUA","Alemanha","UK","França","Canadá","Austrália"))

names(ar_inflation)[-1] = c("80s","90s","00s","10s","All")

ft <- flextable::flextable(ar_inflation)
ft <- add_header_row(ft,
                     colwidths = c(1, 5),
                     values = c(" ", "Coeficiente P (AR model)")) %>%
  theme_box() %>%
  align(align = "center", part = "all") %>%
  color(i = ~ All > 0.95, j = ~ All, color = "red") %>%
  color(i = ~ `80s` > 0.95, j = ~ `80s`, color = "red") %>%
  color(i = ~ `90s` > 0.95, j = ~`90s`, color = "red") %>%
  color(i = ~ `00s` > 0.95, j = ~`00s`, color = "red") %>%
  color(i = ~ `10s` > 0.95, j = ~ `10s`, color = "red")

ft
