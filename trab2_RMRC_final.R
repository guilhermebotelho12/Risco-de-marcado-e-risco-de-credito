# Instalar e carregar pacotes necessários
install.packages("httr")
install.packages("jsonlite")
install.packages("quantmod")
install.packages("writexl")
install.packages("ggplot2")
install.packages("pscl")
library(httr)
library(jsonlite)
library(quantmod)
library(writexl)
library(ggplot2)
library(dplyr)
library(TTR)
library(car)
library(lmtest)
library(pROC)
library(caret)

# Obtenção dos Dados Meteorológicos de Lisboa
url <- "https://archive-api.open-meteo.com/v1/era5?latitude=38.72&longitude=-9.13&start_date=2013-05-01&end_date=2021-05-01&daily=temperature_2m_max,temperature_2m_min,relative_humidity_2m_max,relative_humidity_2m_min,precipitation_sum,wind_speed_10m_max&timezone=Europe/Lisbon"
response <- GET(url)

# Verifica se os dados foram coletados com sucesso
if (status_code(response) == 200) {
  data <- fromJSON(content(response, "text"))
  
  # Organiza os dados meteorológicos
  weather_data <- data.frame(
    date = as.Date(data$daily$time),
    temp_max = as.numeric(data$daily$temperature_2m_max),
    temp_min = as.numeric(data$daily$temperature_2m_min),
    humidity_max = as.numeric(data$daily$relative_humidity_2m_max),
    humidity_min = as.numeric(data$daily$relative_humidity_2m_min),
    precipitation = as.numeric(data$daily$precipitation_sum),
    wind_speed = as.numeric(data$daily$wind_speed_10m_max)
  )
} else {
  stop("Erro em encontrar os dados meteorológicos.")
}

# Obtenção dos dados Financeiros para EDP
getSymbols("EDP.LS", src = "yahoo", from = "2013-05-01", to = "2021-05-01")

# Preparação dos dados financeiros
stock_data <- data.frame(
  date = index(EDP.LS),
  price_max = as.numeric(Hi(EDP.LS)),
  price_close = as.numeric(Cl(EDP.LS)),
  volume = as.numeric(Vo(EDP.LS))
)

# Garante que as datas estão no mesmo formato
weather_data$date <- as.Date(weather_data$date)
stock_data$date <- as.Date(stock_data$date)

# Merge dos dados climáticos e financeiros
combined_data <- merge(weather_data, stock_data, by = "date", all = TRUE)

# Cálculo do retorno diário da EDP
combined_data$return <- c(NA, diff(log(combined_data$price_close)))

# Obtenção dos dados do PSI20
getSymbols("^PSI20", src = "yahoo", from = "2013-05-01", to = "2021-05-01")
market_data <- data.frame(date = index(PSI20), market_price = as.numeric(Cl(PSI20)))

# Merge dos dados de mercado com os dados climáticos e financeiros
combined_data <- merge(combined_data, market_data, by = "date", all = TRUE)

# Cálculo do retorno diário do mercado
combined_data$market_return <- c(NA, diff(log(combined_data$market_price)))

# Remove as linhas com valores NA ou NaN
combined_data <- na.omit(combined_data)
head(combined_data)

# Índices
combined_data$y <- ifelse(combined_data$price_close > lag(combined_data$price_close, 1), 1, 0)
combined_data$RIS <- RSI(combined_data$price_close, n = 14)
combined_data$SMA <- SMA(combined_data$price_close, n = 10)
combined_data$OBV <- OBV(combined_data$price_close, combined_data$volume)

dados_ind<-na.omit(combined_data)
subdados <- dados_ind[, !(names(dados_ind) %in% c("date", "price_max","volume","market_price","temp_max","humidity_min","return","market_return","price_close","y"))]
cor(subdados)

modelo <- glm(y ~  RIS + SMA + OBV, data = dados_ind, family = binomial)
summary(modelo)
dados_ind$pred_prob <- predict(modelo, type = "response")
dados_ind$pred_class <- ifelse(dados_ind$pred_prob > 0.5, 1, 0)
table(Predito = dados_ind$pred_class, Real = dados_ind$y)
mean(dados_ind$pred_class == dados_ind$y, na.rm = TRUE) # Acurâcia

# Calcula o VIF para o modelo de regressão logística
vif(modelo)

#AUC
roc_curve <- roc(dados_ind$y, dados_ind$pred_prob)
plot(roc_curve)
auc(roc_curve)

# F1-Score
F1 <- F_meas(as.factor(dados_ind$pred_class), as.factor(dados_ind$y))
F1


#Modelo 2
modelo2<-glm(y ~  RIS + SMA + OBV + temp_min + humidity_max + precipitation + wind_speed, data = dados_ind, family = binomial)
summary(modelo2)
dados_ind$pred_prob2 <- predict(modelo2, type = "response")
dados_ind$pred_class2 <- ifelse(dados_ind$pred_prob2 > 0.5, 1, 0)
table(Predito = dados_ind$pred_class2, Real = dados_ind$y)
mean(dados_ind$pred_class2 == dados_ind$y, na.rm = TRUE) #Acur?cia

# Calcula o VIF para o modelo de regressão logística
vif(modelo2)

#AUC
roc_curve2 <- roc(dados_ind$y, dados_ind$pred_prob2)
plot(roc_curve2)
auc(roc_curve2)

# F1-Score
F1 <- F_meas(as.factor(dados_ind$pred_class2), as.factor(dados_ind$y))
F1

# Regressão do retorno da EDP sobre o retorno do mercado (modelo CAPM)
model <- lm(return ~ market_return, data = combined_data)

# Exibe os resultados do modelo CAPM
cat("Resultados do Modelo CAPM:\n")
summary(model)

# Regressão ajustada para incluir as variáveis climáticas
climate_model <- lm(return ~ market_return + temp_max + temp_min + humidity_max + humidity_min + precipitation + wind_speed, data = combined_data)

# Exibe os resultados do modelo ajustado com variáveis climáticas
cat("\nResultados do Modelo Ajustado com Variáveis Climáticas:\n")
summary(climate_model)

#testes
vif(climate_model)

# Teste RESET
resettest(climate_model)

# Teste de White para heterocedasticidade
bptest(climate_model, studentize = TRUE) 

# Teste de Shapiro-Wilk para normalidade
shapiro.test(residuals(climate_model))

climate_model2 <- lm(return ~ market_return + temp_min + humidity_max + precipitation + wind_speed, data = combined_data)
summary(climate_model2)

#testes
vif(climate_model2)

# Teste RESET
resettest(climate_model2)

# Teste de White para heterocedasticidade
bptest(climate_model2, studentize = TRUE)

# Teste de Shapiro-Wilk para normalidade
shapiro.test(residuals(climate_model2))

# Avalia o impacto das variáveis climáticas no retorno e risco sistemático
cat("\nCoeficientes do Modelo Ajustado (impacto das variaveis climaticas):\n")
print(coef(climate_model))


##################################################################################
### Graficos exemplo para visualizacao dos dados

# Grafico de linha para visualização do retorno diário da EDP
ggplot(combined_data, aes(x = date, y = return)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Retorno Diario da EDP",
    x = "Data",
    y = "Retorno Diario"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Grafico de dispersao para visualizar a relacao entre o retorno da EDP e a temperatura maxima
ggplot(combined_data, aes(x = temp_max, y = return)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Relacao entre Retorno da EDP e Temperatura Maxima",
    x = "Temperatura Maxima (?C)",
    y = "Retorno Diario da EDP"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Grafico de linha para visualizar a variacao do preco de fecho da EDP ao longo do tempo
ggplot(combined_data, aes(x = date, y = price_close)) +
  geom_line(color = "purple", size = 1) +
  labs(
    title = "Variacao do Preco de fecho da EDP",
    x = "Data",
    y = "Preco de fecho"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Grafico de dispersao para a relacao entre o retorno do mercado (PSI20) e da EDP
ggplot(combined_data, aes(x = market_return, y = return)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Relacao entre Retorno do Mercado (PSI20) e Retorno da EDP",
    x = "Retorno do Mercado",
    y = "Retorno Diario da EDP"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

############################################################################################
#Vamos agragar os dados para ser possivel visualizar melhor a variacao dos dados
# Funcaoo ajustada para agregar dados por ano com temperatura e humidade manimas incluidas
# Verifica os nomes das colunas no `combined_data`
print(names(combined_data))


# Calcula a temperatura media entre a temperatura maxima e minima
combined_data <- combined_data %>%
  mutate(temp_mean = (temp_max + temp_min) / 2)

# Calcula a media da humidade (maxima e minima)
combined_data <- combined_data %>%
  mutate(humidity_mean = (humidity_max + humidity_min) / 2)

# Agora, agregamos por ano
agregar_por_ano_completo <- function(dados) {
  dados %>%
    mutate(year = format(date, "%Y")) %>%
    group_by(year) %>%
    summarise(
      temp_max = mean(temp_max, na.rm = TRUE),
      temp_min = mean(temp_min, na.rm = TRUE),
      temp_mean = mean(temp_mean, na.rm = TRUE),  # Temperatura media
      humidity_max = mean(humidity_max, na.rm = TRUE),
      humidity_min = mean(humidity_min, na.rm = TRUE),
      humidity_mean = mean(humidity_mean, na.rm = TRUE),  # Media da humidade
      precipitation = sum(precipitation, na.rm = TRUE),
      wind_speed = mean(wind_speed, na.rm = TRUE)
    ) %>%
    mutate(year = as.integer(year))
}

# Dados anuais agregados com a media da humidade
annual_data <- agregar_por_ano_completo(combined_data)

# Grafico de Temperatura Maxima, Minima e Media ao longo dos anos
ggplot(annual_data, aes(x = year)) +
  geom_line(aes(y = temp_max), color = "blue") +
  geom_point(aes(y = temp_max), color = "red") +
  geom_line(aes(y = temp_min), color = "green") +
  geom_point(aes(y = temp_min), color = "orange") +
  geom_line(aes(y = temp_mean), color = "purple", linetype = "dashed") +  # Linha da temperatura media
  labs(
    title = "Variação da Temperatura Maxima, Minima e Media ao Longo dos Anos",
    x = "Ano",
    y = "Temperatura (?C)"
  ) +
  theme_minimal()

# Grafico de Humidade Maxima, Minima e Media ao longo dos anos
ggplot(annual_data, aes(x = year)) +
  geom_line(aes(y = humidity_max), color = "green") +
  geom_point(aes(y = humidity_max), color = "orange") +
  geom_line(aes(y = humidity_min), color = "blue") +
  geom_point(aes(y = humidity_min), color = "red") +
  geom_line(aes(y = humidity_mean), color = "purple", linetype = "dashed") +  # Linha da humidade media
  labs(
    title = "Variação da Humidade Maxima, Minima e Media ao Longo dos Anos",
    x = "Ano",
    y = "Humidade (%)"
  ) +
  theme_minimal()

# Grafico de Precipitacao ao longo dos anos
ggplot(annual_data, aes(x = year)) +
  geom_line(aes(y = precipitation), color = "blue") +
  geom_point(aes(y = precipitation), color = "red") +
  labs(
    title = "Variação da Precipitação ao Longo dos Anos",
    x = "Ano",
    y = "Precipitação (mm)"
  ) +
  theme_minimal()

# Grafico da Velocidade do Vento ao longo dos anos
ggplot(annual_data, aes(x = year)) +
  geom_line(aes(y = wind_speed), color = "cyan") +
  geom_point(aes(y = wind_speed), color = "magenta") +
  labs(
    title = "Variação da Velocidade do Vento ao Longo dos Anos",
    x = "Ano",
    y = "Velocidade do Vento (m/s)"
  ) +
  theme_minimal()
