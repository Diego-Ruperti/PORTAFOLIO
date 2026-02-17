# ============================================================
# PROYECTO: Demand Forecasting & Demand Planning
# OBJETIVO: Construir y evaluar un forecast base estadístico
# HERRAMIENTAS: R, MySQL, tidyverse, forecast
# FUENTE: Kaggle - Demand Forecasting with Tabular Data
# AUTOR: Diego Correa
# ============================================================

# ============================================================
# 1. CARGA DE LIBRERÍAS
# ============================================================

library(DBI)
library(RMySQL)
library(tidyverse)
library(lubridate)
library(forecast)


# ============================================================
# 2. CONEXIÓN A BASE DE DATOS (PLANTILLA)
# ============================================================

# NOTA:
# Las credenciales reales NO deben publicarse.
# Completar los campos según el entorno local.

Conexión <- dbConnect(
  MySQL(),
  user = "tu_usuario",
  password = "tu_password",
  host = "localhost",
  dbname = "tu_base_datos"
)


# ============================================================
# 3. CARGA DEL DATASET ANALÍTICO
# ============================================================

monthly_demand <- dbGetQuery(Conexión, 
  "SELECT *
   FROM monthly_demand")

## Revisión inicial
glimpse(monthly_demand)
summary(monthly_demand)


# ============================================================
# 4. DEFINICIÓN DEL NIVEL DE ANÁLISIS
# ============================================================

# El forecast base se construye a nivel:
# Producto (item_id) - Tiempo (mensual)

monthly_demand %>%
    count(item_id) %>%
    arrange(desc(n))
  
# Selección de un producto ejemplo
    item_selected <- 25
  
df_item <- monthly_demand %>%
  filter(item_id == item_selected) %>%
  arrange(year, month)


# ============================================================
# 5. CONSTRUCCIÓN DE VARIABLE FECHA
# ============================================================

df_item <- df_item %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-")))

# Validación:
  head(df_item)

  
# ============================================================
# 6. VISUALIZACIÓN INICIAL DE LA DEMANDA
# ============================================================

ggplot(df_item, aes(x = date, y = monthly_units_sold)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point() +
  labs(
    title = "Demanda histórica mensual",
    subtitle = paste("Item ID:", item_selected),
    x = "Fecha",
    y = "Unidades vendidas") + theme_minimal()


# ============================================================
# 7. CONVERSIÓN A SERIE DE TIEMPO
# ============================================================

ts_item <- ts(
  df_item$monthly_units_sold,
  start = c(min(df_item$year), min(df_item$month)),
  frequency = 12)

# Visualización:
  autoplot(ts_item) +
    labs(title = "Serie de tiempo mensual")

  
# ============================================================
# 8. DESCOMPOSICIÓN DE LA SERIE
# ============================================================

decomp <- stl(ts_item, s.window = "periodic")
autoplot(decomp)

      
# ============================================================
# 9. DIVISIÓN TRAIN / TEST
# ============================================================      

# Criterio empresarial:
# - Train: hasta septiembre 2021
# - Test: últimos 3 meses

n <- length(ts_item)
train_ts <- window(ts_item, end = c(2021, 9))
test_ts  <- window(ts_item, start = c(2021, 10))
   
# Validación:
  length(train_ts)
  length(test_ts)
 
     
# ============================================================
# 10. MODELOS DE FORECAST
# ============================================================

# Modelo 1 — Media Móvil (baseline ingenuo)
ma_model <- ma(train_ts, order = 3)     
ma_forecast <- forecast(ma_model, h = 3)

# Modelo 2 — Suavizamiento Exponencial Simple (SES)
ses_model <- ses(train_ts, h = 3)

# Modelo 3 — Holt (nivel + tendencia)
holt_model <- holt(train_ts, h = 3)

# Modelo 4 — Holt-Winters (nivel + tendencia + estacionalidad)
hw_model <- hw(train_ts, seasonal = "additive", h = 3)   

# Modelo 5 — ARIMA
arima_model <- auto.arima(
  train_ts,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE)

  # Resumen del modelo ARIMA     
  summary(arima_model)

  # Forecast Arima
  arima_forecast <- forecast(arima_model, h = 3)

  # Validación de ruido blanco, si es mayor a p>0.05 entonces hay ruido blanco (media igual a cero, varianza constante)
  Box.test(residuals(arima_model), type = "Ljung-Box")


# ============================================================
# 11. COMPARACIÓN VISUAL DE MODELOS
# ============================================================

autoplot(train_ts) +
  autolayer(test_ts, series = "Real") +
  autolayer(ma_forecast$mean, series = "Media Móvil") +
  autolayer(ses_model$mean, series = "SES") +
  autolayer(holt_model$mean, series = "Holt") +
  autolayer(hw_model$mean, series = "Holt-Winters") +
  autolayer(arima_forecast$mean, series = "ARIMA") +
  labs(title = "Comparación de modelos de forecast") +
  theme_minimal()

  
# ============================================================
# 12. EVALUACIÓN DEL DESEMPEÑO
# ============================================================

accuracy(ma_forecast, test_ts)
accuracy(ses_model, test_ts)
accuracy(holt_model, test_ts)
accuracy(hw_model, test_ts)  
accuracy(arima_forecast, test_ts)


# ============================================================
# 13. CONCLUSIONES ANALÍTICAS
# ============================================================

# El modelo SES y ARIMA presentan desempeños similares.
# Dado el principio de parsimonia, SES se considera
# El forecast base estadístico para planeación de demanda.
