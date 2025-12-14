# ============================================================
# PROYECTO: A/B TESTING – EFECTIVIDAD DE PROMOCIONES
# OBJETIVO: Evaluar si existen diferencias significativas
#           en ventas promedio entre distintos tipos de promoción
# HERRAMIENTAS: R, dplyr, ggplot2
# ============================================================
# Dataset fuente:
# https://www.kaggle.com/datasets/chebotinaa/fast-food-marketing-campaign-ab-test


# ============================================================
# 1. CARGA DE LIBRERÍAS
# ============================================================

library(dplyr)
library(ggplot2)
library(car)   # Para prueba de Levene


# ============================================================
# 2. CARGA Y EXPLORACIÓN INICIAL DE LOS DATOS
# ============================================================

# Cargar dataset original
## Modificar la dirección del archivo de acuerdo al directorio donde esta el archivo
DATA <- read.csv("C:/Users/Usuario/Desktop/Proyecto - AB Test/WA_Marketing-Campaign.csv")

# Vista rápida del dataset
head(DATA)
str(DATA)
View(DATA)


# ============================================================
# 3. CONSTRUCCIÓN DEL DATASET AGREGADO
# ============================================================
# Se agregan las ventas por tienda (LocationID) y promoción,
# calculando el promedio de ventas para evitar dependencia temporal.

DATA_V2 <- DATA %>%
  group_by(LocationID, Promotion, MarketSize, AgeOfStore) %>%
  summarise(
    Avg_Sales = mean(SalesInThousands),
    .groups = "drop"
  )

# Verificación de tamaños
nrow(DATA)      # Dataset original
nrow(DATA_V2)   # Dataset agregado


# ============================================================
# 4. CONTROL DE CALIDAD DE DATOS
# ============================================================

# Resumen estadístico básico
summary(DATA_V2$Avg_Sales)

# Verificación de valores faltantes
any(is.na(DATA_V2$Avg_Sales))


# ============================================================
# 5. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# ============================================================

# ------------------------------------------------------------
# 5.1 Estadísticas descriptivas por promoción
# ------------------------------------------------------------

DATA_V2 %>%
  group_by(Promotion) %>%
  summarise(
    Mean   = mean(Avg_Sales),
    Median = median(Avg_Sales),
    SD     = sd(Avg_Sales),
    N      = n()
  )


# ------------------------------------------------------------
# 5.2 Visualización: Media e intervalo estándar
# ------------------------------------------------------------

ggplot(DATA_V2, aes(x = factor(Promotion), y = Avg_Sales)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(
    title = "Ventas Promedio por Promoción",
    x = "Promoción",
    y = "Ventas Promedio (en miles)"
  )


# ------------------------------------------------------------
# 5.3 Boxplot por promoción
# ------------------------------------------------------------

ggplot(DATA_V2, aes(x = factor(Promotion), y = Avg_Sales)) +
  geom_boxplot() +
  labs(
    title = "Distribución de Ventas Promedio por Promoción",
    x = "Promoción",
    y = "Ventas Promedio (en miles)"
  )


# ------------------------------------------------------------
# 5.4 Distribución de densidad
# ------------------------------------------------------------

ggplot(DATA_V2, aes(x = Avg_Sales, fill = factor(Promotion))) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribución de Ventas Promedio por Promoción",
    x = "Ventas Promedio (en miles)",
    fill = "Promoción"
  )


# ============================================================
# 6. VALIDACIÓN DE SUPUESTOS DEL ANOVA
# ============================================================

# ------------------------------------------------------------
# 6.1 Normalidad (Shapiro-Wilk por grupo)
# ------------------------------------------------------------

by(DATA_V2$Avg_Sales, DATA_V2$Promotion, shapiro.test)

# ------------------------------------------------------------
# 6.2 Apoyo visual: QQ-Plot por promoción
# ------------------------------------------------------------

ggplot(DATA_V2, aes(sample = Avg_Sales)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Promotion) +
  labs(title = "QQ-Plot de Ventas Promedio por Promoción")


# ------------------------------------------------------------
# 6.3 Homogeneidad de varianzas (Levene)
# ------------------------------------------------------------

leveneTest(Avg_Sales ~ factor(Promotion), data = DATA_V2)


# ============================================================
# 7. ANÁLISIS DE VARIANZA (ANOVA)
# ============================================================

Anova_Modelo <- aov(Avg_Sales ~ factor(Promotion), data = DATA_V2)
summary(Anova_Modelo)


# ============================================================
# 8. ANÁLISIS POST-HOC (Tukey HSD)
# ============================================================

Tukey_result <- TukeyHSD(Anova_Modelo)
Tukey_result

# Representación gráfica de comparaciones (Rangos)
plot(Tukey_result, las = 1)

# Tabla resumen de medias por grupo / Numero de Datos (Recuento)
model.tables(Anova_Modelo, type = "means")


# ============================================================
# 9. VISUALIZACIONES EJECUTIVAS FINALES
# ============================================================

# ------------------------------------------------------------
# 9.1 Boxplot base R (variabilidad + mediana)
# ------------------------------------------------------------

boxplot(
  Avg_Sales ~ factor(Promotion),
  data = DATA_V2,
  col = c("lightblue", "lightcoral", "lightgreen"),
  main = "Distribución de Ventas Promedio por Promoción",
  xlab = "Promoción",
  ylab = "Ventas promedio (miles)"
)


# ------------------------------------------------------------
# 9.2 Ranking de medias (gráfico de barras)
# ------------------------------------------------------------

# Cálculo de medias
medias <- aggregate(Avg_Sales ~ Promotion, data = DATA_V2, mean)

# Ajuste de escala del eje Y
ylim_range <- c(
  min(medias$Avg_Sales) - 2,
  max(medias$Avg_Sales) + 2
)

# Gráfico
BarPlot <- barplot(
  medias$Avg_Sales,
  names.arg = medias$Promotion,
  ylim = ylim_range,
  col = c("steelblue", "tomato", "seagreen"),
  main = "Ventas Promedio por Tipo de Promoción",
  xlab = "Promoción",
  ylab = "Ventas promedio (miles)"
)

# Etiquetas de valor
text(
  x = BarPlot,
  y = medias$Avg_Sales,
  labels = round(medias$Avg_Sales, 1),
  pos = 3,
  cex = 0.9
)
