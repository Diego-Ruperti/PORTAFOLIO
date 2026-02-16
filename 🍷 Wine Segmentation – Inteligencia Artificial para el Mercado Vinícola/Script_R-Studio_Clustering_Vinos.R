# ==============================================================================
# ANÁLISIS DE CLUSTERING K-MEANS PARA SEGMENTACIÓN DE VINOS
# ==============================================================================
# Autor: Diego Correa
# Fecha: Febrero 2026
# Descripción: 
#   Este script realiza un análisis de clustering no supervisado sobre datos
#   químicos de vinos italianos derivados del repositorio UCI Machine Learning.
#   El dataset contiene 13 variables químicas de vinos de tres cultivares 
#   diferentes de la misma región de Italia. El objetivo es segmentar los vinos
#   en grupos homogéneos para desarrollar estrategias de mercado diferenciadas.
#
# Dataset: Wine Data Set (UCI Machine Learning Repository)
# Fuente: https://archive.ics.uci.edu/ml/datasets/wine
# Variables: Alcohol, Ácido málico, Ceniza, Alcalinidad de cenizas, Magnesio,
#           Fenoles totales, Flavanoides, Fenoles no flavonoides, 
#           Proantocianinas, Intensidad del color, Tono, OD280/OD315, Prolina
# ==============================================================================


# ==============================================================================
# 1. CONFIGURACIÓN INICIAL Y CARGA DE LIBRERÍAS
# ==============================================================================

# 1.1 Limpiar el entorno de trabajo
# ------------------------------------------------------------------------------
rm(list = ls())
cat("\014")

# 1.2 Instalación de librerías (ejecutar solo si no están instaladas)
# ------------------------------------------------------------------------------
# Instalar los paquetes, solo si no los tienes previamente

install.packages("tidyverse")
install.packages("skimr")
install.packages("readxl")
install.packages("cluster")
install.packages("factoextra")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("DataExplorer")

# 1.3 Cargar librerías necesarias
# ------------------------------------------------------------------------------
library(tidyverse)    # Conjunto de librerías para manipulación y visualización
library(skimr)        # Alternativa moderna de summary() con más información
library(readxl)       # Lectura de archivos Excel (si aplica)
library(cluster)      # Algoritmos de clustering y validación
library(factoextra)   # Visualización elegante de análisis multivariado
library(ggplot2)      # Gráficos avanzados
library(dplyr)        # Manipulación de datos
library(corrplot)     # Visualización de matrices de correlación
library(gridExtra)    # Organización de múltiples gráficos
library(DataExplorer) # Generación automática de reportes EDA

# ==============================================================================
# 2. CARGA Y EXPLORACIÓN INICIAL DE DATOS
# ==============================================================================

# 2.1 Importar datos
# ------------------------------------------------------------------------------
# IMPORTANTE: Ajustar la ruta del archivo según la ubicación en tu sistema
wine <- read.csv("Escribir la ruta de la DATA")

# Ejemplo de ruta específica (ajustar según tu caso)
wine <- read.csv("C:/Users/Usuario/Desktop/Proyecto - Clustering/Clustering - Vinos/wine-clustering.csv")


# 2.2 Verificar las primeras filas
# ------------------------------------------------------------------------------
head(wine, 10)

# 2.3 Ver primer vistazo a la estructura: tipos de columnas, clases, etc.
# ------------------------------------------------------------------------------
glimpse(wine)

# 2.4 Vista compacta resumida y amigable con skimr
# ------------------------------------------------------------------------------
skim(wine)

# 2.5 Dimensiones del conjunto de datos
# ------------------------------------------------------------------------------
dim(wine)


# ==============================================================================
# 3. LIMPIEZA Y PREPARACIÓN DE DATOS
# ==============================================================================

# 3.1 Verificación de nombres de columnas
# ------------------------------------------------------------------------------

# Mostrar nombres originales
print(colnames(wine))

# Estandarizar nombres con puntos en lugar de espacios y caracteres especiales
# make.names asegura que los nombres sean válidos en R
names(wine) <- make.names(names(wine))

# Verificación posterior al cambio
print(colnames(wine))


# 3.2 Verificación de duplicados
# ------------------------------------------------------------------------------

# Contar filas duplicadas
num_duplicados <- sum(duplicated(wine))
num_duplicados

# Si existen duplicados, eliminarlos
if(num_duplicados > 0) {
  wine <- wine[!duplicated(wine), ]
  cat("✓ Duplicados eliminados\n")
  cat("Nuevas dimensiones:", nrow(wine), "x", ncol(wine), "\n")
}


# 3.3 Revisión de columnas innecesarias
# ------------------------------------------------------------------------------

# Revisar cantidad de valores únicos por columna
# Esto ayuda a identificar columnas con poca variabilidad
valores_unicos <- sapply(wine, function(x) length(unique(x)))
print(valores_unicos)


# 3.4 Estandarización de tipos (reconfirmación rápida)
# ------------------------------------------------------------------------------

# Validar que todo lo que vamos a usar en clustering sea numérico
str(wine)


# ==============================================================================
# 4. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# ==============================================================================

# 4.1 Generación de reporte automático con DataExplorer
# ------------------------------------------------------------------------------

# Este comando genera un reporte HTML completo con todas las visualizaciones
# y estadísticas del dataset
DataExplorer::create_report(wine)


# 4.2 Transformación de datos para visualización
# ------------------------------------------------------------------------------
# Transformamos el dataset a formato largo (long) para graficar dinámicamente

wine_long <- wine %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")


# 4.3 Boxplots para identificar outliers
# ------------------------------------------------------------------------------

ggplot(wine_long, aes(x = Variable, y = Valor)) +
  geom_boxplot(fill = "tomato", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Detección Visual de Outliers por Variable",
       subtitle = "Boxplots de todas las variables químicas",
       y = "Valor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))


# 4.4 Vista compacta resumida (segunda revisión post-limpieza)
# ------------------------------------------------------------------------------
skim(wine)


# ==============================================================================
# 5. ESCALADO (NORMALIZACIÓN) DE VARIABLES NUMÉRICAS
# ==============================================================================

# La normalización es CRÍTICA en K-means para evitar que variables 
# con mayor escala dominen el cálculo de distancias.

# 5.1 Aplicar escalado (estandarización Z-score)
# ------------------------------------------------------------------------------
# Crear nuevo dataset con solo las variables numéricas escaladas
# La función scale() centra (media=0) y escala (sd=1) cada variable

wine_scaled <- scale(wine)

# Verificar resultado del escalado con skimr
skim(wine_scaled)

# Media de variables normalizadas (debe ser ≈0)
print(round(colMeans(wine_scaled), 4))

# Desviación estándar (debe ser ≈1)
print(round(apply(wine_scaled, 2, sd), 4))


# 5.2 Visualización de datos escalados
# ------------------------------------------------------------------------------

# Convertimos a tibble para graficar con ggplot2
wine_scaled_df <- as_tibble(wine_scaled)

wine_scaled_df %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = Variable, y = Valor)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Variables Estandarizadas para Clustering",
       subtitle = "Todas las variables ahora tienen media=0 y sd=1",
       y = "Valor (escalado)")


# ==============================================================================
# 6. DETERMINACIÓN DEL NÚMERO ÓPTIMO DE CLUSTERS
# ==============================================================================

# 6.1 Método del Codo (Elbow Method)
# ------------------------------------------------------------------------------
# Evalúa la varianza intra-cluster (WSS) para diferentes valores de k
# El "codo" indica el punto donde añadir más clusters aporta rendimientos 
# decrecientes

fviz_nbclust(wine_scaled, kmeans, method = "wss", k.max = 10) +
  geom_vline(xintercept = 3, linetype = 2, color = "red") +
  labs(title = "Método del Codo - Número Óptimo de Clusters",
       subtitle = "Within-Cluster Sum of Squares (WSS)",
       y = "Suma de cuadrados (WSS)",
       x = "Número de Clusters (k)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Codo identificado en k = 3 clusters ✓ 


# 6.2 Método de la Silueta (Silhouette Method)
# ------------------------------------------------------------------------------
# Mide qué tan similar es un objeto a su propio cluster comparado con otros
# Valores cercanos a 1 indican clustering excelente


fviz_nbclust(wine_scaled, kmeans, method = "silhouette", k.max = 10) +
  labs(title = "Método del Silhouette - Calidad de Agrupamiento",
       subtitle = "Average Silhouette Width",
       x = "Número de Clusters (k)",
       y = "Coeficiente de Silueta Promedio") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Coeficiente de silueta óptimo en k = 3 clusters ✓

# 6.3 Conclusión de validación
# ------------------------------------------------------------------------------
#  DECISIÓN: Se utilizarán K = 3 CLUSTERS
#   • Método del Codo: codo pronunciado en k=3
#   • Método de Silueta: valor óptimo en k=3
#   • Consistencia entre ambos métodos


# ==============================================================================
# 7. APLICACIÓN DEL ALGORITMO K-MEANS
# ==============================================================================

# 7.1 Configuración y ejecución de K-means
# ------------------------------------------------------------------------------
# Parámetros:
#   - centers = 3: número de clusters determinado en sección 6
#   - nstart = 25: ejecuta el algoritmo 25 veces con diferentes inicializaciones
#                  y selecciona la mejor solución (reduce sensibilidad a centroides iniciales)
#   - iter.max = 100: máximo de iteraciones permitidas

set.seed(123)  # Para reproducibilidad de resultados

# Aplicamos K-Means con K = 3 clusters
kmeans_model <- kmeans(wine_scaled, centers = 3, nstart = 25)


# 7.2 Resumen del modelo
# ------------------------------------------------------------------------------
print(kmeans_model)


# 7.3 Asignar los clusters al dataset original
# ------------------------------------------------------------------------------

# Creamos un nuevo dataframe combinando datos originales y cluster asignado
wine_clustered <- wine %>%
  mutate(Cluster = as.factor(kmeans_model$cluster))

# Verificamos primeras filas
head(wine_clustered)


# 7.4 Analizar tamaño de cada grupo
# ------------------------------------------------------------------------------
#  DISTRIBUCIÓN DE OBSERVACIONES POR CLUSTER

wine_clustered %>%
  count(Cluster, sort = TRUE)

#  TAMAÑO DE LOS CLUSTERS
cat("Cluster 1:", kmeans_model$size[1], "vinos\n")
cat("Cluster 2:", kmeans_model$size[2], "vinos\n")
cat("Cluster 3:", kmeans_model$size[3], "vinos\n")


# 7.5 Varianza explicada
# ------------------------------------------------------------------------------
#  MÉTRICAS DE CALIDAD DEL CLUSTERING

varianza_explicada <- (kmeans_model$betweenss / kmeans_model$totss) * 100
cat("Varianza entre clusters / Varianza total:", round(varianza_explicada, 2), "%\n")
cat("Interpretación: Un", round(varianza_explicada, 2), "% de la varianza total es explicada por las diferencias entre clusters (mayor es mejor)\n")


# 7.6 Centroide medio de cada cluster (en escala original)
# ------------------------------------------------------------------------------
#  PERFIL PROMEDIO POR CLUSTER (ESCALA ORIGINAL)

# Media real (sin escalar) por cluster
perfil_clusters <- wine_clustered %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop") %>%
  mutate(across(-Cluster, ~round(.x, 2)))

# Ver tabla completa en ventana emergente
View(perfil_clusters)
print(perfil_clusters)


# ==============================================================================
# 8. VISUALIZACIÓN DE CLUSTERS
# ==============================================================================

# 8.1 Visualización de Clusters en 2D con PCA
# ------------------------------------------------------------------------------
# PCA reduce las 13 dimensiones a 2 para facilitar la visualización
# manteniendo la mayor varianza posible

fviz_cluster(kmeans_model, 
             data = wine_scaled,
             geom = c("point"),           # solo puntos, más limpio
             ellipse.type = "convex",     # elipse convexa alrededor de cada grupo
             ellipse.level = 0.95,        # intervalo de confianza 95%
             palette = c("#E41A1C", "#377EB8", "#4DAF4A"),  # colores profesionales
             ggtheme = theme_minimal(),
             main = "Segmentación de Vinos mediante K-Means (k=3)",
             xlab = "Componente Principal 1 (36.2%)",
             ylab = "Componente Principal 2 (19.2%)",
             legend.title = "Cluster",
             shape = 19,
             labelsize = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

#  Nota: Los puntos cercanos tienen características químicas similares\n")
#  Las elipses muestran la dispersión de cada cluster\n")


# 8.2 Panel de comparación múltiple de variables por cluster
# ------------------------------------------------------------------------------

# PANEL 1: Primeras 4 variables (Alcohol y Componentes Básicos)
# ------------------------------------------------------------------------------

p1 <- ggplot(wine_clustered, aes(x = Cluster, y = Alcohol, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Alcohol") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p2 <- ggplot(wine_clustered, aes(x = Cluster, y = Malic_Acid, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Ácido Málico") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p3 <- ggplot(wine_clustered, aes(x = Cluster, y = Ash, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Cenizas") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p4 <- ggplot(wine_clustered, aes(x = Cluster, y = Ash_Alcanity, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Alcalinidad de Cenizas") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Generar Panel 1
panel1 <- grid.arrange(p1, p2, p3, p4, ncol = 2,
                       top = "Panel 1: Alcohol y Componentes Básicos")



# PANEL 2: Siguientes 4 variables (Componentes Fenólicos y Minerales)
# ------------------------------------------------------------------------------

p5 <- ggplot(wine_clustered, aes(x = Cluster, y = Magnesium, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Magnesio") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p6 <- ggplot(wine_clustered, aes(x = Cluster, y = Total_Phenols, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Fenoles Totales") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p7 <- ggplot(wine_clustered, aes(x = Cluster, y = Flavanoids, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Flavonoides") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p8 <- ggplot(wine_clustered, aes(x = Cluster, y = Nonflavanoid_Phenols, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Fenoles No Flavonoides") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Generar Panel 2
panel2 <- grid.arrange(p5, p6, p7, p8, ncol = 2,
                       top = "Panel 2: Componentes Fenólicos y Minerales")



# PANEL 3: Últimas 5 variables (Características Visuales y Prolina)
# ------------------------------------------------------------------------------

p9 <- ggplot(wine_clustered, aes(x = Cluster, y = Proanthocyanins, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Proantocianinas") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

p10 <- ggplot(wine_clustered, aes(x = Cluster, y = Color_Intensity, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Intensidad del Color") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

p11 <- ggplot(wine_clustered, aes(x = Cluster, y = Hue, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Matiz (Hue)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

p12 <- ggplot(wine_clustered, aes(x = Cluster, y = OD280, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "OD280/OD315") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

p13 <- ggplot(wine_clustered, aes(x = Cluster, y = Proline, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + 
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71")) +
  theme_minimal() + 
  labs(title = "Prolina") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

# Gráfico vacío para completar el grid
empty <- ggplot() + theme_void()

# Generar Panel 3 (2x3)
panel3 <- grid.arrange(p9, p10, p11, p12, p13, empty, 
                       ncol = 2, nrow = 3,
                       top = "Panel 3: Características Visuales y Prolina")



# ==============================================================================
# 9. INTERPRETACIÓN DE LOS CLUSTERS + INSIGHTS DE NEGOCIO
# ==============================================================================

# 9.1 Análisis detallado del perfil de cada cluster
# ------------------------------------------------------------------------------

# Crear tabla de perfiles con interpretación
perfil_clusters <- wine_clustered %>%
  group_by(Cluster) %>%
  summarise(
    n_vinos = n(),
    Alcohol_avg = round(mean(Alcohol), 2),
    Flavonoides_avg = round(mean(Flavanoids), 2),
    Intensidad_Color_avg = round(mean(Color_Intensity), 2),
    Prolina_avg = round(mean(Proline), 2),
    Fenoles_Totales_avg = round(mean(Total_Phenols), 2),
    Acidez_Malica_avg = round(mean(Malic_Acid), 2)
  ) %>%
  arrange(desc(Alcohol_avg))

View(perfil_clusters)
print(perfil_clusters)


# 9.2 Naming estratégico de clusters
# ------------------------------------------------------------------------------

# Función para asignar nombres basados en características dominantes
asignar_nombres <- function(data) {
  data %>%
    mutate(
      Nombre_Cluster = case_when(
        Cluster == 1 ~ "Classic Balance",
        Cluster == 2 ~ "Premium Reserve", 
        Cluster == 3 ~ "Light & Fresh",
        TRUE ~ "Sin clasificar"
      ),
      Descripcion = case_when(
        Cluster == 1 ~ "Vinos equilibrados con perfil tradicional",
        Cluster == 2 ~ "Vinos robustos de alta gama con cuerpo intenso",
        Cluster == 3 ~ "Vinos ligeros y refrescantes para consumo casual",
        TRUE ~ ""
      )
    )
}

# Aplicar nombres al dataset
wine_final <- wine_clustered %>%
  asignar_nombres()

# Ver resumen con nombres
wine_final %>%
  group_by(Cluster, Nombre_Cluster, Descripcion) %>%
  summarise(
    Cantidad = n(),
    .groups = "drop"
  ) %>%
  print()


# 9.3 Insights clave para el negocio
# ------------------------------------------------------------------------------

# Crear tabla de insights
insights <- data.frame(
  Cluster = c("Cluster 1", "Cluster 2", "Cluster 3"),
  
  Perfil = c(
    "CLASSIC BALANCE - Representa el estándar de la industria",
    "PREMIUM RESERVE - Vinos de alta gama con características superiores",
    "LIGHT & FRESH - Vinos accesibles para nuevos consumidores"
  ),
  
  Caracteristicas_Clave = c(
    "Alcohol moderado (12.2%), flavonoides equilibrados (2.05), color medio",
    "Alcohol alto (13.7%), máximos flavonoides (3.0), color intenso",
    "Alcohol medio (13.1%), mínimos flavonoides (0.82), acidez alta"
  ),
  
  Estrategia_Comercial = c(
    "Posicionar como 'bestseller' confiable. Precio medio, distribución masiva",
    "Marketing premium, canales selectivos, maridajes gourmet",
    "Entrada al mercado, promociones, público joven, consumo verano"
  ),
  
  Precio_Sugerido = c(
    "$15-25 USD",
    "$40-60 USD",
    "$8-15 USD"
  )
)

# Mostrar tabla de insights
print(insights)
View(insights)


# 9.4 KPIs de valor del clustering
# ------------------------------------------------------------------------------

# Calcular métricas de calidad del clustering

# Silhouette Score
sil <- silhouette(kmeans_model$cluster, dist(wine_scaled))
sil_avg <- mean(sil[, 3])

# Within-cluster sum of squares
wss <- kmeans_model$tot.withinss

# Between-cluster sum of squares  
bss <- kmeans_model$betweenss

# Ratio de varianza explicada
var_explicada <- bss / (bss + wss)

# Crear tabla de KPIs
kpis <- data.frame(
  Metrica = c("Silhouette Score", "Varianza Explicada", "Compacidad (WSS)", "Separación (BSS)"),
  Valor = c(
    round(sil_avg, 3),
    paste0(round(var_explicada * 100, 1), "%"),
    round(wss, 0),
    round(bss, 0)
  ),
  Interpretacion = c(
    ifelse(sil_avg > 0.5, "Clustering robusto ✓", "Clustering aceptable"),
    "Porcentaje de variabilidad capturada por los clusters",
    "Menor es mejor - qué tan compactos son los clusters",
    "Mayor es mejor - qué tan separados están los clusters"
  )
)

print(kpis)
View(kpis)


# 9.5 Recomendaciones estratégicas finales
# ------------------------------------------------------------------------------

# Recomendaciones estratégicas detalladas
recomendaciones <- list(
  
  "1. ACCIONES INMEDIATAS" = c(
    "• Reclasificar el portafolio actual según los 3 segmentos identificados",
    "• Subir precio promedio +25% en Cluster 2 (Premium Reserve) - el mercado lo aceptará por su perfil superior",
    "• Reducir inversión publicitaria en Cluster 1 y usarlo como 'entry product' para captar nuevos clientes"
  ),
  
  "2. ESTRATEGIA DE PRODUCTO" = c(
    "• Crear nueva línea 'GRAN RESERVA' exclusivamente con vinos del Cluster 2",
    "• Cluster CLASSIC: Mantener como producto de volumen con margen controlado",
    "• Cluster LIGHT: Explorar producción de espumante o vino joven aprovechando su alta acidez"
  ),
  
  "3. CANALES DE DISTRIBUCIÓN" = c(
    "• PREMIUM (Cluster 2) → Tiendas especializadas, restaurantes alta gama, wine clubs exclusivos",
    "• CLASSIC (Cluster 1) → Supermercados masivos como producto de entrada",
    "• LIGHT (Cluster 3) → Bares de vinos, eventos juveniles, formato espumante para celebraciones"
  ),
  
  "4. ESTRATEGIA COMERCIAL" = c(
    "• Campaña específica de retención y up-selling para clientes actuales del Cluster 2",
    "• Programa de fidelización premium para compradores recurrentes del segmento alto",
    "• Cross-selling: ofrecer Cluster 2 a compradores frecuentes de Cluster 1"
  ),
  
  "5. MÉTRICAS DE SEGUIMIENTO" = c(
    "• Monitorear aceptación del aumento de precio (+25%) en Cluster 2",
    "• Tracking de conversión de Cluster 1 → Cluster 2 (up-selling rate)",
    "• ROI por cluster después de ajuste de inversión publicitaria"
  )
)


# Imprimir recomendaciones estructuradas
for(categoria in names(recomendaciones)) {
  cat(categoria, "\n")
  cat(paste(recomendaciones[[categoria]], collapse = "\n"), "\n\n")
}


# ==============================================================================
# 10. EXPORTACIÓN DE RESULTADOS
# ==============================================================================

# Por defecto se guardan en la carpeta Documentos

# 10.1 Guardar dataset con etiquetas de cluster y nombres
# ------------------------------------------------------------------------------
write.csv(wine_final, "Vinos_Clustered_Final.csv", row.names = FALSE)


# 10.2 Guardar perfiles estadísticos
# ------------------------------------------------------------------------------
write.csv(perfil_clusters, "Perfiles_Clusters.csv", row.names = FALSE)


# 10.3 Guardar insights de negocio
# ------------------------------------------------------------------------------
write.csv(insights, "Insights_Negocio.csv", row.names = FALSE)


# 10.4 Guardar KPIs
# ------------------------------------------------------------------------------
write.csv(kpis, "KPIs_Clustering.csv", row.names = FALSE)



# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================


# ==============================================================================
# NOTAS ADICIONALES Y PRÓXIMOS PASOS
# ==============================================================================

# PRÓXIMOS PASOS SUGERIDOS:
# 1. Validar clusters con métodos alternativos (Hierarchical Clustering, DBSCAN)
# 2. Aplicar técnicas de ensemble clustering para mayor robustez
# 3. Realizar análisis discriminante para identificar variables más importantes
# 4. Implementar modelo predictivo para clasificar nuevos vinos en clusters
# 5. Análisis de rentabilidad por cluster combinando con datos de ventas
# 6. Testear estrategias de marketing diferenciadas por segmento
# 7. Realizar análisis de sensibilidad variando el número de clusters (k=2, k=4)
# 8. Implementar validación cruzada para robustez del modelo

# CONSIDERACIONES TÉCNICAS:
# • K-means asume clusters esféricos - validar con otros métodos si hay dudas
# • La normalización es crítica - nunca aplicar K-means a datos sin escalar
# • nstart=25 reduce sensibilidad a inicialización pero aumenta tiempo de cómputo
# • Para datasets grandes (>10,000 obs), considerar mini-batch K-means
# • Los outliers pueden afectar los centroides - considerar métodos robustos si hay muchos
# • El método del codo es subjetivo - combinar siempre con silhouette para decisión final

# INTERPRETACIÓN DE RESULTADOS:
# • Silhouette > 0.7: Estructura fuerte y bien definida
# • Silhouette 0.5-0.7: Estructura razonable y útil para negocio
# • Silhouette 0.25-0.5: Estructura débil, revisar número de clusters
# • Silhouette < 0.25: Sin estructura sustancial, reconsiderar clustering

# ==============================================================================