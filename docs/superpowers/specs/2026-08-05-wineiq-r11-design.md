# WineIQ — Plataforma de Segmentación Inteligente de Vinos + Evidencia Técnica R11

Fecha: 2026-08-05
Estado: Aprobado por el usuario, pendiente de plan de implementación

## 1. Contexto y motivación

Este proyecto tiene un doble propósito, decidido explícitamente durante el diseño:

1. **Portafolio**: convertir el análisis original de clustering de vinos en R
   (`🍷 Wine Segmentation – Inteligencia Artificial para el Mercado Vinícola/Script_R-Studio_Clustering_Vinos.R`)
   en una aplicación web profesional en Python (WineIQ).
2. **Evidencia técnica verificable para una licitación (RFP, requisito R11)**: el
   equipo de Freddy Zambrano necesita llenar una "matriz de proyectos" de 7
   columnas (cliente/sector, alcance, rol, duración, tecnologías, volumen,
   resultado) para varios candidatos a un rol de especialista Databricks. El
   RFP declara explícitamente (§6.1): *"Se descarta experiencia sin evidencia
   verificable o que sea solo cursos/laboratorios"*, y advierte que la
   entrevista técnica puede profundizar en cualquier fila.

   **Regla de honestidad no negociable**: este proyecto puede usarse como fila
   de la matriz R11 únicamente como *proyecto propio / portafolio técnico*,
   nunca disfrazado de cliente real, y nunca como sustituto de años de
   experiencia. Documenta **habilidad real**, no **antigüedad**. Duración y
   volumen deben reportarse tal cual son (proyecto de portafolio, dataset de
   178 registros) — no se infla nada. Este documento y el código resultante
   son en sí mismos la "evidencia verificable" que pide el RFP.

## 2. Fuente de datos

`wine-clustering.csv` — 178 vinos italianos, 13 variables físico-químicas
(Alcohol, Malic_Acid, Ash, Ash_Alcanity, Magnesium, Total_Phenols,
Flavanoids, Nonflavanoid_Phenols, Proanthocyanins, Color_Intensity, Hue,
OD280, Proline). Ya existe localmente en
`🍷 Wine Segmentation – Inteligencia Artificial para el Mercado Vinícola/DATA/wine-clustering.csv`
y se copiará a `wineiq/data/wine-clustering.csv`.

## 3. Ubicación en el repositorio

Carpeta nueva en la raíz del repo: `/wineiq`. Decidido explícitamente para
evitar que el build de Docker, los paths de GitHub Actions y los imports de
Python hereden el emoji y los espacios del nombre de la carpeta del proyecto R.

## 4. Arquitectura

```
══════════════ INGENIERÍA DE DATOS (Databricks Community/Free Edition) ══════════════

wine-clustering.csv (fuente pública, 178 vinos, 13 variables)
        │
        ▼
🥉 BRONZE  (notebook PySpark)
Ingesta raw → Delta table bronze_wine_raw + metadata de ingesta
        │
        ▼
🥈 SILVER  (notebook PySpark)
Dedupe, tipado, validación de rangos/nulos
→ silver_wine_clean (Delta)
→ silver_wine_quarantine (filas inválidas, con motivo de rechazo)
        │
        ▼
🥇 GOLD  (notebook PySpark ML)
StandardScaler + KMeans (pyspark.ml.clustering, k=3, seed=123)
Ranking de centroides → mapeo a nombre comercial (misma regla de negocio que la app)
MLflow tracking del experimento
→ gold_wine_segments (por vino) / gold_cluster_profiles (por cluster)
        │
        │ orquestado como Job/Workflow bronze → silver → gold
        │ (si la Free Edition no soporta Jobs, se orquesta con notebook
        │  maestro + %run, documentando la limitación)
        │
        │ export del gold layer (CSV)
        ▼
══════════════ SERVICIO / NEGOCIO (local, Docker) ══════════════

src/train_model.py
  entrena pipeline scikit-learn (StandardScaler + KMeans, n_clusters=3,
  n_init=25, random_state=123) sobre los datos YA VALIDADOS que salieron
  de Silver/Gold
  → models/wine_kmeans_pipeline.joblib
  → models/cluster_profile.json (mapeo cluster_id → nombre/descr/precio/canal,
    calculado una sola vez en entrenamiento a partir de los centroides)
        │
        ▼
app.py (Streamlit) — dashboard, formulario individual, CSV masivo
        │
        ▼
Docker / docker-compose (puerto 8501) / CI-CD (build + push a GHCR)
```

**Justificación del split**: Databricks/PySpark/Delta asume la ingeniería de
datos y calidad a escala (lo que R02/R03/R16/R18/R19 piden verificar); el
modelo scikit-learn liviano sirve las predicciones en tiempo real en la app
(patrón real de producción: Spark para batch, modelo liviano para *serving*).
Es el mismo patrón que describe la fila de ejemplo del propio RFP
(migración a Lakehouse bronze/silver/gold → reducción de tiempo de reporting).

**Trazabilidad honesta**: `wineiq/data/wine-clustering.csv` y el
`README.md` del proyecto documentan explícitamente que los datos de
entrenamiento de la app representan lo que salió de la capa Silver de
Databricks, para que la historia de punta a punta sea consistente y
defendible en una entrevista técnica.

## 5. Modelo (capa de servicio)

- Pipeline: `StandardScaler` → `KMeans(n_clusters=3, n_init=25, random_state=123)`.
- Las etiquetas de K-Means son arbitrarias: se mapean a nombres comerciales
  según los centroides (mayor alcohol/flavonoides/prolina → Premium Reserve;
  perfil intermedio → Classic Balance; menor flavonoides/mayor acidez →
  Light & Fresh), calculado una única vez en `train_model.py` y persistido
  en `models/cluster_profile.json` (no se recalcula en cada predicción).
- Recomendación comercial desde configuración explícita en `src/config.py`,
  nunca inventada por el modelo:
  - Premium Reserve: \$40–60, restaurantes premium / wine clubs / tiendas especializadas.
  - Classic Balance: \$15–25, supermercados y distribución masiva.
  - Light & Fresh: \$8–15, público joven, bares de vino y eventos.

## 6. Dashboard Streamlit

1. Página principal con paleta burdeos/vino, título WineIQ.
2. Métricas: total de vinos, número de segmentos, distribución por segmento.
3. Gráfico interactivo PCA (2D) de los clusters con Plotly.
4. Formulario de clasificación individual (13 variables).
5. Resultado: segmento, perfil, precio sugerido, canal.
6. Carga de CSV para clasificación masiva + descarga del CSV clasificado.
7. Validación de columnas/valores faltantes/no numéricos — **si hay
   cualquier fila inválida, se rechaza el archivo completo** con un listado
   claro de errores (decisión explícita: prioriza confiabilidad del
   resultado sobre flexibilidad).
8. Tab adicional "Sobre el modelo": Silhouette Score, % varianza explicada
   (BSS/TSS), gráfico del método del codo — extra decidido explícitamente
   para dar credibilidad estadística, replicando los KPIs ya calculados en
   el script R original.
9. CSV de ejemplo incluido.

## 7. Estructura del proyecto

```
wineiq/
  app.py
  requirements.txt
  Dockerfile
  docker-compose.yml
  README.md
  .gitignore
  data/
    wine-clustering.csv
    sample_batch.csv
  models/
    wine_kmeans_pipeline.joblib
    cluster_profile.json
  src/
    __init__.py
    train_model.py
    predictor.py
    config.py
  tests/
    test_predictor.py
  databricks/
    01_bronze_ingest.py
    02_silver_clean.py
    03_gold_features_and_model.py
    README.md   (pasos para correrlo en Databricks Community/Free Edition)
  .github/workflows/ci.yml
```

## 8. DevOps

- `Dockerfile` para ejecutar la app.
- `docker-compose.yml` exponiendo Streamlit en :8501; arranque con
  `docker compose up --build`.
- GitHub Actions: **pipeline completo** — lint + pytest en cada push/PR,
  build de la imagen Docker, y push a GitHub Container Registry (GHCR) en
  cada release (decisión explícita: CI/CD más completo de las opciones
  presentadas).
- Nota de entorno: esta máquina de desarrollo no tiene Python instalado
  (solo Docker), así que el flujo de desarrollo/pruebas local se apoya en
  contenedores en vez de un entorno virtual local.

## 9. Testing

- `pytest` sobre `src/predictor.py` (carga del pipeline, predicción
  individual, predicción batch, casos de validación: columnas faltantes,
  valores no numéricos, archivo vacío).
- Los notebooks de Databricks no se pueden ejecutar ni verificar desde este
  entorno (requieren una cuenta real de Databricks) — quedan fuera del
  alcance de `pytest` y se verifican manualmente por el usuario en su
  workspace.

## 10. Decisiones registradas (de las rondas de consulta con el usuario)

| Decisión | Elegida |
|---|---|
| Ubicación del proyecto | Carpeta nueva `/wineiq` en la raíz del repo |
| Extra de alcance | Panel de calidad del modelo (Silhouette, varianza explicada, codo) |
| Profundidad de CI/CD | Lint + tests + build Docker + push a GHCR |
| Filas de CSV inválidas en carga masiva | Rechazar el archivo completo con listado de errores |
| Profundidad de adaptación a R11 | Reconstruir con PySpark + Delta Lake (arquitectura medallion) |
| ¿Databricks real? | Sí, Databricks Community/Free Edition (gratuito) |
| Entregable final | Ambos: pipeline Databricks + dashboard Streamlit conectados por los datos |

## 11. Riesgos y fuera de alcance

- **Jobs/Workflows en la capa gratuita de Databricks**: por confirmar al
  crear la cuenta; si no está disponible, se documenta la limitación y se
  orquesta con notebook maestro (`%run`).
- **Escala del dataset**: 178 registros es escala de portafolio, no
  productiva. Se reporta así de forma honesta en la matriz R11, no se
  presenta como volumen de producción.
- **Unity Catalog**: posible extra si la Free Edition lo habilita; no se
  asume ni se diseña como requisito.
- Fuera de alcance: ejecutar o validar los notebooks de Databricks desde
  este entorno (Claude Code no tiene acceso a una cuenta Databricks del
  usuario).
