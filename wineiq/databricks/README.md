# Cómo ejecutar el pipeline WineIQ en Databricks

1. Crea una cuenta gratuita en https://www.databricks.com/try-databricks
   (Free/Community Edition) si no tienes una.
2. En el workspace, sube `data/wine-clustering.csv` a DBFS: Data > Add data
   > Upload file > destino `dbfs:/FileStore/wineiq/wine-clustering.csv`.
3. Importa los tres notebooks de esta carpeta (Workspace > Import > File),
   conservando el orden 01, 02, 03.
4. Crea un clúster con Databricks Runtime **ML** (incluye MLflow
   preinstalado).
5. Ejecuta, en este orden, `01_bronze_ingest`, `02_silver_clean`,
   `03_gold_features_and_model` (Run All en cada uno).
6. Verifica en Data > Catalog que existan las tablas `bronze_wine_raw`,
   `silver_wine_clean`, `silver_wine_quarantine`, `gold_wine_segments` y
   `gold_cluster_profiles`.
7. Revisa el experimento en el ícono de Experiments (MLflow) para ver el
   run `wineiq_gold_kmeans` con sus métricas.
8. (Opcional, evidencia adicional para R11) Exporta `gold_wine_segments` a
   CSV y compáralo contra la salida de
   `wineiq/models/wine_kmeans_pipeline.joblib` para mostrar consistencia
   entre ambas capas.

## Nota sobre Jobs/Workflows

Si tu workspace no tiene habilitada la sección Workflows (limitación
conocida en algunas cuentas gratuitas), documenta esto tal cual en la
matriz R11 y usa como evidencia de orquestación un notebook maestro con:

```python
%run ./01_bronze_ingest
%run ./02_silver_clean
%run ./03_gold_features_and_model
```

No afirmes uso de Jobs/Workflows si no lo pudiste probar de verdad — la
regla de honestidad del proyecto (ver spec) aplica también aquí.
