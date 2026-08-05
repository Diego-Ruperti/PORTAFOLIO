# 🍷 WineIQ — Plataforma de Segmentación Inteligente de Vinos

Clasifica vinos en 3 segmentos comerciales (Premium Reserve, Classic
Balance, Light & Fresh) a partir de 13 variables físico-químicas, usando un
pipeline `StandardScaler + KMeans` entrenado sobre el dataset público UCI
Wine.

## Quickstart

```bash
docker compose up --build
```

Abre http://localhost:8501.

**Importante:** antes de la primera ejecución hay que generar el modelo
entrenado (ver "Reentrenar el modelo" abajo) si `models/` no trae ya
`wine_kmeans_pipeline.joblib` y `cluster_profile.json`.

## Arquitectura

Este proyecto tiene dos capas:

1. **`databricks/`** — pipeline de ingeniería de datos en PySpark + Delta
   Lake (arquitectura medallion bronze/silver/gold), pensado para correr en
   Databricks Community/Free Edition. Ver `databricks/README.md`.
2. **`app.py` + `src/`** — la app de negocio: un pipeline scikit-learn
   equivalente, entrenado sobre los mismos datos validados, sirviendo
   predicciones en tiempo real desde Streamlit.

## Reentrenar el modelo

```bash
docker build -t wineiq:dev .
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev python -m src.train_model
```

## Tests

```bash
docker build -t wineiq:dev .
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/ -v
```

## Nota de transparencia

Este es un proyecto de portafolio técnico, con un dataset de referencia
(178 registros). No representa años de experiencia profesional ni volumen
de datos de producción — documenta habilidad práctica real y verificable
(código, notebooks ejecutables, tests), no antigüedad laboral.
