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

### Módulos principales (`src/`)

- `config.py` — constantes y reglas de negocio (columnas, hiperparámetros
  del modelo, precios/canales por segmento). Sin lógica, solo datos.
- `train_model.py` → **`WineModelTrainer`** — entrena el pipeline
  `StandardScaler + KMeans` y persiste el modelo (`.joblib`) y el perfil de
  clusters (`.json`). `WineModelTrainer(data_path, model_path,
  profile_path).train()`.
- `predictor.py` → **`WinePredictor`** — carga el modelo entrenado y
  clasifica vinos. `WinePredictor(model_path, profile_path)` expone
  `.predict_single(features)`, `.predict_batch(df)` y el estático
  `.validate_columns(df)`.
- `app.py` cachea una única instancia de `WinePredictor` (`get_predictor()`,
  vía `st.cache_resource`) y la reutiliza en las 4 pestañas del dashboard.

## Comandos rápidos (Makefile)

```bash
make build    # construye la imagen wineiq:dev
make train    # entrena el modelo (genera models/*.joblib y cluster_profile.json)
make test     # corre pytest
make lint     # corre ruff
make retrain  # build + train
make up       # docker compose up --build
make down     # docker compose down
make all      # build + train + test + lint
```

## Reentrenar el modelo (sin `make`)

Usa `make retrain` si tienes `make` instalado (recomendado — funciona igual
en PowerShell, cmd o Git Bash). Si prefieres el comando de Docker directo:

```bash
# Git Bash / WSL / macOS / Linux
docker build -t wineiq:dev .
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev python -m src.train_model
```

```powershell
# PowerShell
docker build -t wineiq:dev .
docker run --rm -v "${PWD}:/app" -w /app wineiq:dev python -m src.train_model
```

## Tests (sin `make`)

`make test` es el atajo recomendado. Equivalente directo:

```bash
# Git Bash / WSL / macOS / Linux
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/ -v
```

```powershell
# PowerShell
docker run --rm -v "${PWD}:/app" -w /app wineiq:dev pytest tests/ -v
```

## Nota de transparencia

Este es un proyecto de portafolio técnico, con un dataset de referencia
(178 registros). No representa años de experiencia profesional ni volumen
de datos de producción — documenta habilidad práctica real y verificable
(código, notebooks ejecutables, tests), no antigüedad laboral.
