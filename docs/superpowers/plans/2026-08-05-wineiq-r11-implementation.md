# WineIQ + Databricks R11 Evidence — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build WineIQ, a Python/Streamlit wine-segmentation web app driven by a scikit-learn KMeans pipeline, plus a companion PySpark/Delta Lake medallion pipeline that runs in Databricks — together forming one honest, end-to-end, verifiable technical artifact usable both as a portfolio piece and as evidence for the R11 project-matrix requirement.

**Architecture:** Databricks (bronze → silver → gold, PySpark + Delta Lake + MLflow) owns data ingestion, quality, and a Spark-native KMeans training run. The Streamlit app owns real-time/business serving via an equivalent scikit-learn `StandardScaler → KMeans` pipeline trained on the same validated data, packaged with Docker and a GitHub Actions CI/CD pipeline.

**Tech Stack:** Python 3.11, pandas, scikit-learn, Streamlit, Plotly, joblib, pytest, ruff, Docker/Docker Compose, GitHub Actions, PySpark + Delta Lake + MLflow (Databricks Community/Free Edition).

**Spec:** [`docs/superpowers/specs/2026-08-05-wineiq-r11-design.md`](../specs/2026-08-05-wineiq-r11-design.md)

## Global Constraints

- Repo location: new folder `/wineiq` at the repository root (sibling to the existing emoji-named portfolio folders) — chosen to avoid emoji/space path breakage in Docker and CI.
- Model: `StandardScaler` → `KMeans(n_clusters=3, n_init=25, random_state=123)`, persisted to `models/wine_kmeans_pipeline.joblib`.
- Cluster naming rule: rank clusters by **mean `Flavanoids`, descending** → highest = "Premium Reserve", middle = "Classic Balance", lowest = "Light & Fresh". Computed once at training time, persisted to `models/cluster_profile.json` — never recomputed per-prediction.
- Commercial recommendation comes from `src/config.py`, never invented by the model: Premium Reserve `$40-60` (restaurantes premium, wine clubs, tiendas especializadas); Classic Balance `$15-25` (supermercados, distribución masiva); Light & Fresh `$8-15` (público joven, bares de vino, eventos).
- Batch CSV validation: if **any** row is invalid, reject the **whole file** with a full list of errors. No partial processing.
- Streamlit app exposed on port 8501; stack starts with `docker compose up --build`.
- CI/CD: lint (ruff) + pytest + Docker build + push to GHCR. The workflow file must live at the **true repository root** — `.github/workflows/ci.yml` — not under `wineiq/`, because GitHub only discovers workflows there.
- **No local Python interpreter exists on this dev machine** (verified: `python`/`python3`/`py` all resolve to nothing real, only Docker is installed). Every test run in this plan goes through Docker, using an image built once in Task 1 and reused for every subsequent `docker run` test command.
- The Databricks notebooks are real code meant to run in the user's own free Databricks workspace. They cannot be executed or unit-tested from this environment — verification there is manual, and that is a deliberate, documented scope boundary, not a shortcut.
- **R11 honesty rule** (non-negotiable, carried over from the spec): this project is documented everywhere as a personal/portfolio project, never disguised as client work. Duration and data volume are reported truthfully (small reference dataset, recently built) — never inflated to imply years of production experience.

## Task Overview

| # | Task | Deliverable |
|---|---|---|
| 1 | Scaffolding, Dockerfile, config | Docker test loop working, `src/config.py` |
| 2 | `train_model.py` | Trained pipeline + cluster profile committed |
| 3 | `predictor.py` — single prediction | Validation + `predict_single` |
| 4 | `predictor.py` — batch prediction | `predict_batch`, reject-whole-file rule |
| 5 | `app.py` — dashboard core | Header, metrics, PCA chart |
| 6 | `app.py` — classification form | Single-wine form tab |
| 7 | `app.py` — batch upload | CSV upload/validate/download tab |
| 8 | `app.py` — model quality tab | Silhouette, variance explained, elbow |
| 9 | Docker Compose + README | Full stack running end-to-end |
| 10 | GitHub Actions CI/CD | Lint + test + build + push to GHCR |
| 11 | Databricks bronze + silver | PySpark ingestion + data-quality notebooks |
| 12 | Databricks gold + MLflow | PySpark KMeans, medallion complete, run guide |

---

### Task 1: Project scaffolding, Docker test loop, and business config

**Files:**
- Create: `wineiq/.gitignore`
- Create: `wineiq/requirements.txt`
- Create: `wineiq/pytest.ini`
- Create: `wineiq/Dockerfile`
- Create: `wineiq/src/__init__.py`
- Create: `wineiq/src/config.py`
- Create: `wineiq/tests/__init__.py`
- Test: `wineiq/tests/test_config.py`
- Create: `wineiq/data/wine-clustering.csv` (copied from the existing R project folder)

**Interfaces:**
- Produces: `config.COLUMN_NAMES: list[str]` (13 chemical columns), `config.RANDOM_STATE = 123`, `config.N_CLUSTERS = 3`, `config.N_INIT = 25`, `config.DATA_PATH = "data/wine-clustering.csv"`, `config.MODEL_PATH = "models/wine_kmeans_pipeline.joblib"`, `config.CLUSTER_PROFILE_PATH = "models/cluster_profile.json"`, `config.CLUSTER_RULES: dict[str, dict]` keyed by `"premium_reserve"`, `"classic_balance"`, `"light_fresh"`, each a dict with `name`, `description`, `price_range`, `channel`.
- Produces: a Docker image tagged `wineiq:dev` that every later task uses to run tests via `docker run --rm -v "$(pwd):/app" -w /app wineiq:dev <command>` (run from inside `wineiq/`).

- [ ] **Step 1: Create the project skeleton and copy the dataset**

```bash
cd "/c/FAZQ/PROYECT/ESTADISTICA/VINO -R"
mkdir -p wineiq/src wineiq/tests wineiq/data wineiq/models wineiq/databricks
touch wineiq/src/__init__.py wineiq/tests/__init__.py
cp "🍷 Wine Segmentation – Inteligencia Artificial para el Mercado Vinícola/DATA/wine-clustering.csv" wineiq/data/wine-clustering.csv
```

- [ ] **Step 2: Write `wineiq/.gitignore`**

```
__pycache__/
*.pyc
.pytest_cache/
.ruff_cache/
.venv/
venv/
*.egg-info/
.DS_Store
```

- [ ] **Step 3: Write `wineiq/requirements.txt`**

```
pandas>=2.2,<3.0
scikit-learn>=1.4,<2.0
streamlit>=1.32,<2.0
plotly>=5.20,<6.0
joblib>=1.3,<2.0
pytest>=8.0,<9.0
ruff>=0.4,<1.0
```

- [ ] **Step 4: Write `wineiq/pytest.ini`**

```ini
[pytest]
pythonpath = .
```

- [ ] **Step 5: Write `wineiq/Dockerfile`**

```dockerfile
FROM python:3.11-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

EXPOSE 8501

HEALTHCHECK --interval=30s --timeout=5s --start-period=15s CMD \
    python -c "import urllib.request as u; u.urlopen('http://localhost:8501/_stcore/health')" || exit 1

CMD ["streamlit", "run", "app.py", "--server.port=8501", "--server.address=0.0.0.0"]
```

Note: `CMD` (not `ENTRYPOINT`) is used deliberately so `docker run wineiq:dev <anything>` cleanly overrides it for test commands.

- [ ] **Step 6: Build the dev image (this is the test loop every later task reuses)**

```bash
cd wineiq
docker build -t wineiq:dev .
```

Expected: image builds successfully (there's no `app.py` yet, but `COPY . .` and `pip install` don't require it — the `CMD` is only invoked when the container actually runs without an override).

- [ ] **Step 7: Write the failing test — `wineiq/tests/test_config.py`**

```python
from src import config


def test_has_thirteen_chemical_columns():
    assert len(config.COLUMN_NAMES) == 13


def test_cluster_rules_cover_three_segments():
    assert set(config.CLUSTER_RULES.keys()) == {
        "premium_reserve",
        "classic_balance",
        "light_fresh",
    }


def test_premium_reserve_price_range():
    assert config.CLUSTER_RULES["premium_reserve"]["price_range"] == "$40-60"


def test_classic_balance_price_range():
    assert config.CLUSTER_RULES["classic_balance"]["price_range"] == "$15-25"


def test_light_fresh_price_range():
    assert config.CLUSTER_RULES["light_fresh"]["price_range"] == "$8-15"


def test_kmeans_hyperparameters_match_spec():
    assert config.N_CLUSTERS == 3
    assert config.N_INIT == 25
    assert config.RANDOM_STATE == 123
```

- [ ] **Step 8: Run the test to verify it fails**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_config.py -v
```

Expected: FAIL — `ImportError: cannot import name 'config' from 'src'` (the module doesn't exist yet).

- [ ] **Step 9: Write `wineiq/src/config.py`**

```python
"""Business rules and constants for WineIQ. No values here are invented by
the model — they are the explicit source of truth for pricing and channel
recommendations."""

COLUMN_NAMES = [
    "Alcohol",
    "Malic_Acid",
    "Ash",
    "Ash_Alcanity",
    "Magnesium",
    "Total_Phenols",
    "Flavanoids",
    "Nonflavanoid_Phenols",
    "Proanthocyanins",
    "Color_Intensity",
    "Hue",
    "OD280",
    "Proline",
]

RANDOM_STATE = 123
N_CLUSTERS = 3
N_INIT = 25

DATA_PATH = "data/wine-clustering.csv"
SAMPLE_BATCH_PATH = "data/sample_batch.csv"
MODEL_PATH = "models/wine_kmeans_pipeline.joblib"
CLUSTER_PROFILE_PATH = "models/cluster_profile.json"

CLUSTER_RULES = {
    "premium_reserve": {
        "name": "Premium Reserve",
        "description": "Vinos robustos de alta gama con cuerpo intenso: mayor alcohol, flavonoides y prolina.",
        "price_range": "$40-60",
        "channel": "Restaurantes premium, wine clubs y tiendas especializadas",
    },
    "classic_balance": {
        "name": "Classic Balance",
        "description": "Vinos equilibrados con perfil intermedio y tradicional.",
        "price_range": "$15-25",
        "channel": "Supermercados y distribución masiva",
    },
    "light_fresh": {
        "name": "Light & Fresh",
        "description": "Vinos ligeros y refrescantes, con menor nivel de flavonoides y mayor acidez.",
        "price_range": "$8-15",
        "channel": "Público joven, bares de vino y eventos",
    },
}
```

- [ ] **Step 10: Run the test to verify it passes**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_config.py -v
```

Expected: PASS — 6 passed.

- [ ] **Step 11: Commit**

```bash
git add wineiq/.gitignore wineiq/requirements.txt wineiq/pytest.ini wineiq/Dockerfile \
  wineiq/src/__init__.py wineiq/src/config.py wineiq/tests/__init__.py \
  wineiq/tests/test_config.py wineiq/data/wine-clustering.csv
git commit -m "wineiq: scaffold project, Docker test loop, business config"
```

---

### Task 2: Train the KMeans segmentation pipeline

**Files:**
- Create: `wineiq/src/train_model.py`
- Test: `wineiq/tests/test_train_model.py`

**Interfaces:**
- Consumes: everything from `src.config` (Task 1).
- Produces: `load_raw_data(path: str = config.DATA_PATH) -> pd.DataFrame`, `build_pipeline() -> sklearn.pipeline.Pipeline` (steps named `"scaler"` and `"kmeans"`), `rank_clusters_to_profiles(labeled_df: pd.DataFrame) -> dict[int, str]` (maps cluster id → one of `"premium_reserve"/"classic_balance"/"light_fresh"`), `train(data_path=config.DATA_PATH, model_path=config.MODEL_PATH, profile_path=config.CLUSTER_PROFILE_PATH) -> dict` returning `{"silhouette": float, "variance_explained": float, "cluster_sizes": dict[str, int]}`. `train()` writes the joblib pipeline and the JSON cluster profile (keyed by stringified cluster id, each value a copy of the matching `config.CLUSTER_RULES` entry plus `"cluster_id": int`).

- [ ] **Step 1: Write the failing tests — `wineiq/tests/test_train_model.py`**

```python
import json
import os

import pandas as pd
import pytest

from src import config
from src.train_model import (
    build_pipeline,
    load_raw_data,
    rank_clusters_to_profiles,
    train,
)


@pytest.fixture
def tmp_paths(tmp_path):
    return str(tmp_path / "wine_kmeans_pipeline.joblib"), str(tmp_path / "cluster_profile.json")


def test_load_raw_data_returns_thirteen_columns_no_duplicates():
    df = load_raw_data(config.DATA_PATH)
    assert list(df.columns) == config.COLUMN_NAMES
    assert df.duplicated().sum() == 0


def test_build_pipeline_has_scaler_and_kmeans_with_spec_hyperparameters():
    pipeline = build_pipeline()
    kmeans = pipeline.named_steps["kmeans"]
    assert kmeans.n_clusters == 3
    assert kmeans.n_init == 25
    assert kmeans.random_state == 123


def test_rank_clusters_to_profiles_orders_by_flavanoids_descending():
    labeled_df = pd.DataFrame(
        {
            "Flavanoids": [3.0, 3.0, 0.8, 0.8, 2.0, 2.0],
            "cluster": [0, 0, 1, 1, 2, 2],
        }
    )
    mapping = rank_clusters_to_profiles(labeled_df)
    assert mapping[0] == "premium_reserve"
    assert mapping[2] == "classic_balance"
    assert mapping[1] == "light_fresh"


def test_train_writes_model_and_profile_with_three_segments(tmp_paths):
    model_path, profile_path = tmp_paths
    expected_rows = len(load_raw_data(config.DATA_PATH))

    metrics = train(data_path=config.DATA_PATH, model_path=model_path, profile_path=profile_path)

    assert os.path.exists(model_path)
    assert os.path.exists(profile_path)
    assert 0.0 <= metrics["silhouette"] <= 1.0
    assert 0.0 <= metrics["variance_explained"] <= 1.0
    assert sum(metrics["cluster_sizes"].values()) == expected_rows

    with open(profile_path, encoding="utf-8") as f:
        profiles = json.load(f)
    names = {p["name"] for p in profiles.values()}
    assert names == {"Premium Reserve", "Classic Balance", "Light & Fresh"}
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_train_model.py -v
```

Expected: FAIL — `ModuleNotFoundError: No module named 'src.train_model'`.

- [ ] **Step 3: Write `wineiq/src/train_model.py`**

```python
"""Train the WineIQ KMeans segmentation pipeline."""
import json
import os

import joblib
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

from src import config


def load_raw_data(path: str = config.DATA_PATH) -> pd.DataFrame:
    df = pd.read_csv(path)
    missing = set(config.COLUMN_NAMES) - set(df.columns)
    if missing:
        raise ValueError(f"Dataset is missing required columns: {sorted(missing)}")
    return df[config.COLUMN_NAMES].drop_duplicates().reset_index(drop=True)


def build_pipeline() -> Pipeline:
    return Pipeline(
        [
            ("scaler", StandardScaler()),
            (
                "kmeans",
                KMeans(
                    n_clusters=config.N_CLUSTERS,
                    n_init=config.N_INIT,
                    random_state=config.RANDOM_STATE,
                ),
            ),
        ]
    )


def rank_clusters_to_profiles(labeled_df: pd.DataFrame) -> dict:
    means = labeled_df.groupby("cluster")["Flavanoids"].mean().sort_values(ascending=False)
    ordered_cluster_ids = means.index.tolist()
    profile_keys = ["premium_reserve", "classic_balance", "light_fresh"]
    return dict(zip(ordered_cluster_ids, profile_keys))


def train(
    data_path: str = config.DATA_PATH,
    model_path: str = config.MODEL_PATH,
    profile_path: str = config.CLUSTER_PROFILE_PATH,
) -> dict:
    df = load_raw_data(data_path)
    pipeline = build_pipeline()
    labels = pipeline.fit_predict(df)

    labeled_df = df.copy()
    labeled_df["cluster"] = labels
    cluster_to_key = rank_clusters_to_profiles(labeled_df)

    cluster_profiles = {}
    cluster_sizes = {}
    for cluster_id, profile_key in cluster_to_key.items():
        profile = dict(config.CLUSTER_RULES[profile_key])
        profile["cluster_id"] = int(cluster_id)
        cluster_profiles[str(cluster_id)] = profile
        cluster_sizes[str(cluster_id)] = int((labels == cluster_id).sum())

    scaled = pipeline.named_steps["scaler"].transform(df)
    silhouette = float(silhouette_score(scaled, labels))
    kmeans = pipeline.named_steps["kmeans"]
    total_variance = float(((scaled - scaled.mean(axis=0)) ** 2).sum())
    variance_explained = float(1 - kmeans.inertia_ / total_variance)

    os.makedirs(os.path.dirname(model_path) or ".", exist_ok=True)
    os.makedirs(os.path.dirname(profile_path) or ".", exist_ok=True)
    joblib.dump(pipeline, model_path)
    with open(profile_path, "w", encoding="utf-8") as f:
        json.dump(cluster_profiles, f, indent=2, ensure_ascii=False)

    return {
        "silhouette": silhouette,
        "variance_explained": variance_explained,
        "cluster_sizes": cluster_sizes,
    }


if __name__ == "__main__":
    print(json.dumps(train(), indent=2))
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_train_model.py -v
```

Expected: PASS — 4 passed.

- [ ] **Step 5: Generate the real, committed model artifacts**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev python -m src.train_model
```

Expected: prints a JSON metrics summary; `models/wine_kmeans_pipeline.joblib` and `models/cluster_profile.json` now exist in `wineiq/models/`.

- [ ] **Step 6: Commit**

```bash
git add wineiq/src/train_model.py wineiq/tests/test_train_model.py wineiq/models/
git commit -m "wineiq: train KMeans pipeline and persist cluster profile"
```

---

### Task 3: Predictor — validation and single-wine classification

**Files:**
- Create: `wineiq/src/predictor.py`
- Test: `wineiq/tests/test_predictor.py`

**Interfaces:**
- Consumes: `config.COLUMN_NAMES`, `config.MODEL_PATH`, `config.CLUSTER_PROFILE_PATH` (Task 1); the real committed model/profile files (Task 2).
- Produces: `class ValidationError(Exception)`, `load_pipeline(path=config.MODEL_PATH)`, `load_cluster_profiles(path=config.CLUSTER_PROFILE_PATH) -> dict`, `validate_columns(df: pd.DataFrame) -> list[str]` (empty list = valid), `predict_single(pipeline, profiles: dict, features: dict) -> dict` returning `{"cluster_id": int, "segment": str, "description": str, "price_range": str, "channel": str}`, raises `ValidationError` on invalid input.

- [ ] **Step 1: Write the failing tests — `wineiq/tests/test_predictor.py`**

```python
import pandas as pd
import pytest

from src import config
from src.predictor import (
    ValidationError,
    load_cluster_profiles,
    load_pipeline,
    predict_single,
    validate_columns,
)

VALID_WINE = {
    "Alcohol": 14.23,
    "Malic_Acid": 1.71,
    "Ash": 2.43,
    "Ash_Alcanity": 15.6,
    "Magnesium": 127,
    "Total_Phenols": 2.8,
    "Flavanoids": 3.06,
    "Nonflavanoid_Phenols": 0.28,
    "Proanthocyanins": 2.29,
    "Color_Intensity": 5.64,
    "Hue": 1.04,
    "OD280": 3.92,
    "Proline": 1065,
}


@pytest.fixture(scope="module")
def pipeline():
    return load_pipeline(config.MODEL_PATH)


@pytest.fixture(scope="module")
def profiles():
    return load_cluster_profiles(config.CLUSTER_PROFILE_PATH)


def test_validate_columns_accepts_complete_numeric_row():
    df = pd.DataFrame([VALID_WINE])
    assert validate_columns(df) == []


def test_validate_columns_reports_missing_column():
    df = pd.DataFrame([{k: v for k, v in VALID_WINE.items() if k != "Proline"}])
    errors = validate_columns(df)
    assert any("Proline" in e for e in errors)


def test_validate_columns_reports_non_numeric_value():
    df = pd.DataFrame([dict(VALID_WINE, Alcohol="catorce")])
    errors = validate_columns(df)
    assert any("Alcohol" in e and "no numéricos" in e for e in errors)


def test_validate_columns_reports_missing_value():
    df = pd.DataFrame([dict(VALID_WINE, Hue=None)])
    errors = validate_columns(df)
    assert any("Hue" in e and "faltantes" in e for e in errors)


def test_predict_single_returns_known_segment(pipeline, profiles):
    result = predict_single(pipeline, profiles, VALID_WINE)
    assert result["segment"] in {"Premium Reserve", "Classic Balance", "Light & Fresh"}
    assert result["price_range"] in {"$40-60", "$15-25", "$8-15"}


def test_predict_single_raises_on_invalid_input(pipeline, profiles):
    bad_wine = dict(VALID_WINE, Alcohol="catorce")
    with pytest.raises(ValidationError):
        predict_single(pipeline, profiles, bad_wine)
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_predictor.py -v
```

Expected: FAIL — `ModuleNotFoundError: No module named 'src.predictor'`.

- [ ] **Step 3: Write `wineiq/src/predictor.py`**

```python
"""Load the trained WineIQ pipeline and classify wines."""
import json

import joblib
import pandas as pd

from src import config


class ValidationError(Exception):
    """Raised when input data does not meet WineIQ's classification requirements."""


def load_pipeline(path: str = config.MODEL_PATH):
    return joblib.load(path)


def load_cluster_profiles(path: str = config.CLUSTER_PROFILE_PATH) -> dict:
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def validate_columns(df: pd.DataFrame) -> list:
    errors = []
    missing = [c for c in config.COLUMN_NAMES if c not in df.columns]
    if missing:
        errors.append(f"Faltan columnas requeridas: {', '.join(missing)}")
        return errors

    for column in config.COLUMN_NAMES:
        series = df[column]
        numeric = pd.to_numeric(series, errors="coerce")
        non_numeric_rows = df.index[numeric.isna() & series.notna()].tolist()
        if non_numeric_rows:
            errors.append(f"Columna '{column}' tiene valores no numéricos en las filas: {non_numeric_rows}")
        missing_rows = df.index[series.isna()].tolist()
        if missing_rows:
            errors.append(f"Columna '{column}' tiene valores faltantes en las filas: {missing_rows}")
    return errors


def _profile_for_cluster(profiles: dict, cluster_id: int) -> dict:
    profile = profiles[str(cluster_id)]
    return {
        "cluster_id": cluster_id,
        "segment": profile["name"],
        "description": profile["description"],
        "price_range": profile["price_range"],
        "channel": profile["channel"],
    }


def predict_single(pipeline, profiles: dict, features: dict) -> dict:
    df = pd.DataFrame([features], columns=config.COLUMN_NAMES)
    errors = validate_columns(df)
    if errors:
        raise ValidationError("; ".join(errors))
    cluster_id = int(pipeline.predict(df)[0])
    return _profile_for_cluster(profiles, cluster_id)
```

- [ ] **Step 4: Run the tests to verify they pass**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_predictor.py -v
```

Expected: PASS — 6 passed.

- [ ] **Step 5: Commit**

```bash
git add wineiq/src/predictor.py wineiq/tests/test_predictor.py
git commit -m "wineiq: add predictor validation and single-wine classification"
```

---

### Task 4: Predictor — batch classification with reject-whole-file validation

**Files:**
- Modify: `wineiq/src/predictor.py` (add `predict_batch`)
- Modify: `wineiq/tests/test_predictor.py` (append batch tests)

**Interfaces:**
- Consumes: `validate_columns`, `ValidationError`, `config.COLUMN_NAMES` (this file, Task 3).
- Produces: `predict_batch(pipeline, profiles: dict, df: pd.DataFrame) -> pd.DataFrame` — returns the input df with four new columns appended: `Cluster` (int), `Segmento` (str), `Precio_Sugerido` (str), `Canal_Sugerido` (str). Raises `ValidationError` (whole-file rejection) if any row is invalid.

- [ ] **Step 1: Append the failing tests to `wineiq/tests/test_predictor.py`**

Add at the end of the file (leave the existing top-level imports untouched — each new test imports `predict_batch` locally):

```python
def test_predict_batch_classifies_every_row_and_adds_columns(pipeline, profiles):
    from src.predictor import predict_batch

    df = pd.DataFrame([VALID_WINE, VALID_WINE])
    result = predict_batch(pipeline, profiles, df)
    assert len(result) == 2
    assert {"Cluster", "Segmento", "Precio_Sugerido", "Canal_Sugerido"} <= set(result.columns)


def test_predict_batch_rejects_whole_file_on_any_invalid_row(pipeline, profiles):
    from src.predictor import predict_batch

    df = pd.DataFrame([VALID_WINE, dict(VALID_WINE, Alcohol="catorce")])
    with pytest.raises(ValidationError):
        predict_batch(pipeline, profiles, df)
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_predictor.py -v -k batch
```

Expected: FAIL — `ImportError: cannot import name 'predict_batch' from 'src.predictor'`.

- [ ] **Step 3: Append `predict_batch` to `wineiq/src/predictor.py`**

```python
def predict_batch(pipeline, profiles: dict, df: pd.DataFrame) -> pd.DataFrame:
    errors = validate_columns(df)
    if errors:
        raise ValidationError("; ".join(errors))

    working = df[config.COLUMN_NAMES].copy()
    cluster_ids = pipeline.predict(working).astype(int)

    result = df.copy()
    result["Cluster"] = cluster_ids
    result["Segmento"] = [profiles[str(c)]["name"] for c in cluster_ids]
    result["Precio_Sugerido"] = [profiles[str(c)]["price_range"] for c in cluster_ids]
    result["Canal_Sugerido"] = [profiles[str(c)]["channel"] for c in cluster_ids]
    return result
```

- [ ] **Step 4: Run the full predictor test suite to verify everything passes**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_predictor.py -v
```

Expected: PASS — 8 passed.

- [ ] **Step 5: Commit**

```bash
git add wineiq/src/predictor.py wineiq/tests/test_predictor.py
git commit -m "wineiq: add batch classification with reject-whole-file validation"
```

---

### Task 5: Streamlit dashboard core — header, metrics, PCA chart

**Files:**
- Create: `wineiq/app.py`
- Test: `wineiq/tests/test_app.py`

**Interfaces:**
- Consumes: `config`, `predictor.load_pipeline`, `predictor.load_cluster_profiles`, `predictor.predict_batch` (Tasks 1–4).
- Produces: `get_pipeline()`, `get_profiles()`, `get_labeled_dataset() -> pd.DataFrame`, `build_pca_figure(labeled_df) -> plotly Figure`, `render_header(labeled_df)`, `main()`. Later tasks modify `main()` to add tabs.

- [ ] **Step 1: Write the failing test — `wineiq/tests/test_app.py`**

```python
from streamlit.testing.v1 import AppTest


def test_app_renders_title_and_three_metrics_without_error():
    at = AppTest.from_file("app.py").run(timeout=30)
    assert not at.exception
    assert at.title[0].value == "🍷 WineIQ"
    assert len(at.metric) == 3
```

- [ ] **Step 2: Run the test to verify it fails**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_app.py -v
```

Expected: FAIL — `FileNotFoundError` / AppTest cannot find `app.py`.

- [ ] **Step 3: Write `wineiq/app.py`**

```python
"""WineIQ — Plataforma de Segmentación Inteligente de Vinos."""
import pandas as pd
import plotly.express as px
import streamlit as st
from sklearn.decomposition import PCA

from src import config
from src.predictor import load_cluster_profiles, load_pipeline, predict_batch

st.set_page_config(page_title="WineIQ", page_icon="🍷", layout="wide")

BURGUNDY = "#7B1E3A"
BURGUNDY_DARK = "#4B0E22"


def inject_style():
    st.markdown(
        f"""
        <style>
        .stApp {{ background-color: #FAF6F3; }}
        h1, h2, h3 {{ color: {BURGUNDY_DARK}; }}
        div[data-testid="stMetric"] {{
            background-color: {BURGUNDY}1A;
            border-left: 4px solid {BURGUNDY};
            padding: 10px;
            border-radius: 4px;
        }}
        </style>
        """,
        unsafe_allow_html=True,
    )


@st.cache_resource
def get_pipeline():
    return load_pipeline(config.MODEL_PATH)


@st.cache_resource
def get_profiles():
    return load_cluster_profiles(config.CLUSTER_PROFILE_PATH)


@st.cache_data
def get_labeled_dataset() -> pd.DataFrame:
    df = pd.read_csv(config.DATA_PATH)[config.COLUMN_NAMES].drop_duplicates().reset_index(drop=True)
    return predict_batch(get_pipeline(), get_profiles(), df)


def build_pca_figure(labeled_df: pd.DataFrame):
    scaler = get_pipeline().named_steps["scaler"]
    scaled = scaler.transform(labeled_df[config.COLUMN_NAMES])
    pca = PCA(n_components=2, random_state=config.RANDOM_STATE)
    coords = pca.fit_transform(scaled)

    plot_df = labeled_df.copy()
    plot_df["PC1"] = coords[:, 0]
    plot_df["PC2"] = coords[:, 1]
    return px.scatter(
        plot_df,
        x="PC1",
        y="PC2",
        color="Segmento",
        color_discrete_sequence=["#7B1E3A", "#C08497", "#E8B4BC"],
        title="Segmentación de vinos (PCA 2D)",
        labels={
            "PC1": f"Componente 1 ({pca.explained_variance_ratio_[0]:.1%})",
            "PC2": f"Componente 2 ({pca.explained_variance_ratio_[1]:.1%})",
        },
    )


def render_header(labeled_df: pd.DataFrame):
    st.title("🍷 WineIQ")
    st.caption("Plataforma de Segmentación Inteligente de Vinos")
    col1, col2, col3 = st.columns(3)
    distribution = labeled_df["Segmento"].value_counts()
    col1.metric("Total de vinos", len(labeled_df))
    col2.metric("Segmentos", labeled_df["Segmento"].nunique())
    col3.metric("Segmento más común", distribution.index[0])
    st.bar_chart(distribution)


def main():
    inject_style()
    labeled_df = get_labeled_dataset()
    render_header(labeled_df)
    st.plotly_chart(build_pca_figure(labeled_df), use_container_width=True)


if __name__ == "__main__":
    main()
```

- [ ] **Step 4: Run the test to verify it passes**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_app.py -v
```

Expected: PASS — 1 passed.

- [ ] **Step 5: Commit**

```bash
git add wineiq/app.py wineiq/tests/test_app.py
git commit -m "wineiq: add Streamlit dashboard core (header, metrics, PCA chart)"
```

---

### Task 6: Single-wine classification form

**Files:**
- Modify: `wineiq/app.py` (add `render_single_form()`, update imports and `main()`)
- Modify: `wineiq/tests/test_app.py` (append form test)

**Interfaces:**
- Consumes: `predictor.predict_single`, `predictor.ValidationError`, `get_pipeline()`, `get_profiles()` (Tasks 3, 5).
- Produces: `render_single_form()`, wired into a new "🍇 Clasificar un vino" tab in `main()`. Each of the 13 number inputs uses `key=f"input_{column}"` so tests can target them directly.

- [ ] **Step 1: Append the failing test to `wineiq/tests/test_app.py`**

```python
VALID_WINE = {
    "Alcohol": 14.23,
    "Malic_Acid": 1.71,
    "Ash": 2.43,
    "Ash_Alcanity": 15.6,
    "Magnesium": 127,
    "Total_Phenols": 2.8,
    "Flavanoids": 3.06,
    "Nonflavanoid_Phenols": 0.28,
    "Proanthocyanins": 2.29,
    "Color_Intensity": 5.64,
    "Hue": 1.04,
    "OD280": 3.92,
    "Proline": 1065,
}


def test_single_form_classifies_a_valid_wine():
    at = AppTest.from_file("app.py").run(timeout=30)
    for column, value in VALID_WINE.items():
        at.number_input(key=f"input_{column}").set_value(value)
    at.button[0].click().run(timeout=30)

    assert not at.exception
    known_segments = {"Premium Reserve", "Classic Balance", "Light & Fresh"}
    assert any(any(seg in s.value for seg in known_segments) for s in at.success)
```

- [ ] **Step 2: Run the test to verify it fails**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_app.py -v -k single_form
```

Expected: FAIL — no `number_input` widgets exist with those keys yet (`ValueError`/`KeyError` from AppTest, or `at.button` is empty).

- [ ] **Step 3: Modify `wineiq/app.py`**

Update the import line to include the predictor pieces this task needs:

```python
from src.predictor import ValidationError, load_cluster_profiles, load_pipeline, predict_batch, predict_single
```

Add this function above `main()`:

```python
def render_single_form():
    st.header("Clasificar un vino individual")
    with st.form("single_wine_form"):
        cols = st.columns(3)
        values = {}
        for i, column in enumerate(config.COLUMN_NAMES):
            values[column] = cols[i % 3].number_input(column, value=0.0, format="%.2f", key=f"input_{column}")
        submitted = st.form_submit_button("Clasificar")

    if not submitted:
        return

    try:
        result = predict_single(get_pipeline(), get_profiles(), values)
    except ValidationError as exc:
        st.error(str(exc))
        return

    st.success(f"Segmento: **{result['segment']}**")
    st.write(result["description"])
    col1, col2 = st.columns(2)
    col1.metric("Precio sugerido", result["price_range"])
    col2.metric("Canal sugerido", result["channel"])
```

Replace `main()` with:

```python
def main():
    inject_style()
    labeled_df = get_labeled_dataset()
    render_header(labeled_df)

    tab_dashboard, tab_single = st.tabs(["📊 Dashboard", "🍇 Clasificar un vino"])
    with tab_dashboard:
        st.plotly_chart(build_pca_figure(labeled_df), use_container_width=True)
    with tab_single:
        render_single_form()
```

- [ ] **Step 4: Run the test to verify it passes**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_app.py -v
```

Expected: PASS — 2 passed.

- [ ] **Step 5: Commit**

```bash
git add wineiq/app.py wineiq/tests/test_app.py
git commit -m "wineiq: add single-wine classification form tab"
```

---

### Task 7: Batch CSV upload, validation, and download

**Files:**
- Modify: `wineiq/app.py` (add `process_uploaded_csv()`, `render_batch_upload()`, update `main()`)
- Modify: `wineiq/src/config.py` (add `SAMPLE_BATCH_PATH` — already added in Task 1; confirm it's present)
- Create: `wineiq/data/sample_batch.csv`
- Modify: `wineiq/tests/test_app.py` (append batch tests)

**Interfaces:**
- Consumes: `predict_batch`, `ValidationError`, `config.SAMPLE_BATCH_PATH` (Tasks 1, 4).
- Produces: `process_uploaded_csv(file_bytes: bytes) -> tuple[pd.DataFrame | None, list[str]]` — a pure function (no Streamlit calls) that Task 7's UI wiring wraps in `st.file_uploader`. Testing targets this function directly instead of simulating a file-upload widget.

- [ ] **Step 1: Generate the sample batch CSV**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev python -c "
import pandas as pd
from src import config
df = pd.read_csv(config.DATA_PATH)[config.COLUMN_NAMES].head(5)
df.to_csv(config.SAMPLE_BATCH_PATH, index=False)
print(df)
"
```

Expected: prints 5 rows; `wineiq/data/sample_batch.csv` now exists.

- [ ] **Step 2: Append the failing tests to `wineiq/tests/test_app.py`**

```python
VALID_WINE_CSV = (
    "Alcohol,Malic_Acid,Ash,Ash_Alcanity,Magnesium,Total_Phenols,Flavanoids,"
    "Nonflavanoid_Phenols,Proanthocyanins,Color_Intensity,Hue,OD280,Proline\n"
    "14.23,1.71,2.43,15.6,127,2.8,3.06,0.28,2.29,5.64,1.04,3.92,1065\n"
    "13.2,1.78,2.14,11.2,100,2.65,2.76,0.26,1.28,4.38,1.05,3.4,1050\n"
)


def test_process_uploaded_csv_classifies_valid_rows():
    from app import process_uploaded_csv

    classified, errors = process_uploaded_csv(VALID_WINE_CSV.encode("utf-8"))
    assert errors == []
    assert len(classified) == 2
    assert "Segmento" in classified.columns


def test_process_uploaded_csv_rejects_file_with_any_invalid_row():
    from app import process_uploaded_csv

    bad_csv = VALID_WINE_CSV + "catorce,1.71,2.43,15.6,127,2.8,3.06,0.28,2.29,5.64,1.04,3.92,1065\n"
    classified, errors = process_uploaded_csv(bad_csv.encode("utf-8"))
    assert classified is None
    assert len(errors) > 0
```

- [ ] **Step 3: Run the tests to verify they fail**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_app.py -v -k process_uploaded_csv
```

Expected: FAIL — `ImportError: cannot import name 'process_uploaded_csv' from 'app'`.

- [ ] **Step 4: Modify `wineiq/app.py`**

Add `import io` to the top of the file, alongside the existing `import pandas as pd`.

Add these functions above `main()`:

```python
def process_uploaded_csv(file_bytes: bytes):
    """Returns (classified_df_or_None, error_messages)."""
    try:
        df = pd.read_csv(io.BytesIO(file_bytes))
    except Exception as exc:
        return None, [f"No se pudo leer el archivo CSV: {exc}"]

    try:
        classified = predict_batch(get_pipeline(), get_profiles(), df)
    except ValidationError as exc:
        return None, str(exc).split("; ")
    return classified, []


def render_batch_upload():
    st.header("Clasificación masiva")
    st.caption("Sube un CSV con las 13 columnas químicas para clasificar varios vinos a la vez.")

    with open(config.SAMPLE_BATCH_PATH, "rb") as f:
        st.download_button("Descargar CSV de ejemplo", f, file_name="sample_batch.csv")

    uploaded = st.file_uploader("CSV de vinos", type="csv", key="batch_uploader")
    if uploaded is None:
        return

    classified, errors = process_uploaded_csv(uploaded.getvalue())
    if errors:
        st.error("El archivo tiene errores y no fue procesado:")
        for error in errors:
            st.write(f"- {error}")
        return

    st.success(f"{len(classified)} vinos clasificados correctamente.")
    st.dataframe(classified)
    st.download_button(
        "Descargar resultados clasificados",
        classified.to_csv(index=False).encode("utf-8"),
        file_name="wine_classified.csv",
        mime="text/csv",
    )
```

Replace `main()` with:

```python
def main():
    inject_style()
    labeled_df = get_labeled_dataset()
    render_header(labeled_df)

    tab_dashboard, tab_single, tab_batch = st.tabs(
        ["📊 Dashboard", "🍇 Clasificar un vino", "📁 Clasificación masiva"]
    )
    with tab_dashboard:
        st.plotly_chart(build_pca_figure(labeled_df), use_container_width=True)
    with tab_single:
        render_single_form()
    with tab_batch:
        render_batch_upload()
```

- [ ] **Step 5: Run the tests to verify they pass**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_app.py -v
```

Expected: PASS — 4 passed.

- [ ] **Step 6: Commit**

```bash
git add wineiq/app.py wineiq/tests/test_app.py wineiq/data/sample_batch.csv
git commit -m "wineiq: add batch CSV upload with reject-whole-file validation"
```

---

### Task 8: Model quality tab

**Files:**
- Modify: `wineiq/app.py` (add `render_model_quality()`, update imports and `main()`)
- Modify: `wineiq/tests/test_app.py` (append quality tab test)

**Interfaces:**
- Consumes: `get_pipeline()`, `config.RANDOM_STATE` (Task 5); `sklearn.metrics.silhouette_score`, `sklearn.cluster.KMeans`.
- Produces: `render_model_quality(labeled_df)`, wired into a fourth "🔬 Sobre el modelo" tab.

- [ ] **Step 1: Append the failing test to `wineiq/tests/test_app.py`**

```python
def test_model_quality_tab_shows_silhouette_metric():
    at = AppTest.from_file("app.py").run(timeout=60)
    assert not at.exception
    metric_labels = [m.label for m in at.metric]
    assert "Silhouette Score" in metric_labels
```

- [ ] **Step 2: Run the test to verify it fails**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_app.py -v -k model_quality
```

Expected: FAIL — `AssertionError` (no metric labeled "Silhouette Score" exists yet).

- [ ] **Step 3: Modify `wineiq/app.py`**

Update the imports at the top of the file to add:

```python
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
```

Add this function above `main()`:

```python
def render_model_quality(labeled_df: pd.DataFrame):
    st.header("Sobre el modelo")
    pipeline = get_pipeline()
    scaled = pipeline.named_steps["scaler"].transform(labeled_df[config.COLUMN_NAMES])
    labels = pipeline.named_steps["kmeans"].labels_

    silhouette = silhouette_score(scaled, labels)
    inertia = pipeline.named_steps["kmeans"].inertia_
    total_variance = ((scaled - scaled.mean(axis=0)) ** 2).sum()
    variance_explained = 1 - inertia / total_variance

    col1, col2 = st.columns(2)
    col1.metric("Silhouette Score", f"{silhouette:.3f}")
    col2.metric("Varianza explicada", f"{variance_explained:.1%}")

    st.subheader("Método del codo")
    k_range = range(2, 8)
    inertias = []
    for k in k_range:
        km = KMeans(n_clusters=k, n_init=10, random_state=config.RANDOM_STATE)
        km.fit(scaled)
        inertias.append(km.inertia_)
    elbow_df = pd.DataFrame({"k": list(k_range), "WSS": inertias})
    st.plotly_chart(
        px.line(elbow_df, x="k", y="WSS", markers=True, title="Método del codo"),
        use_container_width=True,
    )
```

Replace `main()` with:

```python
def main():
    inject_style()
    labeled_df = get_labeled_dataset()
    render_header(labeled_df)

    tab_dashboard, tab_single, tab_batch, tab_quality = st.tabs(
        ["📊 Dashboard", "🍇 Clasificar un vino", "📁 Clasificación masiva", "🔬 Sobre el modelo"]
    )
    with tab_dashboard:
        st.plotly_chart(build_pca_figure(labeled_df), use_container_width=True)
    with tab_single:
        render_single_form()
    with tab_batch:
        render_batch_upload()
    with tab_quality:
        render_model_quality(labeled_df)
```

- [ ] **Step 4: Run the full app test suite to verify everything passes**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/test_app.py -v
```

Expected: PASS — 5 passed.

- [ ] **Step 5: Run the entire test suite one more time before moving to DevOps**

```bash
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests/ -v
```

Expected: PASS — all tests across `test_config.py`, `test_train_model.py`, `test_predictor.py`, `test_app.py`.

- [ ] **Step 6: Commit**

```bash
git add wineiq/app.py wineiq/tests/test_app.py
git commit -m "wineiq: add model quality tab (silhouette, variance explained, elbow)"
```

---

### Task 9: Docker Compose, README, and full-stack verification

**Files:**
- Create: `wineiq/docker-compose.yml`
- Create: `wineiq/README.md`

**Interfaces:**
- Consumes: `wineiq/Dockerfile` (Task 1), the complete `app.py` (Task 8).
- Produces: a running container reachable at `http://localhost:8501`.

- [ ] **Step 1: Write `wineiq/docker-compose.yml`**

```yaml
services:
  wineiq:
    build: .
    ports:
      - "8501:8501"
    volumes:
      - ./data:/app/data
      - ./models:/app/models
    restart: unless-stopped
```

- [ ] **Step 2: Write `wineiq/README.md`**

````markdown
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
````

- [ ] **Step 3: Bring the full stack up**

```bash
docker compose up --build -d
```

Expected: image builds, container starts.

- [ ] **Step 4: Verify container health**

```bash
docker compose ps
```

Expected: the `wineiq` service `STATUS` column shows `healthy` (may take up to ~30s after start for the healthcheck's start period to elapse — re-run the command if it still says `starting`).

- [ ] **Step 5: Manual browser verification**

Open `http://localhost:8501` in a browser and confirm, clicking through all four tabs:
- Dashboard: title, 3 metrics, bar chart, and PCA scatter plot all render.
- Clasificar un vino: fill the form with the values from `VALID_WINE` in the tests, submit, and confirm a segment/price/channel result appears.
- Clasificación masiva: download the sample CSV, re-upload it, confirm classified results appear and the results CSV downloads.
- Sobre el modelo: confirm the Silhouette Score, variance explained, and elbow chart render.

- [ ] **Step 6: Tear down**

```bash
docker compose down
```

- [ ] **Step 7: Commit**

```bash
git add wineiq/docker-compose.yml wineiq/README.md
git commit -m "wineiq: add Docker Compose stack and README"
```

---

### Task 10: GitHub Actions CI/CD

**Files:**
- Create: `.github/workflows/ci.yml` — **at the true repository root**, i.e. `VINO -R/.github/workflows/ci.yml`, *not* `wineiq/.github/...`. GitHub only discovers workflows under the repo root's `.github/workflows/`; nesting it inside `wineiq/` would silently never run.

**Interfaces:**
- Consumes: `wineiq/requirements.txt`, `wineiq/Dockerfile`, `wineiq/tests/` (all prior tasks).

- [ ] **Step 1: Write `.github/workflows/ci.yml`**

```yaml
name: WineIQ CI

on:
  push:
    branches: [main, "feature/**"]
  pull_request:

permissions:
  contents: read
  packages: write

jobs:
  lint-and-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-python@v5
        with:
          python-version: "3.11"
      - name: Install dependencies
        working-directory: wineiq
        run: pip install -r requirements.txt
      - name: Lint
        working-directory: wineiq
        run: ruff check src app.py tests
      - name: Test
        working-directory: wineiq
        run: pytest tests -v

  build-and-push:
    needs: lint-and-test
    runs-on: ubuntu-latest
    if: github.event_name == 'push'
    steps:
      - uses: actions/checkout@v4
      - uses: docker/setup-buildx-action@v3
      - uses: docker/login-action@v3
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}
      - uses: docker/build-push-action@v5
        with:
          context: ./wineiq
          push: true
          tags: |
            ghcr.io/${{ github.repository }}/wineiq:latest
            ghcr.io/${{ github.repository }}/wineiq:${{ github.sha }}
```

- [ ] **Step 2: Validate the YAML syntax locally**

```bash
cd "/c/FAZQ/PROYECT/ESTADISTICA/VINO -R"
docker run --rm -v "$(pwd):/repo" -w /repo python:3.11-slim sh -c \
  "pip install -q pyyaml && python -c \"import yaml; yaml.safe_load(open('.github/workflows/ci.yml')); print('valid yaml')\""
```

Expected: prints `valid yaml`.

- [ ] **Step 3: Run the same lint + test commands locally exactly as CI will, to confirm they pass before pushing**

```bash
cd wineiq
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev ruff check src app.py tests
docker run --rm -v "$(pwd):/app" -w /app wineiq:dev pytest tests -v
```

Expected: ruff reports no issues (fix any it finds before continuing); pytest shows all tests passing.

- [ ] **Step 4: Commit**

```bash
cd "/c/FAZQ/PROYECT/ESTADISTICA/VINO -R"
git add .github/workflows/ci.yml
git commit -m "ci: add lint, test, Docker build and GHCR push workflow for wineiq"
```

---

### Task 11: Databricks — bronze and silver notebooks

**Files:**
- Create: `wineiq/databricks/01_bronze_ingest.py`
- Create: `wineiq/databricks/02_silver_clean.py`

**Interfaces:**
- Produces: Delta tables `bronze_wine_raw`, `silver_wine_clean`, `silver_wine_quarantine` when run in a Databricks workspace. Not executable from this environment — see Task 12 for the manual run guide covering all three notebooks together.

- [ ] **Step 1: Write `wineiq/databricks/01_bronze_ingest.py`**

```python
# Databricks notebook source
# MAGIC %md
# MAGIC # 01 - Bronze: Ingesta de datos crudos de vinos
# MAGIC Lee `wine-clustering.csv` (subido a DBFS) y lo persiste como tabla
# MAGIC Delta `bronze_wine_raw`, preservando los datos tal como llegaron más
# MAGIC metadata de ingesta.

# COMMAND ----------

from pyspark.sql import functions as F

RAW_PATH = "dbfs:/FileStore/wineiq/wine-clustering.csv"
BRONZE_TABLE = "bronze_wine_raw"

# COMMAND ----------

df_raw = spark.read.option("header", True).option("inferSchema", True).csv(RAW_PATH)

df_bronze = df_raw.withColumn("_ingested_at", F.current_timestamp()).withColumn(
    "_source_file", F.lit(RAW_PATH)
)

df_bronze.write.format("delta").mode("overwrite").saveAsTable(BRONZE_TABLE)

# COMMAND ----------

display(spark.sql(f"SELECT * FROM {BRONZE_TABLE} LIMIT 10"))
print(f"Filas en {BRONZE_TABLE}: {spark.table(BRONZE_TABLE).count()}")
```

- [ ] **Step 2: Write `wineiq/databricks/02_silver_clean.py`**

```python
# Databricks notebook source
# MAGIC %md
# MAGIC # 02 - Silver: limpieza y validación
# MAGIC Deduplica, tipa y valida las 13 columnas químicas. Las filas
# MAGIC inválidas se separan en `silver_wine_quarantine` con el motivo del
# MAGIC rechazo, en vez de descartarse silenciosamente.

# COMMAND ----------

from pyspark.sql import functions as F
from pyspark.sql.types import DoubleType

BRONZE_TABLE = "bronze_wine_raw"
SILVER_TABLE = "silver_wine_clean"
QUARANTINE_TABLE = "silver_wine_quarantine"

CHEMICAL_COLUMNS = [
    "Alcohol", "Malic_Acid", "Ash", "Ash_Alcanity", "Magnesium",
    "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols",
    "Proanthocyanins", "Color_Intensity", "Hue", "OD280", "Proline",
]

# COMMAND ----------

df = spark.table(BRONZE_TABLE).select(*CHEMICAL_COLUMNS).dropDuplicates()

for column in CHEMICAL_COLUMNS:
    df = df.withColumn(column, F.col(column).cast(DoubleType()))

null_condition = F.lit(False)
for column in CHEMICAL_COLUMNS:
    null_condition = null_condition | F.col(column).isNull()

df_quarantine = df.filter(null_condition).withColumn(
    "_rejection_reason", F.lit("valor nulo o no numérico en una o más columnas")
)
df_clean = df.filter(~null_condition)

# COMMAND ----------

df_clean.write.format("delta").mode("overwrite").saveAsTable(SILVER_TABLE)
df_quarantine.write.format("delta").mode("overwrite").saveAsTable(QUARANTINE_TABLE)

print(f"Filas válidas en {SILVER_TABLE}: {df_clean.count()}")
print(f"Filas en cuarentena en {QUARANTINE_TABLE}: {df_quarantine.count()}")
```

- [ ] **Step 3: Commit**

```bash
git add wineiq/databricks/01_bronze_ingest.py wineiq/databricks/02_silver_clean.py
git commit -m "wineiq: add Databricks bronze and silver notebooks"
```

---

### Task 12: Databricks — gold notebook, MLflow, and the run guide

**Files:**
- Create: `wineiq/databricks/03_gold_features_and_model.py`
- Create: `wineiq/databricks/README.md`

**Interfaces:**
- Consumes: `silver_wine_clean` (Task 11).
- Produces: Delta tables `gold_wine_segments`, `gold_cluster_profiles`, and an MLflow run named `wineiq_gold_kmeans` when run in a Databricks workspace.

- [ ] **Step 1: Write `wineiq/databricks/03_gold_features_and_model.py`**

```python
# Databricks notebook source
# MAGIC %md
# MAGIC # 03 - Gold: features, entrenamiento KMeans y perfiles de negocio
# MAGIC Entrena StandardScaler + KMeans (k=3) con pyspark.ml sobre la capa
# MAGIC Silver, registra el experimento en MLflow, y mapea cada cluster a un
# MAGIC nombre comercial según su Flavanoids promedio (mismo criterio que la
# MAGIC app WineIQ), dejando las tablas gold_wine_segments y
# MAGIC gold_cluster_profiles listas para consumo de negocio.

# COMMAND ----------

import mlflow
from pyspark.ml import Pipeline
from pyspark.ml.clustering import KMeans
from pyspark.ml.feature import StandardScaler, VectorAssembler
from pyspark.sql import functions as F
from pyspark.sql.window import Window

SILVER_TABLE = "silver_wine_clean"
GOLD_SEGMENTS_TABLE = "gold_wine_segments"
GOLD_PROFILES_TABLE = "gold_cluster_profiles"

CHEMICAL_COLUMNS = [
    "Alcohol", "Malic_Acid", "Ash", "Ash_Alcanity", "Magnesium",
    "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols",
    "Proanthocyanins", "Color_Intensity", "Hue", "OD280", "Proline",
]

# COMMAND ----------

df = spark.table(SILVER_TABLE)

assembler = VectorAssembler(inputCols=CHEMICAL_COLUMNS, outputCol="features_raw")
scaler = StandardScaler(inputCol="features_raw", outputCol="features", withMean=True, withStd=True)
kmeans = KMeans(featuresCol="features", predictionCol="cluster", k=3, seed=123)
pipeline = Pipeline(stages=[assembler, scaler, kmeans])

with mlflow.start_run(run_name="wineiq_gold_kmeans"):
    model = pipeline.fit(df)
    df_clustered = model.transform(df)
    kmeans_model = model.stages[-1]
    mlflow.log_param("k", 3)
    mlflow.log_param("seed", 123)
    mlflow.log_metric("training_cost", kmeans_model.summary.trainingCost)

# COMMAND ----------

flavanoid_rank = (
    df_clustered.groupBy("cluster")
    .agg(F.avg("Flavanoids").alias("avg_flavanoids"))
    .withColumn("rank", F.row_number().over(Window.orderBy(F.col("avg_flavanoids").desc())))
)

profile_names = {1: "Premium Reserve", 2: "Classic Balance", 3: "Light & Fresh"}
profile_prices = {1: "$40-60", 2: "$15-25", 3: "$8-15"}
profile_channels = {
    1: "Restaurantes premium, wine clubs y tiendas especializadas",
    2: "Supermercados y distribución masiva",
    3: "Público joven, bares de vino y eventos",
}

name_map = F.create_map([F.lit(x) for pair in profile_names.items() for x in pair])
price_map = F.create_map([F.lit(x) for pair in profile_prices.items() for x in pair])
channel_map = F.create_map([F.lit(x) for pair in profile_channels.items() for x in pair])

cluster_to_rank = flavanoid_rank.select("cluster", "rank")

df_gold = (
    df_clustered.join(cluster_to_rank, on="cluster")
    .withColumn("Segmento", name_map[F.col("rank")])
    .withColumn("Precio_Sugerido", price_map[F.col("rank")])
    .withColumn("Canal_Sugerido", channel_map[F.col("rank")])
    .drop("features_raw", "features")
)

df_gold.write.format("delta").mode("overwrite").saveAsTable(GOLD_SEGMENTS_TABLE)

df_profiles = flavanoid_rank.withColumn("Segmento", name_map[F.col("rank")]).withColumn(
    "Precio_Sugerido", price_map[F.col("rank")]
)
df_profiles.write.format("delta").mode("overwrite").saveAsTable(GOLD_PROFILES_TABLE)

display(spark.table(GOLD_SEGMENTS_TABLE).limit(10))
```

- [ ] **Step 2: Write `wineiq/databricks/README.md`**

````markdown
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
````

- [ ] **Step 3: Commit**

```bash
git add wineiq/databricks/03_gold_features_and_model.py wineiq/databricks/README.md
git commit -m "wineiq: add Databricks gold notebook (PySpark KMeans + MLflow) and run guide"
```
