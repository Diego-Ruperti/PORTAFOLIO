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
