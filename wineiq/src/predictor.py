"""Load the trained WineIQ pipeline and classify wines."""
import json

import joblib
import pandas as pd

from src import config


class ValidationError(Exception):
    """Raised when input data does not meet WineIQ's classification requirements."""


class WinePredictor:
    """Wraps the trained KMeans pipeline and cluster profiles to classify wines."""

    def __init__(
        self,
        model_path: str = config.MODEL_PATH,
        profile_path: str = config.CLUSTER_PROFILE_PATH,
    ):
        self.pipeline = joblib.load(model_path)
        self.profiles = self._load_profiles(profile_path)

    @staticmethod
    def _load_profiles(path: str) -> dict:
        with open(path, encoding="utf-8") as f:
            return json.load(f)

    @staticmethod
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
                errors.append(
                    f"Columna '{column}' tiene valores no numéricos en las filas: "
                    f"{non_numeric_rows}"
                )
            missing_rows = df.index[series.isna()].tolist()
            if missing_rows:
                errors.append(
                    f"Columna '{column}' tiene valores faltantes en las filas: "
                    f"{missing_rows}"
                )
        return errors

    def _profile_for_cluster(self, cluster_id: int) -> dict:
        profile = self.profiles[str(cluster_id)]
        return {
            "cluster_id": cluster_id,
            "segment": profile["name"],
            "description": profile["description"],
            "price_range": profile["price_range"],
            "channel": profile["channel"],
        }

    def predict_single(self, features: dict) -> dict:
        df = pd.DataFrame([features], columns=config.COLUMN_NAMES)
        errors = self.validate_columns(df)
        if errors:
            raise ValidationError("; ".join(errors))
        cluster_id = int(self.pipeline.predict(df)[0])
        return self._profile_for_cluster(cluster_id)

    def predict_batch(self, df: pd.DataFrame) -> pd.DataFrame:
        errors = self.validate_columns(df)
        if errors:
            raise ValidationError("; ".join(errors))

        working = df[config.COLUMN_NAMES].copy()
        cluster_ids = self.pipeline.predict(working).astype(int)

        result = df.copy()
        result["Cluster"] = cluster_ids
        result["Segmento"] = [self.profiles[str(c)]["name"] for c in cluster_ids]
        result["Precio_Sugerido"] = [self.profiles[str(c)]["price_range"] for c in cluster_ids]
        result["Canal_Sugerido"] = [self.profiles[str(c)]["channel"] for c in cluster_ids]
        return result
